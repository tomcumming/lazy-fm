module FM.Report (generate, Report) where

import Prelude

import Control.Monad.State as State
import Data.Argonaut as J
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Enum (upFromIncluding)
import Data.Foldable (class Foldable, maximumBy)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Ord.Down as Ord
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import FM.Attr (Attr)
import FM.ImportData as Import
import FM.Maths (Normal, mean, weightedMean, zscore, zscorePercentile)
import FM.Position as Pos
import FM.Roles (Role, allRoles, zipRoleAttributes)
import FM.StandardsGroup as SG
import Partial.Unsafe (unsafeCrashWith)

type AttrStandards =
  { others :: Normal
  , position :: Map.Map SG.StandardsGroup Normal
  }

type PlayerRole =
  { canPlay :: Pos.Positions
  , score :: Number
  , attrs :: Map.Map Attr (Tuple Number Number)
  }

type PlayerWithRoles =
  { name :: String
  , roles :: Map.Map String PlayerRole
  }

type PlayerBestPosition =
  { role :: String
  , score :: Number
  }

type PlayerPositions =
  { name :: String
  , positions :: Map.Map Pos.Rw (Map.Map Pos.Cl PlayerBestPosition)
  }

data Report = Report
  { standards :: Map.Map Attr AttrStandards
  , playerRoles :: Array PlayerWithRoles
  , bestPositions :: Array PlayerPositions
  }

generate
  :: Map.Map SG.StandardsGroup Import.StandardsForPos
  -> Array Import.PlayerData
  -> Report
generate inStds inPlayers = Report { standards, playerRoles, bestPositions }
  where
  standards = generateStandards inStds
  playerRoles = generatePlayerRoles standards inPlayers
  bestPositions = generateBestPositions playerRoles

generateStandards
  :: Map.Map SG.StandardsGroup Import.StandardsForPos
  -> Map.Map Attr AttrStandards
generateStandards inStds =
  map
    (\attr -> Tuple attr (calculateAttr attr))
    (upFromIncluding bottom :: Array _)
    # Map.fromFoldable
  where
  crudeStandards :: Map.Map SG.StandardsGroup (Map.Map Attr Normal)
  crudeStandards = inStds
    <#> map (\{ max, avg } -> { mean: avg, stdev: (max - avg) / 2.0 })

  completeGuessOthers = crudeStandards
    # Map.values
    # List.concatMap Map.values
    # averageNormals

  calculateAttr :: Attr -> AttrStandards
  calculateAttr attr =
    if Map.isEmpty positionals then { others: completeGuessOthers, position: Map.empty }
    else if Map.size positionals == 1 then { others: Map.values positionals # averageNormals, position: Map.empty }
    else { others: Map.values positionals # averageNormals, position: positionals }
    where
    positionals = Map.mapMaybe (Map.lookup attr) crudeStandards

generatePlayerRoles
  :: Map.Map Attr AttrStandards
  -> Array Import.PlayerData
  -> Array PlayerWithRoles
generatePlayerRoles attrStandards = map goPlayer
  where
  goPlayer :: Import.PlayerData -> PlayerWithRoles
  goPlayer { name, positions, attrs } =
    { name
    , roles: map (goRole positions attrs) allRoles
        # Map.fromFoldable
    }

  goRole :: Pos.Positions -> Map.Map Attr Number -> Role -> Tuple String PlayerRole
  goRole poss attrs role = Tuple role.name
    { canPlay: Set.filter (Pos.canPlay role.positions) poss
    , score
    , attrs: roleAttrs
    }
    where
    stdsForGroup = attrStandardsForGroup attrStandards role.group
    score = scorePlayerRoleAttributes stdsForGroup roleAttrs # zscorePercentile
    roleAttrs = zipRoleAttributes secondaryWeight attrs role

generateBestPositions
  :: Array PlayerWithRoles
  -> Array PlayerPositions
generateBestPositions = map go
  where
  go :: PlayerWithRoles -> PlayerPositions
  go { name, roles } =
    { name
    , positions:
        [ { row: Pos.GK, cols: [ Pos.C ] }
        , { row: Pos.D, cols: [ Pos.L, Pos.C, Pos.R ] }
        , { row: Pos.DM, cols: [ Pos.L, Pos.C, Pos.R ] }
        , { row: Pos.M, cols: [ Pos.L, Pos.C, Pos.R ] }
        , { row: Pos.AM, cols: [ Pos.L, Pos.C, Pos.R ] }
        , { row: Pos.ST, cols: [ Pos.C ] }
        ]
          # map (goRow roles)
          # Map.fromFoldable
          # Map.filter (Map.isEmpty >>> not)
    }

  goRow
    :: Map.Map String PlayerRole
    -> { row :: Pos.Rw, cols :: Array Pos.Cl }
    -> Tuple Pos.Rw (Map.Map Pos.Cl PlayerBestPosition)
  goRow roles { row, cols } = Tuple row cols'
    where
    cols' = map (Pos.Pos row) cols
      # Array.mapMaybe (goPos roles)
      # Map.fromFoldable

  goPos
    :: Map.Map String PlayerRole
    -> Pos.Pos
    -> Maybe (Tuple Pos.Cl PlayerBestPosition)
  goPos roles pos@(Pos.Pos _ c) = do
    Tuple roleName playerRole <-
      Map.filter
        (_.canPlay >>> Set.member pos)
        roles
        # (Map.toUnfoldable :: _ -> Array _)
        # maximumBy (comparing (Tuple.snd >>> _.score))
    Tuple c { role: roleName, score: playerRole.score } # Just

attrStandardsForGroup
  :: Map.Map Attr AttrStandards
  -> SG.StandardsGroup
  -> Map.Map Attr Normal
attrStandardsForGroup stds sg = map
  ( \{ others, position } -> case Map.lookup sg position of
      Nothing -> others
      Just positional -> positional
  )
  stds

scorePlayerRoleAttributes
  :: Map.Map Attr Normal
  -> Map.Map Attr (Tuple Number Number)
  -> Number
scorePlayerRoleAttributes attrStandards roleAttrs = weightedMean adjustedScores
  where
  weightedScores :: NE.NonEmptyArray (Tuple Number Number)
  weightedScores = Map.intersectionWith go attrStandards roleAttrs
    # NE.fromFoldable
    # case _ of
        Nothing -> unsafeCrashWith "No attributes"
        Just xs -> xs
    # NE.sortWith Tuple.fst

  -- The first score is double the weight of the last, with fast decay
  adjustedScores :: NE.NonEmptyArray (Tuple Number Number)
  adjustedScores = traverse adjustScore weightedScores
    # (\mx -> State.evalState mx 0.0)

  adjustScore :: Tuple Number Number -> State.State Number (Tuple Number Number)
  adjustScore (Tuple s w) = State.state $ \ws -> Tuple
    (Tuple s (w * (1.0 + 1.0 / pow 2.0 ws)))
    (ws + w)

  go :: Normal -> Tuple Number Number -> Tuple Number Number
  go normal (Tuple s w) = Tuple (zscore normal s) w

-- This is a bit silly
averageNormals :: forall f. Foldable f => f Normal -> Normal
averageNormals = NE.fromFoldable
  >>> case _ of
    Nothing -> unsafeCrashWith "No normals"
    Just xs ->
      { mean: map _.mean xs # mean
      , stdev: map _.stdev xs # mean
      }

secondaryWeight :: Number
secondaryWeight = 0.5

instance reportEncode :: J.EncodeJson Report where
  encodeJson (Report report) = J.encodeJson report
