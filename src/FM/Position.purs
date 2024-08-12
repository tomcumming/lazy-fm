module FM.Position
  ( Rw(..)
  , Cl(..)
  , Pos(..)
  , Positions
  , parse
  , unsafeParse
  , showPositions
  , canPlay
  ) where

import Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)

data Rw = GK | D | DM | M | AM | ST
data Cl = L | C | R

data Pos = Pos Rw Cl

derive instance rwEq :: Eq Rw
derive instance rwOrd :: Ord Rw

derive instance clEq :: Eq Cl
derive instance clOrd :: Ord Cl

derive instance posEq :: Eq Pos
derive instance posOrd :: Ord Pos

instance rwEncodeJson :: J.EncodeJson Rw where
  encodeJson r = J.fromString $ case r of
    GK -> "GK"
    D -> "D"
    DM -> "DM"
    M -> "M"
    AM -> "AM"
    ST -> "ST"

-- instance rwDecodeJson :: J.DecodeJson Rw where
--   decodeJson = J.decodeJson >=> case _ of
--     "GK" -> pure GK
--     "D" -> pure D
--     "DM" -> pure DM
--     "M" -> pure M
--     "AM" -> pure AM
--     "ST" -> pure ST
--     s -> J.UnexpectedValue (J.fromString s) # Left

instance clEncodeJson :: J.EncodeJson Cl where
  encodeJson c = J.fromString $ case c of
    L -> "L"
    C -> "C"
    R -> "R"

-- instance clDecodeJson :: J.DecodeJson Cl where
--   decodeJson = J.decodeJson >=> case _ of
--     "L" -> pure L
--     "C" -> pure C
--     "R" -> pure R
--     s -> J.UnexpectedValue (J.fromString s) # Left

instance posEncode :: J.EncodeJson Pos where
  encodeJson (Pos r c) = J.encodeJson [ J.encodeJson r, J.encodeJson c ]

type Positions = Set.Set Pos

showPositions :: Positions -> String
showPositions = Array.fromFoldable
  >>> map (\(Pos r c) -> Map.singleton r (Set.singleton c))
  >>> foldl (Map.unionWith (<>)) Map.empty
  >>> Map.toUnfoldable
  >>> map showRow
  >>> Array.intersperse ", "
  >>> fold
  where
  showRow :: Tuple Rw (Set.Set Cl) -> String
  showRow = map (foldMap showCol)
    >>> lmap
      ( case _ of
          GK -> "GK"
          D -> "D"
          DM -> "DM"
          M -> "M"
          AM -> "AM"
          ST -> "ST"
      )
    >>> case _ of
      Tuple rs cs -> rs <> " (" <> cs <> ")"

  showCol :: Cl -> String
  showCol = case _ of
    L -> "L"
    C -> "C"
    R -> "R"

parse :: String -> Maybe Positions
parse = split (Pattern ", ") >>> (traverse parseGroup >=> (fold >>> pure))

parseGroup :: String -> Maybe Positions
parseGroup inStr = case split (Pattern " ") inStr of
  [ rStr, cStr ] -> do
    rs <- parseRows rStr
    cs <- parseCols cStr
    let
      ps = do
        r <- rs
        c <- cs
        Pos r c # pure :: (Array Pos)
    Set.fromFoldable ps # pure
  [ rStr ] -> do
    rs <- parseRows rStr
    map (\r -> Pos r C) rs # Set.fromFoldable # pure
  _ -> Nothing

parseRows :: String -> Maybe (Array Rw)
parseRows = split (Pattern "/") >>> traverse parseRow

parseRow :: String -> Maybe Rw
parseRow = case _ of
  "GK" -> Just GK
  "D" -> Just D
  "DM" -> Just DM
  "WB" -> Just DM
  "M" -> Just M
  "AM" -> Just AM
  "ST" -> Just ST
  _ -> Nothing

parseCols :: String -> Maybe (Array Cl)
parseCols = replaceAll (Pattern "(") (Replacement "")
  >>> replaceAll (Pattern ")") (Replacement "")
  >>> toCharArray
  >>> traverse parseCol

parseCol :: Char -> Maybe Cl
parseCol = case _ of
  'L' -> Just L
  'C' -> Just C
  'R' -> Just R
  _ -> Nothing

unsafeParse :: String -> Positions
unsafeParse = parse >>> case _ of
  Nothing -> unsafeThrow "Failed to parse"
  Just ps -> ps

canPlay :: Positions -> Pos -> Boolean
canPlay = flip Set.member
