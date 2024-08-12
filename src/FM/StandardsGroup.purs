module FM.StandardsGroup
  ( StandardsGroup(..)
  , sgName
  , sgNamesMap
  , standardGroupAttrs
  ) where

import Prelude

import Data.Argonaut as J
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum, upFromIncluding)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import FM.Attr (Attr(..))

data StandardsGroup
  = GK
  | Def
  | Mid
  | Att

derive instance sgGeneric :: Generic StandardsGroup _
derive instance sgEq :: Eq StandardsGroup
derive instance sgOrd :: Ord StandardsGroup

instance sgEnum :: Enum StandardsGroup where
  succ = genericSucc
  pred = genericPred

instance sgBounded :: Bounded StandardsGroup where
  top = genericTop
  bottom = genericBottom

sgCode :: StandardsGroup -> String
sgCode = case _ of
  GK -> "GK"
  Def -> "Def"
  Mid -> "Mid"
  Att -> "Att"

sgName :: StandardsGroup -> String
sgName = case _ of
  GK -> "Goalkeepers"
  Def -> "Defenders"
  Mid -> "Midfielders"
  Att -> "Attackers"

sgCodesMap :: Map.Map String StandardsGroup
sgCodesMap = (upFromIncluding bottom :: Array _)
  # map (\sg -> Tuple (sgCode sg) sg)
  # Map.fromFoldable

sgNamesMap :: Map.Map StandardsGroup String
sgNamesMap = (upFromIncluding bottom :: Array _)
  # map (\sg -> Tuple sg (sgName sg))
  # Map.fromFoldable

standardGroupAttrs :: Map.Map StandardsGroup (Set.Set Attr)
standardGroupAttrs =
  Map.fromFoldable
    [ (Tuple GK [ Kic, Ref, Han, Aer, Cmd, OneV1, Thr, Agi ])
    , (Tuple Def [ Tck, Hea, Jum, Mar, Pos, Str, Pac, Acc ])
    , (Tuple Mid [ Pas, Lon, Vis, Sta, Tea, Tck, Tec, Dec ])
    , (Tuple Att [ Fin, Lon, Hea, Jum, Ant, OtB, Pac, Acc ])
    ] # map Set.fromFoldable

instance sgDecode :: J.DecodeJson StandardsGroup where
  decodeJson json = J.decodeJson json
    >>= (\code -> Map.lookup code sgCodesMap # pure)
    >>= maybe (J.UnexpectedValue json # Left) pure

instance sgEncode :: J.EncodeJson StandardsGroup where
  encodeJson = sgCode >>> J.fromString
