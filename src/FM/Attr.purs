module FM.Attr (Attr(..), attrName, attrCode, attrNamesMap) where

import Prelude

import Data.Argonaut as J
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum, upFromIncluding)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

data Attr
  -- GK
  = Kic
  | Ref
  | Han
  | Aer
  | Cmd
  | OneV1
  | Thr
  | Com
  | Ecc
  | Pun
  | Tro
  -- Phys
  | Acc
  | Agi
  | Bal
  | Jum
  | Nat
  | Pac
  | Sta
  | Str
  -- Mental
  | Agg
  | Ant
  | Bra
  | Cmp
  | Cnt
  | Dec
  | Det
  | Fla
  | Ldr
  | OtB
  | Pos
  | Tea
  | Vis
  | Wor
  -- Tech
  | Cor
  | Cro
  | Dri
  | Fin
  | Fir
  | Fre
  | Hea
  | Lon
  | LTh
  | Mar
  | Pas
  | Pen
  | Tck
  | Tec

derive instance eqAttr :: Eq Attr
derive instance ordAttr :: Ord Attr
derive instance genericAttr :: Generic Attr _
instance boundedAttr :: Bounded Attr where
  top = genericTop
  bottom = genericBottom

instance enumAttr :: Enum Attr where
  succ = genericSucc
  pred = genericPred

attrCode :: Attr -> String
attrCode = case _ of
  OneV1 -> "1v1"
  LTh -> "L Th"
  Tro -> "TRO"
  attr -> genericShow attr

attrName :: Attr -> String
attrName = case _ of
  -- Keeping
  Kic -> "Kicking"
  Ref -> "Reflexes"
  Han -> "Handling"
  Aer -> "Aerial Reach"
  Cmd -> "Command of Area"
  OneV1 -> "One on Ones"
  Thr -> "Throwing"
  Com -> "Communication"
  Ecc -> "Eccentricity"
  Pun -> "Punching"
  Tro -> "Rushing out"

  -- Physical
  Acc -> "Acceleration"
  Agi -> "Agility"
  Bal -> "Balance"
  Jum -> "Jumping Reach"
  Nat -> "Fitness"
  Pac -> "Pace"
  Sta -> "Stamina"
  Str -> "Strength"

  -- Mental
  Agg -> "Aggression"
  Ant -> "Anticipation"
  Bra -> "Bravery"
  Cmp -> "Composure"
  Cnt -> "Concentration"
  Dec -> "Decisions"
  Det -> "Determination"
  Fla -> "Flair"
  Ldr -> "Leadership"
  OtB -> "Off the Ball"
  Pos -> "Positioning"
  Tea -> "Teamwork"
  Vis -> "Vision"
  Wor -> "Work Rate"

  -- Technical
  Cor -> "Corners"
  Cro -> "Crossing"
  Dri -> "Dribbling"
  Fin -> "Finishing"
  Fir -> "First Touch"
  Fre -> "Free Kicks"
  Hea -> "Heading"
  Lon -> "Long Shots"
  LTh -> "Long Throws"
  Mar -> "Marking"
  Pas -> "Passing"
  Pen -> "Penalties"
  Tck -> "Tackling"
  Tec -> "Technique"

attrCodesMap :: Map.Map String Attr
attrCodesMap = (upFromIncluding bottom :: Array _)
  # map (\attr -> Tuple (attrCode attr) attr)
  # Map.fromFoldable

attrNamesMap :: Map.Map Attr String
attrNamesMap = (upFromIncluding bottom :: Array _)
  # map (\attr -> Tuple attr (attrName attr))
  # Map.fromFoldable

instance attrDecodeJson :: J.DecodeJson Attr where
  decodeJson inVal = J.decodeJson inVal
    >>=
      ( (\code -> Map.lookup code attrCodesMap)
          >>> maybe (J.UnexpectedValue inVal # Left) Right
      )

instance attrEncodeJson :: J.EncodeJson Attr where
  encodeJson = attrCode >>> J.fromString
