module FM.ImportData
  ( decodeLeagueStandards
  , decodePlayerData
  , StandardsForPos
  , PlayerData
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (JsonDecodeError(..), decodeJson, fromString, getField) as J
import Data.Argonaut.Decode.Decoders (decodeJArray, decodeJObject) as J
import Data.Either (Either(..))
import Data.Enum (upFromIncluding)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Number as Number
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import FM.Attr (Attr, attrCode)
import FM.Position as Pos
import FM.StandardsGroup as SG
import Foreign.Object as FO

type StandardsForPos = Map.Map Attr Number

type PlayerData =
  { uid :: String
  , name :: String
  , positions :: Pos.Positions
  , attrs :: Map.Map Attr Number
  }

parseAttrRow
  :: Tuple String Json
  -> Either J.JsonDecodeError (Tuple Attr Number)
parseAttrRow (Tuple k v) = Tuple
  <$> J.decodeJson (J.fromString k)
  <*> J.decodeJson v

parseStandardsRow
  :: Tuple String Json
  -> Either J.JsonDecodeError (Tuple SG.StandardsGroup StandardsForPos)
parseStandardsRow (Tuple k parseAttrsObj) = do
  pos <- J.decodeJson (J.fromString k)
  attrs <- J.decodeJObject parseAttrsObj <#> FO.toUnfoldable
    >>= traverse @Array parseAttrRow
    >>= (Map.fromFoldable >>> pure)

  Tuple pos attrs # pure

decodeLeagueStandards
  :: Json
  -> Either J.JsonDecodeError (Map.Map SG.StandardsGroup StandardsForPos)
decodeLeagueStandards = J.decodeJObject >=> \obj ->
  (FO.toUnfoldable obj :: Array _)
    # traverse parseStandardsRow
    # map Map.fromFoldable

decodePlayerRow
  :: Json
  -> Either J.JsonDecodeError PlayerData
decodePlayerRow = J.decodeJObject >=> \obj -> do
  uid <- J.getField obj "UID"
  name <- J.getField obj "Name"

  positions <- J.getField obj "Position"
    >>=
      ( \str -> Pos.parse str
          # maybe (J.UnexpectedValue (J.fromString str) # Left) pure
      )

  attrs <- (upFromIncluding bottom :: Array Attr)
    # traverse
        ( \attr -> J.getField obj (attrCode attr)
            >>=
              ( Number.fromString
                  >>> maybe (J.TypeMismatch "Expected Num" # Left) pure
              )
            <#> Tuple attr
        )
    # map Map.fromFoldable

  pure { uid, name, positions, attrs }

decodePlayerData
  :: Json
  -> Either J.JsonDecodeError (Array PlayerData)
decodePlayerData = J.decodeJArray >=> traverse decodePlayerRow
