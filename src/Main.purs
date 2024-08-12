module Main 
  ( generateReports
  , dataTypes
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError, encodeJson, printJsonDecodeError) as J
import Data.Argonaut.Core (Json)
import Data.Either (Either, either)
import Data.Map as Map
import Effect (Effect)
import Effect.Exception (error, throwException)
import FM.Attr (attrNamesMap)
import FM.ImportData as Import
import FM.Report as Report
import FM.StandardsGroup as SG

parseRawInput :: { leagueStandards :: Json, players :: Json }
  -> Either J.JsonDecodeError {
    stds :: Map.Map SG.StandardsGroup Import.StandardsForPos
    , ps :: Array Import.PlayerData
    }
parseRawInput rawInput = do
  stds <- Import.decodeLeagueStandards rawInput.leagueStandards
  ps <- Import.decodePlayerData rawInput.players
  pure { stds, ps }

generateReports :: { leagueStandards :: Json, players :: Json } -> Effect Json
generateReports rawInput = do
  { stds, ps } <- parseRawInput rawInput
    # either (J.printJsonDecodeError >>> error >>> throwException) pure

  let report = Report.generate stds ps
  J.encodeJson report # pure

dataTypes :: Json
dataTypes = J.encodeJson
  { standardGroupAttrs: SG.standardGroupAttrs
  , standardGroupNames: SG.sgNamesMap
  , attrs: attrNamesMap 
  }
