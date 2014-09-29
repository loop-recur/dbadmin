module Main where

import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM
import Network.XHR
import Data.Maybe
import Data.Either
import Data.Traversable
import Data.Tuple
import Data.Array(head, map)
import Data.JSON((.:), (.:?), decode, FromJSON, parseJSON)
import qualified Data.Map as M

newtype ColumnDetails = ColumnDetails { name::String, kind::String, maxLen:: Maybe Number, nullable::Boolean, position:: Number, updatable:: Boolean, schema:: String, precision:: Maybe Number }

newtype Schema = Schema {pkey :: [String], columns :: [ColumnDetails]}

schema :: [String] -> [ColumnDetails] -> Schema
schema pkey columns = Schema {pkey:pkey, columns:columns}

columnDetails :: String -> String -> Maybe Number -> Boolean -> Number -> Boolean -> String -> Maybe Number -> ColumnDetails
columnDetails name kind maxLen nullable position updatable schma precision = ColumnDetails {name:name, kind:kind, maxLen:maxLen, nullable:nullable, position: position, updatable: updatable, schema: schma, precision: precision}

instance schemaFromJSON :: FromJSON Schema where
  parseJSON (Data.JSON.JObject o) = do
    pkey <- (o .: "pkey")
    columns <- (o .: "columns")
    Right $ schema pkey columns
  parseJSON x = Left "this ain't no schema"

instance columnDetailsFromJSON :: FromJSON ColumnDetails where
  parseJSON (Data.JSON.JObject o) = do
    name <- (o .: "name")
    kind <- (o .: "type")
    maxLen <- (o .:? "maxLen")
    nullable <- (o .: "nullable")
    position <- (o .: "position")
    updatable <- (o .: "updatable")
    schma <- (o .: "schema")
    precision <- (o .:? "precision")
    Right $ columnDetails name kind maxLen nullable position updatable schma precision

httpGet path opts cb = get defaultAjaxOptions
        { onReadyStateChange = onSuccess $ \response -> do
          txt <- getResponseText response
          cb txt
        } path opts

httpOptions path cb = ajax defaultAjaxOptions
        { method = "OPTIONS",
          url = path,
          onReadyStateChange = onSuccess $ \response -> do
          txt <- getResponseText response
          cb txt
        } {} noBody

getSchema url cb = httpOptions url \text -> do
  cb text
  return unit

makeInput :: String -> {} -> React.UI
makeInput x = mkUI spec do
  return $ input [value x] []

makeText :: String -> {} -> React.UI
makeText x = mkUI spec do
  return $ textarea [value x] []

getComponent :: ColumnDetails -> React.UI
getComponent (ColumnDetails cd) = getCorrectComponent cd.name $ {}
  where
    getCorrectComponent = case cd.kind of
      "character varying" -> makeInput
      "text" -> makeText
      _ -> makeInput

renderComponents :: Schema -> React.UI
renderComponents (Schema x) = div' $ (getComponent <$> x.columns)

createUI :: forall eff. Maybe Schema -> Eff (dom :: React.DOM | eff) React.UI
createUI = renderToBody <<< maybe (div' [text "no go"]) renderComponents

main = do
  getSchema "http://localhost:3000/sessions" (createUI <<< decode)

