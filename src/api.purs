module Api where

import Control.Monad.Eff
import Debug.Trace
import Network.XHR
import Data.Maybe
import Data.Either
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Control.Bind
import Data.Traversable
import Data.Tuple
import Data.Array(head, map)
import Data.JSON((.:), (.:?), decode, FromJSON, parseJSON)
import qualified Data.Map as M

newtype ColumnDetails = ColumnDetails { name::String, kind::String, maxLen:: Maybe Number, nullable::Boolean, position:: Number, updatable:: Boolean, schema:: String, precision:: Maybe Number }

newtype Schema = Schema {pkey :: [String], columns :: [ColumnDetails]}

data Row = Row (M.Map String String)


-- row :: forall a. Tuple String a -> Row
-- row (Tuple String a) = Schema {pkey:pkey, columns:columns}

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
  parseJSON x = Left "not a column"

instance rowFromJSON :: FromJSON Row where
    parseJSON (Data.JSON.JObject o) = (Row <<< M.fromList) <$> (sequence $ fn <$> M.toList o)
      where
        fn (Tuple k (Data.JSON.JString v)) =  Right $ (Tuple k (show v))
        fn (Tuple k (Data.JSON.JNumber v)) =  Right $ (Tuple k (show v))
        fn (Tuple k (Data.JSON.JNull)) =  Right $ (Tuple k "")
        fn (Tuple k (Data.JSON.JBool v)) =  Right $ (Tuple k (show v))
    parseJSON x = Left "You wrong boyyeeee!"

httpGet :: forall r eff. String -> {} -> (String -> EffAjax r eff) -> EffAjax r XHRTask
httpGet path opts cb = get defaultAjaxOptions
      { onReadyStateChange = onSuccess $ \response -> do
          txt <- getResponseText response
          cb txt
          return unit
      } path opts

httpOptions :: forall eff a. String -> (String -> (EffAjax eff a)) -> EffAjax eff XHRTask
httpOptions path cb = ajax defaultAjaxOptions
          { method = "OPTIONS",
            url = path,
            onReadyStateChange = onSuccess $ \response -> do
              txt <- getResponseText response
              cb txt
              return unit
          } {} noBody

httpPost :: forall eff. String -> {} -> EffAjax (trace :: Trace | eff) XHRTask
httpPost path body = ajax defaultAjaxOptions
          { method = "POST",
            url = path,
            onReadyStateChange = onSuccess $ \response -> do
              txt <- getResponseText response
              trace txt
          } {} (urlEncoded body)

