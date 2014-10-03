module Api where

import Ajax
import Control.Monad.Eff
import Debug.Trace
import Data.Maybe
import Data.Either
import Control.Bind
import Control.Monad.Cont.Trans
import Data.Traversable
import Data.Tuple
import Data.JSON((.:), (.:?), decode, FromJSON, parseJSON)
import qualified Data.Map as M

newtype ColumnDetails = ColumnDetails { name::String, kind::String, maxLen:: Maybe Number, nullable::Boolean, position:: Number, updatable:: Boolean, schema:: String, precision:: Maybe Number }

newtype Schema = Schema {pkey :: [String], columns :: [ColumnDetails]}

data Row = Row (M.Map String String)

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

-- Not quite sure how to just make a FromJSON a => (Row a) so...
instance rowFromJSON :: FromJSON Row where
    parseJSON (Data.JSON.JObject o) = (Row <<< M.fromList) <$> (sequence $ fn <$> M.toList o)
      where
        fn (Tuple k (Data.JSON.JString v)) =  Right $ (Tuple k (show v))
        fn (Tuple k (Data.JSON.JNumber v)) =  Right $ (Tuple k (show v))
        fn (Tuple k (Data.JSON.JNull)) =  Right $ (Tuple k "")
        fn (Tuple k (Data.JSON.JBool v)) =  Right $ (Tuple k (show v))
    parseJSON x = Left "You wrong boyyeeee!"

http :: forall r. String -> String -> ContT Unit (EffJqAjax r) String
http method url = ContT $ \res -> jqAjax {method: method, url:url} res

http' :: forall r. Tuple String String -> ContT Unit (EffJqAjax r) String
http' (Tuple method url) = http method url

-- TODO make this just http'
save' (Tuple method url) body = ContT $ \res -> jqAjax {method: "POST", url:url, body: body, contentType: "application/json", processData: false} res

