module Api where

import Ajax
import Types
import Control.Monad.Eff
import Debug.Trace
import Data.Maybe
import Data.Either
import Control.Bind
import Control.Monad.Cont.Trans
import Data.Traversable
import Data.Tuple
import Data.JSON
import qualified Data.Map as M

newtype ColumnDetails = ColumnDetails { name::String, kind::String, maxLen:: Maybe Number, nullable::Boolean, position:: Number, updatable:: Boolean, schema:: String, precision:: Maybe Number }

newtype Schema = Schema {pkey :: [String], columns :: [ColumnDetails]}

newtype Table = Table {schema :: String, name :: String, insertable :: Boolean}
type DB = [Table]

data Row = Row (M.Map String JValue)

schema :: [String] -> [ColumnDetails] -> Schema
schema pkey columns = Schema {pkey:pkey, columns:columns}

columnDetails :: String -> String -> Maybe Number -> Boolean -> Number -> Boolean -> String -> Maybe Number -> ColumnDetails
columnDetails name kind maxLen nullable position updatable schma precision = ColumnDetails {name:name, kind:kind, maxLen:maxLen, nullable:nullable, position: position, updatable: updatable, schema: schma, precision: precision}

instance tableFromJSON :: FromJSON Table where
  parseJSON (Data.JSON.JObject o) = do
    schema <- (o .: "schema")
    name <- (o .: "name")
    insertable <- (o .: "insertable")
    Right $ Table {schema: schema, name: name, insertable: insertable}
  parseJSON x = Left "not a db"

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
    parseJSON (JObject o) = (Row <<< M.fromList) <$> (sequence $ fn <$> M.toList o)
      where
        fn t =  Right t
    parseJSON x = Left "Uncaught js value"

http :: forall r. String -> String -> ContT Unit (EffJqAjax r) String
http method url = ContT $ \res -> jqAjax {method: method, url:url} res

http' :: forall r. Tuple String String -> ContT Unit (EffJqAjax r) String
http' (Tuple method url) = http method url

-- TODO make this just http'
save' (Tuple method url) body = ContT $ \res -> jqAjax {method: "POST", url:url, body: body, contentType: "application/json", processData: false} res

-- Let's get READER up in here.
createUrls :: String -> String -> URLS
createUrls baseurl tablename = {schema: (Tuple "OPTIONS" url), index: (Tuple "GET" url), create:  (Tuple "POST" url), nav: (Tuple "GET" baseurl)}
  where
    url = baseurl++tablename
