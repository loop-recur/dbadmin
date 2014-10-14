module Api(
    http
  , http'
  , blankUrls
  , createUrls
  ) where

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
import Network.HTTP

schema :: [String] -> [ColumnDetails] -> Schema
schema pkey columns = Schema {pkey:pkey, columns:columns}

table :: String -> String -> Boolean -> Table
table schma name insertable = Table {schema:schma, name:name, insertable: insertable}

columnDetails :: String -> String -> Maybe Number -> Boolean -> Number -> Boolean -> String -> Maybe Number -> ColumnDetails
columnDetails name kind maxLen nullable position updatable schma precision = ColumnDetails {name:name, kind:kind, maxLen:maxLen, nullable:nullable, position: position, updatable: updatable, schema: schma, precision: precision}

instance tableFromJSON :: FromJSON Table where
  parseJSON (Data.JSON.JObject o) = table <$> (o .: "schema") <*> (o .: "name") <*> (o .: "insertable")
  parseJSON x = Left "not a db"

instance schemaFromJSON :: FromJSON Schema where
  parseJSON (Data.JSON.JObject o) = schema <$> (o .: "pkey") <*> (o .: "columns")
  parseJSON x = Left "this ain't no schema"

instance columnDetailsFromJSON :: FromJSON ColumnDetails where
  parseJSON (Data.JSON.JObject o) = columnDetails <$> (o .: "name") <*>
                                                    (o .: "type") <*>
                                                    (o .:? "maxLen") <*>
                                                    (o .: "nullable") <*>
                                                    (o .: "position") <*>
                                                    (o .: "updatable") <*>
                                                    (o .: "schema") <*>
                                                    (o .:? "precision")
  parseJSON x = Left "not a column"

instance rowFromJSON :: FromJSON Row where
    parseJSON (JObject o) = (Row <<< M.fromList) <$> (sequence $ Right <$> M.toList o)
    parseJSON x = Left "Uncaught js value"

http :: forall r. HTTPAction -> StringifiedJSON -> ContT Unit (EffJqAjax r) String
http (Tuple method url) body = ContT $ \res -> jqAjax {method: method, url:url, body: body, contentType: "application/json", processData: false} res

http' :: forall r. HTTPAction -> ContT Unit (EffJqAjax r) String
http' t = http t ""

blankUrls :: URLS
blankUrls = createUrls "" ""

createUrls :: String -> String -> URLS
createUrls baseurl tablename = { schema: (Tuple OPTIONS url)
                               , index: (Tuple GET url)
                               , create:  (Tuple POST url)
                               , nav: (Tuple GET baseurl)
                               , destroy: (Tuple DELETE idUrl)
                               , update: (Tuple PATCH idUrl)
                               }
  where
    idUrl = url++"?id=eq:{id}"
    url = baseurl++tablename
