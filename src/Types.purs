module Types where

import Data.Tuple
import Data.Maybe
import qualified Data.Map as M
import Data.JSON
import Network.HTTP

type StringifiedJSON = String
type HTTPMethod = Verb
type Url = String
type HTTPAction = Tuple HTTPMethod Url
type URLS = {create :: HTTPAction, schema :: HTTPAction, index :: HTTPAction, nav :: HTTPAction, destroy :: HTTPAction, update :: HTTPAction }

newtype ColumnDetails = ColumnDetails { name::String, kind::String, maxLen:: Maybe Number, nullable::Boolean, position:: Number, updatable:: Boolean, schema:: String, precision:: Maybe Number }

newtype Schema = Schema {pkey :: [String], columns :: [ColumnDetails]}

type TableDesc = {schema :: String, name :: String, insertable :: Boolean}
newtype Table = Table TableDesc
type DB = [Table]

type RowDesc = (M.Map String JValue)
data Row = Row RowDesc

