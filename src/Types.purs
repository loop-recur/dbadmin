module Types where

import Data.Tuple

type StringifiedJSON = String
type HTTPMethod = String
type Url = String
type HTTPAction = Tuple HTTPMethod Url
type URLS = {create :: HTTPAction, schema :: HTTPAction, index :: HTTPAction, nav :: HTTPAction, destroy :: HTTPAction, update :: HTTPAction }

