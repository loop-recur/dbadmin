module Types where

import Data.Tuple

type StringifiedJSON = String
type HTTPAction = Tuple String String
type URLS = {create :: HTTPAction, schema :: HTTPAction, index :: HTTPAction, nav :: HTTPAction }
