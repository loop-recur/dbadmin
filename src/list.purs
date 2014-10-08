module List where

import Api
import Helper
import Types
import React
import React.DOM
import Data.Maybe
import Data.JSON(decode)
import qualified Data.Map as M

theList :: [React.UI] -> {} -> React.UI
theList trs = mkUI spec do
  return $ table [
      className "list table table-bordered table-striped"
    ] trs

renderListHead:: Row -> React.UI
renderListHead (Row x) = tr' ((th' <<< pure <<< text) <$> M.keys x)

renderListItem :: Row -> React.UI
renderListItem (Row x) = tr' ((td' <<< pure <<< text) <$> (M.values $ unpackedJValueMap x))

createTable :: [Row] -> React.UI
createTable xs = theList ((getTheTopRow xs) : (renderComponents xs)) $ {}
  where
    getTheTopRow (x:xs) = renderListHead x
    renderComponents xs = renderListItem <$> xs

createList :: Maybe [Row] -> React.UI
createList = maybe (div' [text "Couldn't create list"]) createTable

widget baseUrl tablename = (createList <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename
