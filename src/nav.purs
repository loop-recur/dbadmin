module Nav where

import Api
import Helper
import Types
import React
import React.DOM
import Data.Maybe
import Data.JSON(decode)
import qualified Data.Map as M
import qualified Dispatcher as Dispatcher

theUl :: [React.UI] -> React.UI
theUl = ul [ className "nav navbar-nav" ]

getLink x = a [onClick (Dispatcher.trigger x.name), href "#"] [text x.name]

renderListItem :: Table -> React.UI
renderListItem (Table x) = li' [getLink x]

createUl :: DB -> React.UI
createUl = theUl <<< fmap renderListItem

createNav :: Maybe DB -> React.UI
createNav = maybe (div' [text "Couldn't create nav"]) createUl

widget baseUrl = (createNav <<< decode) <$> (http' urls.nav) 
  where
    urls = createUrls baseUrl ""
