module Nav where

import Api
import Helper
import Types
import React
import React.DOM
import Debug.Trace
import Data.Maybe
import Data.JSON(decode)
import qualified Data.Map as M
import Dispatcher

theUl :: [React.UI] -> React.UI
theUl = ul [ className "nav navbar-nav" ]

sendNavEvent disp _ = do
  trigger disp "navClick" "ox"

getLink disp x = a [onClick (sendNavEvent disp), href ("#"++x.name)] [text x.name]

renderListItem :: Dispatch -> Table -> React.UI
renderListItem disp (Table x) = li' [getLink disp x]

createUl :: Dispatch -> DB -> React.UI
createUl disp db = theUl <<< fmap (renderListItem disp) $ db

createNav :: Dispatch -> Maybe DB -> React.UI
createNav disp mdb = maybe (div' [text "Couldn't create nav"]) (createUl disp) mdb

widget baseUrl disp = ((createNav disp) <<< decode) <$> (http' urls.nav) 
  where
    urls = createUrls baseUrl ""
