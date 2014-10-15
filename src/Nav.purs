module Nav (widget) where

import Api
import Helper
import Types
import React
import React.DOM
import Data.Maybe
import Control.Monad.Eff
import Data.JSON(decode)
import qualified Data.Map as M
import Dispatcher

triggerNav :: forall eff. MouseEvent -> Eff eff Unit
triggerNav e = do
  trigger "navClick"<<<innerHtml<<<getTarget $ e
  return unit

navLink :: TableDesc -> {} -> UI
navLink x = mkUI spec do
  return $ a [onClick triggerNav, href "#"] [text x.name]

renderListItem :: Table -> React.UI
renderListItem (Table x) = li' [navLink x {}]

createUl :: DB -> React.UI
createUl = (ul [ className "nav navbar-nav" ]) <<< fmap renderListItem

createNav :: Maybe DB -> React.UI
createNav = maybe (div' [text "Couldn't create nav"]) createUl

widget baseUrl = (createNav <<< decode) <$> (http' urls.nav) 
  where
    urls = createUrls baseUrl ""
