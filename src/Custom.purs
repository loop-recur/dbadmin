module Custom where

import Api
import Helper
import Types
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM hiding (object)
import Data.Maybe
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import qualified Data.Map as M
import Data.JSON


createCustom :: forall a. ([{|a}] -> String) -> Maybe [Row] -> React.UI
createCustom f = maybe (div' [text "Couldn't create custom"]) html
  where
    html = rawUI <<< f <<< (fmap toJsRec)
    toJsRec (Row x) = toRecord <<< M.toList <<< unpackedJValueMap $ x

widget baseUrl tablename f = ((createCustom f) <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename

