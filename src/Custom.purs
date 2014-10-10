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

theCustom :: [React.UI] -> {} -> React.UI
theCustom subviews = mkUI spec do
  return $ div [
      className "custom"
    ] subviews


createCustom :: forall a. ({|a} -> String) -> Maybe [Row] -> React.UI
createCustom f mr = maybe (div' [text "Couldn't create custom"]) (renderComponents f) mr
  where
    renderComponents f xs = theCustom ((go f) <$> xs) {}
    go f (Row x) = rawUI<<<f $ toRecord $ M.toList $ unpackedJValueMap x

widget baseUrl tablename f = ((createCustom f) <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename

