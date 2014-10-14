module Custom (widget) where

import Api
import Helper
import Types
import React
import React.DOM hiding (object)
import Data.Maybe
import Data.Map(toList)
import Data.JSON(decode)


createCustom :: forall a. ([{|a}] -> String) -> Maybe [Row] -> React.UI
createCustom f = maybe (div' [text "Couldn't create custom"]) html
  where
    html = rawUI <<< f <<< (fmap toJsRec)
    toJsRec (Row x) = toRecord <<< toList <<< unpackedJValueMap $ x

widget baseUrl tablename f = ((createCustom f) <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename

