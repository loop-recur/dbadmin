module Custom where

import Api
import Types
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM hiding (object)
import Data.Maybe
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import Control.Bind
import Data.Tuple
import Data.JSON
import qualified Data.Map as M

theCustom :: [React.UI] -> {} -> React.UI
theCustom subviews = mkUI spec do
  return $ div [
      className "custom"
    ] subviews

readJValue :: forall a. JValue -> a
readJValue jv = case jv of
  (JString s) -> unsafeCoerce s
  (JNumber n) -> unsafeCoerce n
  (JBool r) -> unsafeCoerce r
  (JArray vs) -> unsafeCoerce $ readJValue <$> vs
  --(JObject vs) -> unsafeCoerce $ M.fromList $ readJValue <$> (M.toList vs)
  JNull -> unsafeCoerce "null"

fmap :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
fmap f x = f <$> x

--unpackedJValueMap :: forall a. Row -> (M.Map String a)
unpackedJValueMap = M.fromList <<< fmap fixValues <<< M.toList
  where
    fixValues :: forall a. Tuple String JValue -> Tuple String a
    fixValues (Tuple s x) = (Tuple s (readJValue x))

createCustom :: forall a. ({|a} -> String) -> Maybe [Row] -> React.UI
createCustom f mr = maybe (div' [text "Couldn't create custom"]) (renderComponents f) mr
  where
    renderComponents f xs = theCustom ((go f) <$> xs) {}
    go f (Row x) = rawUI<<<f $ toRecord $ M.toList $ unpackedJValueMap x



widget baseUrl tablename f = ((createCustom f) <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename

foreign import rawUI
  "function rawUI(str) { \
  \ return window.React.DOM.div({dangerouslySetInnerHTML: {__html: str}}); \
  \ }" :: String -> React.UI


--this should be done in ps
foreign import toRecord
  "function toRecord(tuples) { \
  \ return tuples.reduce(function(acc, x){ acc[x.value0]=x.value1; return acc; }, {}); \
    \ }" :: forall a b. [Tuple String a] -> {|b}

foreign import unsafeCoerce "function unsafeCoerce (a) {return a;}"
    :: forall a b. a -> b
