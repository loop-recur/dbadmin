module Helper where

import Data.JSON
{--import Control.Bind--}
import Types
import Data.Tuple
import Data.JSON
import qualified Data.Map as M
import React
import Control.Monad.Eff
import Control.Apply((<*))

fmap :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
fmap f x = f <$> x

readJValue :: forall a. JValue -> a
readJValue jv = case jv of
  (JString s) -> unsafeCoerce s
  (JNumber n) -> unsafeCoerce n
  (JBool r) -> unsafeCoerce r
  (JArray vs) -> unsafeCoerce $ readJValue <$> vs
  --(JObject vs) -> unsafeCoerce $ M.fromList $ readJValue <$> (M.toList vs)
  JNull -> unsafeCoerce "null"

unpackedJValueMap :: forall a. (M.Map String JValue) -> (M.Map String a)
unpackedJValueMap = M.fromList <<< fmap fixValues <<< M.toList
  where
    fixValues :: forall a. Tuple String JValue -> Tuple String a
    fixValues (Tuple s x) = (Tuple s (readJValue x))


foreign import rawUI
  "function rawUI(str) { \
  \ return window.React.DOM.span({dangerouslySetInnerHTML: {__html: str}}); \
  \ }" :: String -> React.UI


--this should be done in ps
foreign import toRecord
  "function toRecord(tuples) { \
  \ return tuples.reduce(function(acc, x){ acc[x.value0]=x.value1; return acc; }, {}); \
    \ }" :: forall a b. [Tuple String a] -> {|b}

foreign import unsafeCoerce "function unsafeCoerce (a) {return a;}"
    :: forall a b. a -> b


foreign import preventDefault
  "function preventDefault(e) {\
  \  e.preventDefault(); \
  \  return function(){ return e; } \
  \}" :: Event -> forall eff. Eff eff Event

--this should be done in ps
foreign import refsToObj
  "function refsToObj(xs) { \
  \   return JSON.stringify(Object.keys(xs).reduce(function(acc, x){ acc[x] = xs[x].state.value; return acc;}, {})); \
    \}" :: {} -> StringifiedJSON

--this should be done in ps
foreign import refsToObj'
  "function refsToObj$prime(xs) { \
  \  console.log(xs); \
  \  var rec = {}; \
  \  rec[xs.key] = xs.val; \
  \  return JSON.stringify(rec); \
  \ }" :: forall r. r -> StringifiedJSON

foreign import getTarget
  "function getTarget(e){ \
  \  return e.target; \
  \}" :: MouseEvent -> {}

foreign import getTarget'
  "function getTarget$prime(e){ \
  \  return e.target; \
  \}" :: forall a. Event -> a

foreign import innerHtml
  "function innerHtml(x){ \
  \  return x.innerHTML; \
  \}" :: {} -> String
