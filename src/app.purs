module Main where

import Api
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM
import Data.Maybe
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Control.Bind
import Data.Traversable
import Data.Tuple
import Data.Array(head)
import Data.JSON(decode, FromJSON)
import qualified Data.Map as M

theList :: [React.UI] -> {} -> React.UI
theList trs = mkUI spec do
  return $ table [
      className "list"
    ] trs

theForm :: [ColumnDetails] -> {} -> React.UI
theForm columns = mkUI spec do
  return $ form [
      className "dbform",
      onSubmit create
    ] ((input [typeProp "Submit"] []) : (getComponent <$> columns))


makeInput :: String -> React.UI
makeInput x = input [placeholder x, name x] []

makeText :: String -> React.UI
makeText x = textarea [placeholder x, name x] []

getComponent :: ColumnDetails -> React.UI
getComponent (ColumnDetails cd) = getCorrectComponent cd.name 
  where
    getCorrectComponent = case cd.kind of
      "character varying" -> makeInput
      "text" -> makeText
      _ -> makeInput

foreign import preventDefault
  "function preventDefault(e) {\
  \  e.preventDefault(); \
  \  return function(){ return e; } \
  \}" :: Event -> forall eff. Eff eff Event

foreign import getTarget
  "function getTarget(e) {\
  \  return function(){ return e.target; } \
  \}" :: Event -> forall eff. Eff eff DOMEventTarget

foreign import serializeForm
  "function serializeForm(f) { \
  \ return function() { \
  \   var o = {}; \
  \   $(f).find(':input').map(function(i, x) {\
  \     if(x.name){ o[x.name] = $(x).val(); } \
  \   }); \
  \   return o; \
  \  }\
  \}" :: DOMEventTarget -> forall eff. Eff eff {}


create e = do
  f <- preventDefault e
  t <- getTarget f
  s <- serializeForm t
  _ <- (httpPost "http://localhost:3000/speakers" s)
  return unit

createForm :: Maybe Schema -> React.UI
createForm = maybe (div' [text "no go"]) renderComponents
  where
    renderComponents (Schema x) = theForm x.columns {}

renderListHead:: Row -> React.UI
renderListHead (Row x) = tr' ((th' <<< pure <<< text) <$> M.keys x)

renderListItem :: Row -> React.UI
renderListItem (Row x) = tr' ((td' <<< pure <<< text) <$> M.values x)

createList :: [Row] -> React.UI
createList xs = theList ((getTheTopRow xs) : (renderComponents xs)) $ {}
  where
    getTheTopRow (x:xs) = renderListHead x
    renderComponents xs = renderListItem <$> xs

megaButt :: Maybe [Row] -> React.UI
megaButt = maybe (div' [text "no go"]) createList

main = do
  (httpGet "http://localhost:3000/speakers") {} (renderToElementById "list" <<< megaButt <<< decode) 
  (httpOptions "http://localhost:3000/speakers") (renderToElementById "create" <<< createForm <<< decode) 

