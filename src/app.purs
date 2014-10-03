module Main where

import Api
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM
import Data.Maybe
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
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
theForm columns = mkUI spec {
    getInitialState = return {}
  } do
    state <- readState
    pure $ div [ className "formbox" ] [ commentForm { columns: columns} ]

commentForm = mkUI spec do
  props <- getProps
  return $ form [
      className "dbform",
      onSubmit create
    ] ((input [typeProp "Submit"] []) : (getComponent <$> props.columns))

makeInput :: String -> React.UI
makeInput x = input [typeProp "text", placeholder x, name x, ref x] []

makeText :: String -> React.UI
makeText x = textarea [placeholder x, name x, ref x] []

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

--this should be done in ps
foreign import refsToObj
  "function refsToObj(xs) { \
  \ return function() { \
  \   return JSON.stringify(Object.keys(xs).reduce(function(acc, x){ acc[x] = xs[x].state.value; return acc;}, {})); \
  \  }\
  \}" ::forall eff. {} -> Eff eff {}

create e = do
  _ <- preventDefault e
  rs <- getRefs
  t <- refsToObj rs
  runContT (save "https://localhost:3000/speakers" t) (\y-> return unit)

createForm :: Maybe Schema -> React.UI
createForm = maybe (div' [text "no go"]) renderComponents
  where
    renderComponents (Schema x) = theForm x.columns {}

renderListHead:: Row -> React.UI
renderListHead (Row x) = tr' ((th' <<< pure <<< text) <$> M.keys x)

renderListItem :: Row -> React.UI
renderListItem (Row x) = tr' ((td' <<< pure <<< text) <$> M.values x)

createTable :: [Row] -> React.UI
createTable xs = theList ((getTheTopRow xs) : (renderComponents xs)) $ {}
  where
    getTheTopRow (x:xs) = renderListHead x
    renderComponents xs = renderListItem <$> xs

createList :: Maybe [Row] -> React.UI
createList = maybe (div' [text "no go"]) createTable

main = do
  let listWidget = (createList <<< decode) <$> (findAll "https://localhost:3000/speakers") 
  runContT listWidget $ \widget -> return unit <* (renderToElementById "list" widget)

  let formWidget = (createForm <<< decode) <$> (getSchema "https://localhost:3000/speakers") 
  runContT formWidget $ \y -> return unit <* (renderToElementById "create" y)

