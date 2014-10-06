module UI where

import Api
import Types
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM
import Data.Maybe
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import Control.Bind
import Data.Tuple
import Data.Array(elemIndex, filter)
import Data.JSON(decode)
import qualified Data.Map as M

theCustom :: [React.UI] -> {} -> React.UI
theCustom subviews = mkUI spec do
  return $ div [
      className "custom"
    ] subviews

theList :: [React.UI] -> {} -> React.UI
theList trs = mkUI spec do
  return $ table [
      className "list"
    ] trs

theForm :: [ColumnDetails] -> URLS -> {} -> React.UI
theForm columns urls = mkUI spec {
    getInitialState = return {}
  } do
    state <- readState
    pure $ div [ className "formbox" ] [ commentForm { columns: columns, create: (create urls)} ]

commentForm = mkUI spec do
  props <- getProps
  return $ form [
      className "dbform",
      onSubmit props.create
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

create urls e = do
  rs <- refsToObj <$> getRefs <* (preventDefault e)
  runContT (save' urls.create rs) (\y-> return unit)

columnsThatArentPrimaryKeys :: [String] -> [ColumnDetails] -> [ColumnDetails]
columnsThatArentPrimaryKeys pkeys columns = filter notAPkey columns
  where
    notAPkey (ColumnDetails c) = c.name `elemIndex` pkeys < 0

createForm :: URLS -> Maybe Schema -> React.UI
createForm urls ms = maybe (div' [text "Couldn't create form"]) renderComponents ms
  where
    renderComponents (Schema x) = theForm (columnsThatArentPrimaryKeys x.pkey x.columns) urls {}

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
createList = maybe (div' [text "Couldn't create list"]) createTable

createCustom :: forall a. ((M.Map String String) -> String) -> Maybe [Row] -> React.UI
createCustom f mr = maybe (div' [text "Couldn't create custom"]) (renderComponents f) mr
  where
    renderComponents f xs = theCustom ((go f) <$> xs) {}
    go f (Row x) = rawUI<<<f $ x

-- Let's get READER up in here.
createUrls :: String -> String -> URLS
createUrls baseurl tablename = {schema: (Tuple "OPTIONS" url), index: (Tuple "GET" url), create:  (Tuple "POST" url)}
  where
    url = baseurl++tablename

formWidget baseUrl tablename = ((createForm urls) <<< decode) <$> (http' urls.schema) 
  where
    urls = createUrls baseUrl tablename

listWidget baseUrl tablename = (createList <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename

customWidget baseUrl tablename f = ((createCustom f) <<< decode) <$> (http' urls.index) 
  where
    urls = createUrls baseUrl tablename

foreign import rawUI
  "function rawUI(str) { \
  \ return window.React.DOM.div({dangerouslySetInnerHTML: {__html: str}}); \
  \ }" :: String -> React.UI

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
