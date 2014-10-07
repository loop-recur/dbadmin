module Form where

import Api
import Types
import Control.Monad.Eff
import React
import React.DOM
import Data.Maybe
import Control.Apply((<*))
import Control.Monad.Cont.Trans(runContT)
import Data.Tuple
import Data.Array(elemIndex, filter)
import Data.JSON(decode)
import qualified Data.Map as M

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

columnsThatArentPrimaryKeys :: [String] -> [ColumnDetails] -> [ColumnDetails]
columnsThatArentPrimaryKeys pkeys columns = filter notAPkey columns
  where
    notAPkey (ColumnDetails c) = c.name `elemIndex` pkeys < 0

createForm :: URLS -> Maybe Schema -> React.UI
createForm urls ms = maybe (div' [text "Couldn't create form"]) renderComponents ms
  where
    renderComponents (Schema x) = theForm (columnsThatArentPrimaryKeys x.pkey x.columns) urls {}

widget baseUrl tablename = ((createForm urls) <<< decode) <$> (http' urls.schema) 
  where
    urls = createUrls baseUrl tablename

create urls e = do
  rs <- refsToObj <$> getRefs <* (preventDefault e)
  runContT (save' urls.create rs) (\y-> return unit)

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
