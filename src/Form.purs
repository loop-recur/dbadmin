module Form where

import Api
import Types
import Helper
import Control.Monad.Eff
import React
import Debug.Trace
import React.DOM
import Data.Maybe
import Control.Apply((<*))
import Control.Monad.Cont.Trans(runContT)
import Data.Tuple
import Data.Array(elemIndex, filter)
import Data.JSON(decode)
import qualified Data.Map as M
import Dispatcher

columnsThatArentPrimaryKeys :: [String] -> [ColumnDetails] -> [ColumnDetails]
columnsThatArentPrimaryKeys pkeys columns = filter notAPkey columns
  where
    notAPkey (ColumnDetails c) = c.name `elemIndex` pkeys < 0

getCorrectColumns :: Maybe Schema -> [ColumnDetails]
getCorrectColumns ms = maybe [] getColumns ms
  where
    getColumns (Schema x) = (columnsThatArentPrimaryKeys x.pkey x.columns)

makeFormState :: URLS -> Maybe Schema -> Tuple [ColumnDetails] URLS
makeFormState urls schma = Tuple (getCorrectColumns schma) urls

getSchema baseUrl tablename = ((makeFormState urls) <<< decode) <$> (http' urls.schema) 
  where
    urls :: URLS
    urls = createUrls baseUrl tablename

getDataRunState self baseUrl n = do
  runContT (getSchema baseUrl n) \s -> do
    runUI self $ do
      writeState s
      return unit

theForm :: String -> {} -> React.UI
theForm baseUrl = mkUI spec {
    getInitialState = return (Tuple [] blankUrls),
    componentDidMount = do
      self <- getSelf
      return $ register "navClick" (getDataRunState self baseUrl)
  } do
    (Tuple schma urls) <- readState
    pure $ div [ className "formbox" ] [ commentForm { columns: schma, create: (create urls)} ]

commentForm = mkUI spec do
  props <- getProps
  return $ form [
      className "dbform",
      onSubmit props.create
    ] ((getComponent <$> props.columns) ++ [input [className "btn btn-primary pull-right", typeProp "Submit"] []])

create urls e = do
  rs <- refsToObj <$> getRefs <* (preventDefault e)
  runContT (http urls.create rs) (\y-> return unit <* trigger "created" unit <* clearFields)

makeInput :: String -> React.UI
makeInput x = input [className "form-control", typeProp "text", placeholder x, name x, ref x] []

makeTimeInput :: String -> React.UI
makeTimeInput x = input [className "form-control", typeProp "time", placeholder x, name x, ref x] []

makeText :: String -> React.UI
makeText x = textarea [className "form-control", placeholder x, name x, ref x] []

getComponent :: ColumnDetails -> React.UI
getComponent (ColumnDetails cd) = getCorrectComponent cd.name 
  where
    getCorrectComponent = case cd.kind of
      "character varying" -> makeInput
      "text" -> makeText
      "time without time zone" -> makeTimeInput
      _ -> makeInput


widget baseUrl = theForm baseUrl {}

foreign import clearFields
  "function clearFields(){ $('form').find(':input').map(function(){ if(this.type !='submit') $(this).val(''); }); }" :: forall eff. Eff(eff) Unit
