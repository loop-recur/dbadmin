module Form  where

import Api
import Ajax
import Types
import Helper
import Control.Monad.Eff
import React
import React.DOM
import Data.Maybe
import Control.Apply((<*))
import Control.Bind((=<<))
import Control.Monad.Cont.Trans
import Data.Tuple
import Data.Array(elemIndex, filter)
import Data.JSON(decode)
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

getSchema :: forall eff. URLS -> ContT Unit (EffJqAjax eff) (Tuple [ColumnDetails] URLS)
getSchema urls = ((makeFormState urls) <<< decode) <$> (http' urls.schema) 

getDataRunState :: forall eff a b. UIRef (state :: ReactState (write :: WriteAllowed, read :: ReadAllowed | a) (Tuple [ColumnDetails] URLS) | b) -> String -> String -> EffJqAjax eff Unit
getDataRunState self baseUrl tname = do
  let urls = createUrls baseUrl tname
  runContT (getSchema urls) $ \s -> do
    (runUI self) $ do
      writeState s
      return unit

create :: forall eff. URLS -> Event -> Eff (refs :: ReactRefs {}, jqajax :: JqAjax | eff) Unit
create urls e = do
  rs <- refsToObj <$> getRefs <* (preventDefault e)
  runContT (http urls.create rs) (\y-> return unit <* trigger "created" unit <* clearFields)

makeInput :: String -> String -> UI
makeInput t x = input [className "form-control", typeProp t, placeholder x, name x, ref x] []

makeText :: String -> UI
makeText x = textarea [className "form-control", placeholder x, name x, ref x] []

uiForColumnType :: ColumnDetails -> UI
uiForColumnType (ColumnDetails cd) = getCorrectComponent cd.name 
  where
    getCorrectComponent = case cd.kind of
      "character varying" -> makeInput "text"
      "text" -> makeText
      "time without time zone" -> makeInput "time"
      _ -> makeInput "text"

formFields :: forall eff prps rfs s r. { create :: Event -> EventHandlerContext eff prps rfs s r, columns :: [ColumnDetails] } -> UI
formFields = mkUI spec do
  props <- getProps
  return $ form [
      className "dbform",
      onSubmit props.create
    ] ((uiForColumnType <$> props.columns) ++ [input [className "btn btn-primary pull-right", typeProp "Submit"] []])

theForm :: String -> {} -> UI
theForm baseUrl = mkUI spec {
    getInitialState = return (Tuple [] blankUrls),
    componentDidMount = do
      self <- getSelf
      return $ register "navClick" (getDataRunState self baseUrl)
  } do
    (Tuple schma urls) <- readState
    pure $ div [ className "formbox" ] [ formFields { columns: schma, create: (create urls)} ]

widget baseUrl = theForm baseUrl {}

foreign import clearFields
  "function clearFields(){ $('form').find(':input').map(function(){ if(this.type !='submit') $(this).val(''); }); }" :: forall eff. Eff(eff) Unit
