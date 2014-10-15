module List where

import Api
import Helper
import Types
import React
import React.DOM
import Control.Monad.Eff
import Ajax
import Data.Maybe
import Data.Monoid(mempty)
import Data.JSON hiding (object)
import Data.Tuple
import Data.String(replace)
import Control.Monad.Cont.Trans(runContT)
import qualified Data.Map as M
import Dispatcher

enter_key = 13

empty :: String
empty = mempty

type ListWidget eff prps = UIRef (state :: ReactState (write :: WriteAllowed, read :: ReadAllowed | prps) (Tuple URLS [UI]), jqajax :: JqAjax | eff)

endpoint (Tuple m p) row = Tuple m (replace "{id}" (show rowId) p)
  where
    rowId = case M.lookup "id" row of
                   (Just (JNumber i)) -> unsafeCoerce i
                   Nothing -> empty

callAndUpdateState self url body state = do
  runContT (http url body) \_ -> do
    return $ runUI self $ do
      writeState state
    return unit

--saveForReal :: forall c d. URLS -> Eff (state :: ReactState (write :: WriteAllowed, read :: ReadAllowed | c) (Tuple Row { editing :: Boolean, key :: String, val :: JValue }), jqajax :: JqAjax | d) Unit
updateField self st row urls = callAndUpdateState self (endpoint urls.update row) (refsToObj' st) (Tuple row {editing: false, val: st.val, key:st.key})

deleteRow :: forall a b c d. URLS -> a -> Eff (state :: ReactState (write :: WriteAllowed, read :: ReadAllowed | c) (Tuple RowDesc { deleted :: Boolean }), jqajax :: JqAjax | d) Unit
deleteRow urls _ = do
  self <- getSelf
  (Tuple row _) <- readState
  callAndUpdateState self (endpoint urls.destroy row) "" (Tuple row {deleted: true})

updateIfEnter urls e = do
  self <- getSelf
  (Tuple row st) <- readState
  if (e.keyCode == enter_key) then (updateField self st row urls) else (return unit)

toggleEdit _ = transformState makeEditing
  where
    makeEditing (Tuple r st) = (Tuple r {editing: true, val: st.val, key: st.key})

updateInputState v (Tuple r st) = (Tuple r {editing: true, val: v, key: st.key})

updateStateVal e = transformState $ updateInputState $ getTarget' e

editableTd urls row key val = mkUI spec {
    getInitialState = return $ (Tuple row {editing: false, val: val, key: key})
  } do
    (Tuple _ state) <- readState
    return $ td [onDoubleClick toggleEdit] if state.editing then [input [placeholder state.val, typeProp "text", onChange updateStateVal, onKeyDown (updateIfEnter urls)] []] else [text state.val]

deleteTd urls row = mkUI spec {
    getInitialState = return (Tuple row {deleted: false})
  } do
    (Tuple row state) <- readState
    return $ td' $ if state.deleted then [] else [button [onClick (deleteRow urls), typeProp "button", className "btn btn-danger"] [text "Delete"] ] 

--renderListItem :: URLS -> Row -> React.UI
tableRow urls (Row x) = tr' ((row x)++[(deleteTd urls x) {}])
  where
    row x' = fmap (makeTd x') <<< M.toList <<< unpackedJValueMap $ x'
    makeTd x' (Tuple k v) = editableTd urls x' k v $ {}

tableHeader:: Row -> UI
tableHeader (Row x) = tr' ((th' <<< pure <<< text) <$> headers)
  where
    headers = (M.keys x) ++ ["actions"]

--makeRows :: URLS -> [Row] -> [UI]
makeRows urls xs = ((headerRow xs) : (tableRows xs))
  where
    headerRow (x:xs) = tableHeader x
    tableRows = fmap (tableRow urls)


--apiGetRows :: URLS -> ContT
apiGetRows urls = ((maybe [] (makeRows urls)) <<< decode) <$> (http' urls.index) 

insertRowsToTable self urls = do
  runUI self $ do
    runContT (apiGetRows urls) \s -> do
      runUI self $ do
        writeState (Tuple urls s)
        return unit

reloadTable :: forall eff e p. (ListWidget e p) -> Event -> Eff eff Unit
reloadTable self _ = do
  runUI self $ do
    (Tuple urls _) <- readState
    insertRowsToTable self urls

getRowsLoadTable :: forall eff e p. (ListWidget e p) -> String -> (String -> Eff eff Unit)
getRowsLoadTable self baseUrl = (insertRowsToTable self) <<< (createUrls baseUrl)

theList :: String -> {} -> UI
theList baseUrl = mkUI spec {
    getInitialState = return (Tuple blankUrls []),
    componentDidMount = do
      self <- getSelf
      return $ register "created" (reloadTable self)
      return $ register "navClick" (getRowsLoadTable self baseUrl)
  } do
    (Tuple urls rows) <- readState
    return $ table [
        className "list table table-bordered table-striped"
      ] [tbody' $ rows]

widget baseUrl = theList baseUrl {}
