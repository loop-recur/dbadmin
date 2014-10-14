module List where

import Api
import Helper
import Types
import React
import React.DOM
import Control.Monad.Eff
import Ajax
import Data.Maybe
import Data.JSON hiding (object)
import Data.Tuple
import Data.String(replace)
import Control.Monad.Cont.Trans(runContT)
import qualified Data.Map as M
import Dispatcher

enter_key = 13

--saveForReal :: forall c d. URLS -> Eff (state :: ReactState (write :: WriteAllowed, read :: ReadAllowed | c) (Tuple Row { editing :: Boolean, key :: String, val :: JValue }), jqajax :: JqAjax | d) Unit
saveForReal self st row urls = do
  runContT (save' (endpoint urls.update row) (refsToObj' st)) \_ -> do
    return $ runUI self $ do
      writeState (Tuple row {editing: false, val: st.val, key:st.key})
    return unit
  where
    endpoint (Tuple m p) row = Tuple m (replace "{id}" (show<<<getId $ row) p)
    getId row = case M.lookup "id" row of
                     (Just (JNumber i)) -> unsafeCoerce i
                     Nothing -> "-1"

saveIt urls e = do
  self <- getSelf
  (Tuple row st) <- readState
  if (e.keyCode == enter_key) then (saveForReal self st row urls) else (return unit)

toggleEdit e = do
  (Tuple r st) <- readState
  writeState (Tuple r {editing: true, val: st.val, key: st.key})

updateStateVal e = do
  let t = getTarget' e
  (Tuple r st) <- readState
  writeState (Tuple r {editing: true, val: t.value, key: st.key})

deleteRow :: forall a b c d. URLS -> a -> Eff (state :: ReactState (write :: WriteAllowed, read :: ReadAllowed | c) (Tuple Row { deleted :: Boolean }), jqajax :: JqAjax | d) Unit
deleteRow urls _ = do
  self <- getSelf
  (Tuple row state) <- readState
  runContT (http' (endpoint urls.destroy row)) \_ -> do
    return $ runUI self $ do
      writeState (Tuple row {deleted: true})
    return unit
  where
    endpoint (Tuple m p) row = Tuple m (replace "{id}" (getId $row) p)
    getId (Row row) = case M.lookup "id" row of
                     (Just (JNumber i)) -> unsafeCoerce i
                     Nothing -> "-1"

editableTd urls row key val = mkUI spec {
    getInitialState = return $ (Tuple row {editing: false, val: val, key: key})
  } do
    (Tuple _ state) <- readState
    return $ td [onDoubleClick toggleEdit] if state.editing then [input [placeholder state.val, typeProp "text", onChange updateStateVal, onKeyDown (saveIt urls)] []] else [text state.val]

deleteTd urls row = mkUI spec {
    getInitialState = return (Tuple row {deleted: false})
  } do
    (Tuple row state) <- readState
    return $ td' $ if state.deleted then [] else [button [onClick (deleteRow urls), typeProp "button", className "btn btn-danger"] [text "Delete"] ] 

renderListHead:: Row -> React.UI
renderListHead (Row x) = tr' ((th' <<< pure <<< text) <$> M.keys x)

--renderListItem :: URLS -> Row -> React.UI
renderListItem urls (Row x) = tr' ((row x)++[(deleteTd urls x) {}])
  where
    row x' = fmap (makeTd x') <<< M.toList <<< unpackedJValueMap $ x'
    makeTd x' (Tuple k v) = editableTd urls x' k v $ {}

--makeRows :: URLS -> [Row] -> [React.UI]
makeRows urls xs = ((getTheTopRow xs) : (renderComponents xs))
  where
    getTheTopRow (x:xs) = renderListHead x
    renderComponents xs = do
      x <- (renderListItem urls) <$> xs
      return x

--makeFormState :: URLS -> Maybe [Row] -> [React.UI]
makeFormState urls = maybe [] (makeRows urls)

getData urls = ((makeFormState urls) <<< decode) <$> (http' urls.index) 

getDataRunState self baseUrl n = do
  let urls = createUrls baseUrl n
  runContT (getData urls) \s -> do
    runUI self $ do
      writeState (Tuple urls s)
      return unit

reloadTable self baseUrl _ = do
    runUI self $ do
      (Tuple urls _) <- readState
      runContT (getData urls) \s -> do
        runUI self $ do
          (Tuple urls _) <- readState
          writeState (Tuple urls s)
          return unit

--theList :: String -> {} -> React.UI
theList baseUrl = mkUI spec {
    getInitialState = return (Tuple blankUrls []),
    componentDidMount = do
      self <- getSelf
      return $ register "created" (reloadTable self baseUrl)
      return $ register "navClick" (getDataRunState self baseUrl)
  } do
    (Tuple urls rows) <- readState
    return $ table [
        className "list table table-bordered table-striped"
      ] [tbody' $ rows]


widget baseUrl = theList baseUrl {}
