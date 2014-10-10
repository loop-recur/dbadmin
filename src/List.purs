module List where

import Api
import Helper
import Types
import React
import React.DOM
import Data.Maybe
import Data.JSON(decode)
import Data.Tuple
import Debug.Trace
import Control.Monad.Cont.Trans(runContT)
import qualified Data.Map as M
import Dispatcher

toggleEdit e = do
  trace "hey"

deleteRow urls e = do
  trace $ snd $ urls.index

renderListHead:: Row -> React.UI
renderListHead (Row x) = tr' ((th' <<< pure <<< text) <$> M.keys x)

renderListItem :: URLS -> Row -> React.UI
renderListItem urls (Row x) = tr' (row++[deleteLink])
  where
    row = (td [onDoubleClick toggleEdit] <<< pure <<< text) <$> (M.values $ unpackedJValueMap x)
    deleteLink = td' [button [onClick (deleteRow urls), typeProp "button", className "btn btn-danger"] [text "Delete"] ] 

makeRows :: URLS -> [Row] -> [React.UI]
makeRows urls xs = ((getTheTopRow xs) : (renderComponents xs))
  where
    getTheTopRow (x:xs) = renderListHead x
    renderComponents xs = (renderListItem urls) <$> xs

makeFormState :: URLS -> Maybe [Row] -> [React.UI]
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

theList :: String -> {} -> React.UI
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
