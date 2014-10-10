module List where

import Api
import Helper
import Types
import React
import React.DOM
import Data.Maybe
import Data.JSON(decode)
import Data.Tuple
import Control.Monad.Cont.Trans(runContT)
import qualified Data.Map as M
import Dispatcher

renderListHead:: Row -> React.UI
renderListHead (Row x) = tr' ((th' <<< pure <<< text) <$> M.keys x)

renderListItem :: Row -> React.UI
renderListItem (Row x) = tr' ((td' <<< pure <<< text) <$> (M.values $ unpackedJValueMap x))

makeRows :: [Row] -> [React.UI]
makeRows xs = ((getTheTopRow xs) : (renderComponents xs))
  where
    getTheTopRow (x:xs) = renderListHead x
    renderComponents xs = renderListItem <$> xs

makeFormState :: Maybe [Row] -> [React.UI]
makeFormState = maybe [] makeRows

getData baseUrl tablename = (makeFormState <<< decode) <$> (http' urls.index) 
  where
    urls :: URLS
    urls = createUrls baseUrl tablename

getDataRunState self baseUrl n = do
  runContT (getData baseUrl n) \s -> do
    runUI self $ do
      writeState (Tuple n s)
      return unit

reloadTable self baseUrl _ = do
    runUI self $ do
      (Tuple n _) <- readState
      runContT (getData baseUrl n) \s -> do
        runUI self $ do
          (Tuple name _) <- readState
          writeState (Tuple n s)
          return unit

theList :: String -> {} -> React.UI
theList baseUrl = mkUI spec {
    getInitialState = return (Tuple "" []),
    componentDidMount = do
      self <- getSelf
      return $ register "created" (reloadTable self baseUrl)
      return $ register "navClick" (getDataRunState self baseUrl)
  } do
    (Tuple name rows) <- readState
    return $ table [
        className "list table table-bordered table-striped"
      ] [tbody' $ rows]


widget baseUrl = theList baseUrl {}
