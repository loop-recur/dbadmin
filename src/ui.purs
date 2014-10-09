module UI where

import React(renderToElementById)
import qualified Form as F
import qualified List as L
import qualified Custom as C
import qualified Login as Lg
import qualified Nav as Nav
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import Data.Maybe(maybe)
import qualified Data.Map as M

runWidget id' w = do
  runContT w $ \y -> return unit <* (renderToElementById id' y) 

custom config id' f = do
  runWidget id' $ C.widget config.host config.table f

list config id' = do
  renderToElementById id' $ L.widget config.host

form config id' = do
  renderToElementById id' $ F.widget config.host

login id' = do
  renderToElementById id' $ Lg.widget {}

nav config id' = do
  runWidget id' $ Nav.widget config.host
