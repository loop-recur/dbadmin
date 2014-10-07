module UI where

import React(renderToElementById)
import qualified Form as F
import qualified List as L
import qualified Custom as C
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import Data.Maybe(maybe)
import qualified Data.Map as M

runWidget id' w = do
  runContT w $ \y -> return unit <* (renderToElementById id' y) 

custom config id' f = do
  runWidget id' $ C.widget config.host config.table f

list config id' = do
  runWidget id' $ L.widget config.host config.table

form config id' f = do
  runWidget id' $ F.widget config.host config.table

