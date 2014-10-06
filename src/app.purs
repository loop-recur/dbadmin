module Main where

import React(renderToElementById)
import UI(formWidget, listWidget, customWidget)
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import Data.Maybe(maybe)
import qualified Data.Map as M

runWidget id' w = do
  runContT w $ \y -> return unit <* (renderToElementById id' y) 

custom config id' f = do
  runWidget id' $ customWidget config.host config.table f

list config id' = do
  runWidget id' $ listWidget config.host config.table

