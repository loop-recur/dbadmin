module Main where

import React(renderToElementById)
import UI(formWidget, listWidget, customWidget)
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))
import Data.Maybe(maybe)
import qualified Data.Map as M

makeImage row = "<img src="++(avatar row)++"/>"
  where
    avatar r = maybe "" id (M.lookup "avatar" r)

main = do
  let baseUrl = "https://localhost:3000/"
  let form = formWidget baseUrl "speakers"
  let speakerlist = listWidget baseUrl "speakers"
  let sessionlist = listWidget baseUrl "sessions"
  let customlist = customWidget baseUrl "speakers" makeImage

  runContT form $ \y -> return unit <* (renderToElementById "create" y) 
  runContT speakerlist $ \y -> return unit <* (renderToElementById "speakers" y) 
  runContT sessionlist $ \y -> return unit <* (renderToElementById "sessions" y) 
  runContT customlist $ \y -> return unit <* (renderToElementById "custom" y) 
