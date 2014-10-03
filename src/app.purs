module Main where

import React(renderToElementById)
import UI(formWidget, listWidget)
import Control.Monad.Cont.Trans(runContT)
import Control.Apply((<*))

main = do
  let baseUrl = "https://localhost:3000/"
  let tablename = "speakers"
  let form = formWidget baseUrl tablename
  let list = listWidget baseUrl tablename

  runContT form $ \y -> return unit <* (renderToElementById "create" y) 
  runContT list $ \y -> return unit <* (renderToElementById "list" y) 
