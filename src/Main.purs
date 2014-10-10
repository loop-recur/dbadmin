module Main where

import Ui(form, list, login, nav)
import Debug.Trace
import Control.Monad.Eff

main = do
  let cfg = {host:  "https://localhost:3000/"}
  login "login"
  nav cfg "nav"
  form cfg "create"
  list cfg "speakers"
