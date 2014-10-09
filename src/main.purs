module Main where

import UI(form, list, login, nav)
import Debug.Trace
import Control.Monad.Eff
import Dispatcher

main = do
  let disp = makeDisp ""
  login "login"
  nav  {host:  "https://localhost:3000"} "nav" disp
  form {host: "https://localhost:3000/"} "create" disp
  --list {host: "https://localhost:3000/"} "speakers"
