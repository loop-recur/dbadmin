module Main where

import Ui(form, list, login, nav)
import Debug.Trace
import Control.Monad.Eff

main = do
  let cfg = {host:  "https://localhost:3000/"}
--  let cfg = {host:  "https://safe-everglades-5655.herokuapp.com/"}
  login "login"
  nav cfg "nav"
  form cfg "create"
  list cfg "speakers"
