module Main where

import UI(form, list, login, nav)
import Debug.Trace
import Control.Monad.Eff

main = do
  nav  {host:  "https://localhost:3000"} "nav"
  form {host: "https://localhost:3000/", table: "speakers"} "create"
  list {host: "https://localhost:3000/", table: "speakers"} "speakers"
