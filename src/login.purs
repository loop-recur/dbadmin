module Login where

import React
import React.DOM
import Control.Monad.Eff
import Helper
import Types
import Debug.Trace
import Data.Maybe
import Control.Apply((<*))

makeInput :: String -> String -> React.UI
makeInput x p = div [className "pull-left"] [input [className "form-control", typeProp p, placeholder x, name x, ref x] []]

logout _ = do
  toLocalStorage "creds" ""
  writeState {logged_in: false}

login e = do
  rs <- refsToObj <$> getRefs <* (preventDefault e)
  toLocalStorage "creds" rs
  writeState {logged_in: true}

makeForm _ = form [
          className "login",
          onSubmit login
        ] [(makeInput "username" "text"), (makeInput "password" "password"), input [className "btn btn-primary pull-left", typeProp "Submit", value "Login", readOnly "true"] []]

makeLink _ = a [className "btn btn-danger pull-right logout", onClick logout, href "#"] [text "Logout"]

widget = mkUI spec {
    getInitialState = return {logged_in: false},
    componentWillMount = do
      writeState <$> (maybe {logged_in: false} (\_ -> {logged_in: true})) <$> fromLocalStorage "creds"
      return unit
  } do
    state <- readState
    pure $ div [ className "formbox" ] if state.logged_in then [makeLink state] else [makeForm state] 


foreign import fromLocalStorage
  "function fromLocalStorage(str) { \
  \ return function() { \
  \   var x = localStorage[str]; \
  \   return x ? (new PS.Data_Maybe.Just(JSON.parse(x))) : (new PS.Data_Maybe.Nothing());\
  \ }; \
 \}" :: forall a. String -> Eff a (Maybe String)

foreign import toLocalStorage
  "function toLocalStorage(name) { \
  \ return function(json_str) { \
  \   return function() { \
  \     if(json_str) { \
  \       localStorage[name] = json_str;\
  \     } else { localStorage.removeItem(name); } \
  \   }; \
  \ }; \
 \}" :: forall a. String -> StringifiedJSON -> Eff a Unit

