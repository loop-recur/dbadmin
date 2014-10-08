module Login where

import React
import React.DOM
import Control.Monad.Eff
import Helper
import Debug.Trace
import Data.Maybe
import Control.Apply((<*))

makeInput :: String -> String -> React.UI
makeInput x p = input [typeProp p, placeholder x, name x, ref x] []

logout _ = do
  toLocalStorage ""
  writeState true

makeForm s = commentForm s
  where
    commentForm = mkUI spec do
      props <- getProps
      return $ form [
          className "dbform",
          onSubmit saveCreds,
          hidden props.hidden
        ] ((input [typeProp "Submit"] []) : [(makeInput "username" "text"), (makeInput "password" "password")])


makeLink = mkUI spec do
  props <- getProps
  return $ a [onClick logout, hidden props.hidden] [text "Logout"]

widget = mkUI spec {
    componentWillMount = do
      writeState <$> (maybe {hidden: false} (\_ -> {hidden: true})) <$> fromLocalStorage "creds"
      return unit
  } do
    state <- readState
    pure $ div [ className "formbox" ] [makeForm {hidden: state.hidden}, makeLink {hidden: ""}]

saveCreds e = do
  rs <- refsToObj <$> getRefs <* (preventDefault e)
  toLocalStorage rs
  writeState false

foreign import fromLocalStorage
  "function fromLocalStorage(str) { \
  \ return function() { \
  \   var x = localStorage[str]; \
  \   return x ? (new PS.Data_Maybe.Just(x)) : (new PS.Data_Maybe.Nothing());\
  \ }; \
 \}" :: forall a. String -> Eff a (Maybe String)

foreign import toLocalStorage
  "function toLocalStorage(rec) { \
  \ return function() { \
  \ console.log('REC', typeof rec); \
  \ if(rec) { \
  \   localStorage.creds = btoa(rec.username+':'+rec.password); \
  \ } else { localStorage.removeItem('creds'); } \
  \ }; \
 \}" :: forall a. String -> Eff a Unit

