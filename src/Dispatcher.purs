module Dispatcher where

import Control.Monad.Eff

foreign import register
"function register(name){ return function(f) { return Dispatcher.register(name, f); };}" :: forall a eff. String -> (a -> Eff (eff) Unit) -> Unit

foreign import trigger
"function trigger(name){ return function(data) { return function(){ Dispatcher.trigger(name, data); }; };}" :: forall a eff. String -> a -> Eff (eff) [Unit]
