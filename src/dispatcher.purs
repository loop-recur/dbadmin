module Dispatcher where

foreign import init
"function init(x){ return function(y) { return Dispatcher.init(x, y); }}" :: forall a b. a -> b -> b

foreign import register
"function register(name){ return function(f) { return Dispatcher.register(name, f); }}" :: forall a b. a -> b -> b

foreign import trigger
"function init(name){ return function(data) { return Dispatcher.trigger(name, data); }}" :: forall a b. a -> b -> b
