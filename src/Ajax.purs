module Ajax where

import Control.Monad.Eff

foreign import data JqAjax :: !
type EffJqAjax r = Eff (jqajax :: JqAjax | r)

foreign import jqAjax
  "function jqAjax(args) { \
  \ args.dataType = 'text'; \
  \ if(args.body) args.data = args.body; \
  \ args.type = args.method || 'GET'; \
  \ return function(cb) { \
  \   args.success = function(r){ return cb(r)(); }; \
  \   return function() { \
  \     $.ajax(args); \
  \    }\
  \  }\
  \}" :: forall a r eff. {|a} -> (String -> EffJqAjax r eff) -> (EffJqAjax r) Unit

