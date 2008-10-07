{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module V8 (
  Handle, Value,
  withHandleScope,
  newStringUtf8,
  valueToUtf8,
  templateSet,
  objectTemplateNew,
  Arguments, argumentsLength, argumentsGet,
  functionTemplateNew,
  undefined, null, true, false,
  contextNew, contextEnter, contextExit, withContext,
  scriptCompile, scriptRun,
  TryCatch, withTryCatch, tryCatchException
) where

#include "/home/martine/projects/h8/v8/include/v8c.h"

import C2HS
import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Prelude hiding (null, undefined)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI


-- | All V8 objects are wrapped in an opaque Handle.  The type
-- parameter t just helps with keeping different handle types
-- straight.
newtype Handle t = Handle (Ptr (Handle t))
withHandle :: Handle t -> Ptr ()
withHandle (Handle ptr) = castPtr ptr
toHandle :: Ptr () -> Handle t
toHandle ptr = Handle (castPtr ptr)

castHandle :: Handle a -> Handle b
castHandle (Handle ptr) = Handle (castPtr ptr)

maybeHandle :: Handle t -> IO (Maybe (Handle t))
maybeHandle handle = do
  empty <- v8_handle_is_empty handle
  if empty
    then return Nothing
    else return $ Just handle
toMaybeHandle :: Ptr () -> IO (Maybe (Handle t))
toMaybeHandle = maybeHandle . toHandle

{# fun unsafe v8_handle_is_empty
    { withHandle `Handle t' } -> `Bool' #}

data Value = Value
-- | Represents V8 types that are subtypes of Value.
class ValueT a where
  toValue :: Handle a -> Handle Value
instance ValueT Value  where toValue = id
instance ValueT String where toValue = castHandle

{# pointer *V8HandleScope as HandleScope newtype #}
-- | @withHandleScope action@ runs @action@ within a new HandleScope.
withHandleScope :: IO a -> IO a
withHandleScope action =
  bracket v8_handle_scope_new v8_handle_scope_free (const action)

{# fun unsafe v8_handle_scope_new
    { } -> `HandleScope' id #}
{# fun unsafe v8_handle_scope_free
    { id `HandleScope' } -> `()' #}

data Script = Script
{# fun unsafe v8_script_compile
    { withHandle `Handle String' } -> `Handle Script' toHandle #}
scriptCompile :: String -> IO (Maybe (Handle Script))
scriptCompile source = do
  source' <- newStringUtf8 source
  script <- v8_script_compile source'
  maybeHandle script

{# fun v8_script_run as scriptRun
    { withHandle `Handle Script' } -> `Maybe (Handle Value)' toMaybeHandle* #}

{# fun unsafe v8_string_new_utf8 as newStringUtf8
    { `String'& } -> `Handle String' toHandle #}

{# pointer *V8StringUtf8Value as StringUtf8Value newtype #}
{# fun unsafe v8_string_utf8_value_new
    { withHandle `Handle Value' } -> `StringUtf8Value' id #}
{# fun unsafe v8_string_utf8_value_free
    { id `StringUtf8Value' } -> `()' #}
{# fun unsafe v8_string_utf8_value_length
    { id `StringUtf8Value' } -> `Int' #}
{# fun unsafe v8_string_utf8_value_chars
    { id `StringUtf8Value' } -> `CString' id #}

-- | Convert a 'ValueT' to a UTF-8 ByteString.
valueToUtf8 :: ValueT v => Handle v -> IO BS.ByteString
valueToUtf8 value = bracket init free read where
  init = v8_string_utf8_value_new (toValue value)
  free = v8_string_utf8_value_free
  read val = do
    len <- v8_string_utf8_value_length val
    chars <- v8_string_utf8_value_chars val
    BSI.create len $ \buf ->
      BSI.memcpy buf (castPtr chars) (fromIntegral len)

data Template = Template
class TemplateT a where
  toTemplate :: Handle a -> Handle Template
instance TemplateT Template where toTemplate = id
instance TemplateT () where toTemplate = castHandle

data Data = Data
class DataT a where
  toData :: Handle a -> Handle Data

instance TemplateT a => DataT a where
  toData = castHandle

{# fun unsafe v8_template_set
    { withHandle `Handle Template', withHandle `Handle String',
      withHandle `Handle Data' } -> `()' #}
templateSet :: (TemplateT t, DataT d) => Handle t -> String -> Handle d -> IO ()
templateSet tmpl name value = do
  str <- newStringUtf8 name
  v8_template_set (toTemplate tmpl) str (toData value)

{# pointer *V8Arguments as Arguments newtype #}
{# fun unsafe v8_arguments_length as argumentsLength
    { id `Arguments' } -> `Int' #}
{# fun unsafe v8_arguments_get as argumentsGet
    { id `Arguments', `Int' } -> `Maybe (Handle Value)' toMaybeHandle* #}

data FunctionTemplate = FunctionTemplate
instance TemplateT FunctionTemplate where toTemplate = castHandle
type InvocationCallback = Arguments -> IO (Ptr ())
foreign import ccall "wrapper"
  mkInvocationCallback :: InvocationCallback -> IO (FunPtr InvocationCallback)

{# fun unsafe v8_function_template_new
    { id `FunPtr InvocationCallback' } -> `Handle FunctionTemplate' toHandle #}
functionTemplateNew :: (Arguments -> IO (Handle ()))
                    -> IO (Handle FunctionTemplate)
functionTemplateNew callback = do
  ccallback <- mkInvocationCallback (liftM withHandle . callback)
  -- XXX leaks ccallback.
  v8_function_template_new ccallback

data ObjectTemplate = ObjectTemplate
instance TemplateT ObjectTemplate where toTemplate = castHandle
{# fun unsafe v8_object_template_new as objectTemplateNew
    { } -> `Handle ObjectTemplate' toHandle #}

{# fun pure unsafe v8_undefined as undefined
    { } -> `Handle ()' toHandle #}
{# fun pure unsafe v8_null as null
    { } -> `Handle ()' toHandle #}
{# fun pure unsafe v8_true as true
    { } -> `Handle ()' toHandle #}
{# fun pure unsafe v8_false as false
    { } -> `Handle ()' toHandle #}

{# pointer *V8TryCatch as TryCatch newtype #}
{# fun unsafe v8_try_catch_new
    { } -> `TryCatch' id #}
{# fun unsafe v8_try_catch_free
    { id `TryCatch' } -> `()' #}
{# fun unsafe v8_try_catch_exception as tryCatchException
    { id `TryCatch' } -> `Maybe (Handle Value)' toMaybeHandle* #}
withTryCatch :: (TryCatch -> IO b) -> IO b
withTryCatch = bracket v8_try_catch_new v8_try_catch_free

data Context = Context
{# fun unsafe v8_context_new
    { id `Ptr ()', withHandle `Handle Template' }
    -> `Handle Context' toHandle #}
contextNew :: TemplateT t => Handle t -> IO (Handle Context)
contextNew template = do
  v8_context_new nullPtr (toTemplate template)
{# fun unsafe v8_context_enter as contextEnter
    { withHandle `Handle Context' } -> `()' #}
{# fun unsafe v8_context_exit as contextExit
    { withHandle `Handle Context' } -> `()' #}
withContext :: TemplateT t => Handle t -> IO a -> IO a
withContext template action = do
  context <- contextNew template
  contextEnter context
  result <- action
  contextExit context
  return result
