{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module V8 (
  Handle, Value,
  withHandleScope,
  newStringUtf8,
  valueToUtf8,
  objectTemplateNew,
  contextNew, contextEnter,
  scriptCompile, scriptRun,
  TryCatch, withTryCatch, tryCatchException
) where

-- {-# INCLUDE "v8c.h" #-}
#include "/home/martine/projects/h8/v8/include/v8c.h"

import C2HS
import Control.Exception
import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

-- foreign import ccall unsafe
--   v8_set_flags_from_command_line :: Ptr CInt -> Ptr (Ptr CChar) -> CInt -> IO ()
-- {# fun unsafe v8_set_flags_from_command_line
--     {`Int', `Int'} -> `()' #}

setFlagsFromCommandLine :: IO ()
setFlagsFromCommandLine = return () -- v8_set_flags_from_command_line nullPtr nullPtr 0

data ValueT = ValueT
newtype Handle t = Handle (Ptr (Handle t))
withHandle :: Handle t -> Ptr ()
withHandle (Handle ptr) = castPtr ptr
toHandle :: Ptr () -> Handle t
toHandle ptr = Handle (castPtr ptr)

castHandle :: Handle a -> Handle b
castHandle (Handle ptr) = Handle (castPtr ptr)

-- | Represents V8 types that are subtypes of Value.
class Value a where
  toValue :: Handle a -> Handle ValueT
instance Value ValueT  where toValue = id
instance Value String where toValue = castHandle

{# pointer *V8HandleScope as HandleScope newtype #}
-- | @withHandleScope action@ runs @action@ within a new HandleScope.
withHandleScope :: IO a -> IO a
withHandleScope action =
  bracket v8_handle_scope_new v8_handle_scope_free (const action)

{# fun unsafe v8_handle_scope_new
    { } -> `HandleScope' id #}
{# fun unsafe v8_handle_scope_free
    { id `HandleScope' } -> `()' #}

{# fun unsafe v8_string_new_utf8 as newStringUtf8
    { `String'& } -> `Handle String' toHandle #}
-- {# fun unsafe v8_string_length
--     { fromHandle `Handle' } -> `Int' #}

data Context = Context
data TemplateT = TemplateT
data ObjectTemplateT = ObjectTemplateT
class Template a where
  toTemplate :: Handle a -> Handle TemplateT
instance Template TemplateT where toTemplate = id
instance Template () where toTemplate = castHandle
instance Template ObjectTemplateT where toTemplate = castHandle

{# fun unsafe v8_object_template_new as objectTemplateNew
    { } -> `Handle ObjectTemplateT' toHandle #}

{# fun unsafe v8_context_new
    { id `Ptr ()', withHandle `Handle TemplateT' }
    -> `Handle Context' toHandle #}
contextNew :: Template t => Handle t -> IO (Handle Context)
contextNew template = do
  v8_context_new nullPtr (toTemplate template)
{# fun unsafe v8_context_enter as contextEnter
    { withHandle `Handle Context' } -> `()' #}
{# fun unsafe v8_context_exit as contextExit
    { withHandle `Handle Context' } -> `()' #}

maybeHandle :: Handle t -> IO (Maybe (Handle t))
maybeHandle handle = do
  empty <- v8_handle_is_empty handle
  if empty
    then return Nothing
    else return $ Just handle
toMaybeHandle :: Ptr () -> IO (Maybe (Handle t))
toMaybeHandle = maybeHandle . toHandle

data Script = Script
{# fun unsafe v8_script_compile
    { withHandle `Handle String' } -> `Handle Script' toHandle #}
scriptCompile :: String -> IO (Maybe (Handle Script))
scriptCompile source = do
  source' <- newStringUtf8 source
  script <- v8_script_compile source'
  maybeHandle script

{# fun unsafe v8_script_run as scriptRun
    { withHandle `Handle Script' } -> `Maybe (Handle ValueT)' toMaybeHandle* #}

{# fun unsafe v8_handle_is_empty
    { withHandle `Handle t' } -> `Bool' #}

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
    { id `TryCatch' } -> `Maybe (Handle ValueT)' toMaybeHandle* #}
withTryCatch :: (TryCatch -> IO b) -> IO b
withTryCatch = bracket v8_try_catch_new v8_try_catch_free

{# pointer *V8StringUtf8Value as StringUtf8Value newtype #}
{# fun unsafe v8_string_utf8_value_new
    { withHandle `Handle ValueT' } -> `StringUtf8Value' id #}
{# fun unsafe v8_string_utf8_value_free
    { id `StringUtf8Value' } -> `()' #}
{# fun unsafe v8_string_utf8_value_length
    { id `StringUtf8Value' } -> `Int' #}
{# fun unsafe v8_string_utf8_value_chars
    { id `StringUtf8Value' } -> `CString' id #}

-- | Convert a 'Value' to a UTF-8 ByteString.
valueToUtf8 :: Value v => Handle v -> IO BS.ByteString
valueToUtf8 value = bracket init free read where
  init = v8_string_utf8_value_new (toValue value)
  free = v8_string_utf8_value_free
  read val = do
    len <- v8_string_utf8_value_length val
    chars <- v8_string_utf8_value_chars val
    BSI.create len $ \buf ->
      BSI.memcpy buf (castPtr chars) (fromIntegral len)

