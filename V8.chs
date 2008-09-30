{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module V8 (
  Handle, Value,
  V8Value,
  withHandleScope,
  newStringUtf8,
  valueToUtf8
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

data Value = Value
newtype Handle t = Handle (Ptr (Handle t))
withHandle :: Handle t -> Ptr ()
withHandle (Handle ptr) = castPtr ptr
toHandle :: Ptr () -> Handle t
toHandle ptr = Handle (castPtr ptr)

castHandle :: Handle a -> Handle b
castHandle (Handle ptr) = Handle (castPtr ptr)

-- | Represents V8 types that are subtypes of Value.
class V8Value a where
  toV8Value :: Handle a -> Handle Value
instance V8Value Value  where toV8Value = id
instance V8Value String where toV8Value = castHandle
withValueHandle :: V8Value v => Handle v -> Ptr ()
withValueHandle = withHandle . toV8Value

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
data Template = Template
{# fun unsafe v8_context_new
    { id `Ptr ()', withHandle `Handle Template' } -> `Handle Context' toHandle #}
{# fun unsafe v8_context_enter
    { withHandle `Handle Context' } -> `()' #}
{# fun unsafe v8_context_exit
    { withHandle `Handle Context' } -> `()' #}


{# pointer *V8StringUtf8Value as StringUtf8Value newtype #}
{# fun unsafe v8_string_utf8_value_new
    { withHandle `Handle Value' } -> `StringUtf8Value' id #}
{# fun unsafe v8_string_utf8_value_free
    { id `StringUtf8Value' } -> `()' #}
{# fun unsafe v8_string_utf8_value_length
    { id `StringUtf8Value' } -> `Int' #}
{# fun unsafe v8_string_utf8_value_chars
    { id `StringUtf8Value' } -> `CString' id #}

-- | Convert a 'V8Value' to a UTF-8 ByteString.
valueToUtf8 :: V8Value v => Handle v -> IO BS.ByteString
valueToUtf8 value = bracket init free read where
  init = v8_string_utf8_value_new (toV8Value value)
  free = v8_string_utf8_value_free
  read val = do
    len <- v8_string_utf8_value_length val
    chars <- v8_string_utf8_value_chars val
    BSI.create len $ \buf ->
      BSI.memcpy buf (castPtr chars) (fromIntegral len)

