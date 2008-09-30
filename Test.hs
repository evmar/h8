import qualified Data.ByteString as BS
import qualified V8

main = do
  V8.withHandleScope $ do
    print "a"
    str <- V8.newStringUtf8 "test"
    print "b"
    bs <- V8.valueToUtf8 str
    BS.putStrLn bs
    print "c"
