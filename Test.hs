import qualified Data.ByteString as BS
import qualified V8
import System.Environment

main = do
  args <- getArgs
  let code = case args of
               [code] -> code
               [] -> "'this is a sample ' + 'string of code'"
  V8.withHandleScope $ do
    global <- V8.objectTemplateNew
    context <- V8.contextNew global
    V8.contextEnter context
    script <- V8.scriptCompile code
    case script of
      Nothing -> putStrLn "compile error"
      Just script -> do
        result <- V8.scriptRun script
        case result of
          Nothing -> putStrLn "run error"
          Just result -> do
            bs <- V8.valueToUtf8 result
            BS.putStrLn bs

