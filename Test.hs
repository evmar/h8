import qualified Data.ByteString as BS
import qualified V8
import System.Environment

printError :: V8.TryCatch -> String -> IO ()
printError trycatch msg = do
  putStr $ msg ++ ": "
  exn <- V8.tryCatchException trycatch
  case exn of
    Just exn -> do
      str <- V8.valueToUtf8 exn
      BS.putStrLn str
    Nothing -> do
      putStrLn "unknown exception"

main = do
  args <- getArgs
  let code = case args of
               [code] -> code
               [] -> "'this is a sample ' + 'string of code'"
  V8.withHandleScope $ do
    global <- V8.objectTemplateNew
    context <- V8.contextNew global
    V8.contextEnter context
    V8.withTryCatch $ \trycatch -> do
      script <- V8.scriptCompile code
      case script of
        Nothing -> printError trycatch "compile error"
        Just script -> do
          result <- V8.scriptRun script
          case result of
            Nothing -> printError trycatch "run error"
            Just result -> do
              bs <- V8.valueToUtf8 result
              BS.putStrLn bs

