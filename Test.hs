import qualified Data.ByteString as BS
import qualified V8
import Control.Monad
import System.Environment

printValue :: V8.Handle V8.Value -> IO ()
printValue v = do
  str <- V8.valueToUtf8 v
  BS.putStr str

printError :: V8.TryCatch -> String -> IO ()
printError trycatch msg = do
  putStr $ msg ++ ": "
  exn <- V8.tryCatchException trycatch
  case exn of
    Just exn -> printValue exn >> putStrLn ""
    Nothing -> putStrLn "unknown exception"

printCallback :: V8.Arguments -> IO (V8.Handle ())
printCallback args = do
  argc <- V8.argumentsLength args
  putStr "print("
  forM_ [0..argc-1] $ \i -> do
    arg <- V8.argumentsGet args i
    case arg of
      Just arg -> do
        when (i > 0) $ putStr ", "
        printValue arg
      Nothing -> return ()
  putStrLn ")"
  return V8.undefined

main = do
  args <- getArgs
  let code = case args of
               [code] -> code
               [] -> "'this is a sample ' + 'string of code'"
  V8.withHandleScope $ do
    print_cb <- V8.functionTemplateNew printCallback
    global <- V8.objectTemplateNew
    V8.templateSet global "print" print_cb
    V8.withContext global $ do
      V8.withTryCatch $ \trycatch -> do
        script <- V8.scriptCompile code
        case script of
          Nothing -> printError trycatch "compile error"
          Just script -> do
            result <- V8.scriptRun script
            case result of
              Nothing -> printError trycatch "run error"
              Just result -> do
                putStr " => "
                printValue result
                putStrLn ""
