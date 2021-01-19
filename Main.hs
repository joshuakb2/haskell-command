module Main (main) where

import Prelude hiding (fail, log)
import Command (ExitCode(..), call, Stdout(..), Command, Cwd(..), CommandAbort(..), call_, fail, log, runCommand)
import Data.List.Extra (splitOn)
import Control.Monad (when)

main :: IO ()
main = do
    let file = "list of files"
    result <- runCommand $ do
        (Stdout out, ExitCode code) <- call (Cwd "/tmp") "cat" [file]
        when (code /= 0) (log $ "Surprise! cat returned " ++ show code)
        let files = filter (/= "") . splitOn " " $ out
        call_ "rm -f" files
    case result of
        Left (ProcessError code) ->
            putStrLn $ "Call failed with code " ++ show code
        Left (ManualAbort message) ->
            putStrLn $ "Command aborted with message: " ++ message
        Right () ->
            putStrLn "Call succeeded!"
