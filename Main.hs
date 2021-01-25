module Main (main) where

import Prelude hiding (fail, log)
import Command (captureStdout_, (&>), Stderr(..), ExitCode(..), call, Stdout(..), Command, Cwd(..), CommandAbort(..), call_, fail, log, runCommand)
import Data.List.Extra (split)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (intercalate)

main :: IO ()
main = do
    result <- runCommand mainCommand
    case result of
        Left error ->
            print error
        Right () ->
            putStrLn "Call succeeded!"

mainCommand :: Command ()
mainCommand = do
    let file = "list of files"
    (Stdout out, Stderr err, ExitCode code) <- call (Cwd "/tmp") "cat" [file]
    if code == 0 then do
        let filesToDelete = filter (/= "") . split isSpace $ out
        log $ "Files to delete: " ++ intercalate ", " filesToDelete
        let n = length filesToDelete
        call_ "rm" filesToDelete
        Stdout _ <- captureStdout_ $ call_ "echo" [show n] &> call_ "tee /tmp/n"
        log "Success!"
    else do
        log $ "Error from cat: " ++ err
        fail "Failed to get the list of files to delete!"
