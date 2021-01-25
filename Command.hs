{-# LANGUAGE FlexibleInstances, NamedFieldPuns, LambdaCase, MultiParamTypeClasses, ScopedTypeVariables, RecordWildCards, GADTs #-}

module Command (Command, Cwd(..), CommandAbort(..), Stdout(..), Stderr(..), ExitCode(..), call, call_, fail, log, runCommand, (&>), captureStdout_) where

import Prelude hiding (fail, log)
import Control.Monad.State.Lazy (modify, MonadIO(..), evalStateT, MonadTrans(..), StateT(..),  State, evalState)
import Data.Char (isSpace)
import Data.List (foldl', intercalate)
import System.Directory (getCurrentDirectory)
import Data.Either.Extra (mapLeft)
import System.FilePath (joinPath)
import Data.List.Extra (split)
import Control.Monad.Trans.State.Lazy (put, get)
import qualified System.Process as P
import System.IO (stdin, hClose, stderr, stdout, Handle, hGetContents)
import Data.IORef (newIORef, writeIORef)
import Control.Concurrent (putMVar, newEmptyMVar, takeMVar, forkIO, newMVar, MVar)
import Data.Traversable (for)
import qualified System.Exit as X
import Data.Maybe (maybeToList)
import Control.Monad (void, when)
import Control.Concurrent.Chan
import Data.Foldable (for_)
import System.IO.Unsafe (unsafePerformIO)

newtype Command a = Command { unCommand :: EitherT CommandAbort (StateT CommandConfig IO) a }

instance Functor Command where
    fmap f =
        Command . fmap f . unCommand

instance Applicative Command where
    pure =
        Command . return
    (Command mf) <*> (Command ma) =
        Command (mf <*> ma)

instance Monad Command where
    return = pure
    (Command ma) >>= f =
        Command (ma >>= (unCommand . f))

newtype EitherT l m r = EitherT { runEitherT :: m (Either l r) }

instance MonadTrans (EitherT l) where
    lift m =
        EitherT (Right <$> m)

instance (Monad m) => Functor (EitherT l m) where
    fmap f =
        EitherT . fmap (fmap f) . runEitherT

instance (Monad m) => Applicative (EitherT l m) where
    pure =
        EitherT . return . return
    (EitherT mf) <*> (EitherT ma) =
        EitherT $
            mf >>= \eitherF ->
            ma >>= \eitherA ->
                return (eitherF <*> eitherA)

instance (Monad m) => Monad (EitherT l m) where
    return = pure
    (EitherT ma) >>= f =
        EitherT $
            ma >>= \case
                Left l ->
                    return (Left l)
                Right r ->
                    runEitherT (f r)

instance (MonadIO m) => MonadIO (EitherT l m) where
    liftIO io =
        EitherT (Right <$> liftIO io)

instance MonadIO Command where
    liftIO io =
        Command (liftIO io)

abort :: CommandAbort -> Command a
abort =
    Command . EitherT . return . Left

runCommand :: Command a -> IO (Either CommandAbort a)
runCommand command = do
    config <- defaultCommandConfig
    runCommandWithConfig config command

runCommand_ :: Command a -> IO (Either CommandAbort ())
runCommand_ =
    fmap void . runCommand

runCommandUnsafe :: Command a -> IO a
runCommandUnsafe command = do
    result <- runCommand command
    case result of
        Left err ->
            error (show err)
        Right r ->
            return r

runCommandWithConfig :: CommandConfig -> Command a -> IO (Either CommandAbort a)
runCommandWithConfig config =
    (`evalStateT` config) . runEitherT . unCommand

getCommandConfig :: Command CommandConfig
getCommandConfig =
    Command (lift get)

setCommandConfig :: CommandConfig -> Command ()
setCommandConfig =
    Command . lift . put

modifyCommandConfig :: (CommandConfig -> CommandConfig) -> Command ()
modifyCommandConfig =
    Command . lift . modify

data CommandConfig = CommandConfig
    { configCwd :: String
    , configStdin :: ReadStreamConfig
    , configStdout :: WriteStreamConfig
    , configStderr :: WriteStreamConfig
    , configNoAbortOnFail :: Bool
    , configRunInBackground :: Bool
    }

data WriteStreamConfig
    = UsingWriteHandle Handle
    | Collecting
    | PipingToFile String

instance Show WriteStreamConfig where
    show Collecting = "collecting"
    show (UsingWriteHandle handle) = "using handle (" ++ show handle ++ ")"
    show (PipingToFile file) = "piping to file (" ++ file ++ ")"

data ReadStreamConfig
    = UsingReadHandle Handle
    | PipingFromFile String

instance Show ReadStreamConfig where
    show (UsingReadHandle handle) = "using handle (" ++ show handle ++ ")"
    show (PipingFromFile file) = "piping from file (" ++ file ++ ")"

class ToStdStream a where
    getStdStream :: a -> P.StdStream

instance ToStdStream WriteStreamConfig where
    getStdStream Collecting = P.CreatePipe
    getStdStream (UsingWriteHandle handle) = P.UseHandle handle
    getStdStream (PipingToFile file) = error "Not implemented"

instance ToStdStream ReadStreamConfig where
    getStdStream (UsingReadHandle handle) = P.UseHandle handle
    getStdStream (PipingFromFile file) = error "Not implemented"

localConfig :: (CommandConfig -> CommandConfig) -> Command a -> Command a
localConfig f command = do
    originalConfig <- getCommandConfig
    setCommandConfig (f originalConfig)
    result <- command
    setCommandConfig originalConfig
    return result

cd :: String -> Command ()
cd path = do
    config@CommandConfig{ configCwd } <- getCommandConfig
    setCommandConfig config{ configCwd = relativeTo configCwd path }

withCwd :: String -> Command a -> Command a
withCwd path =
    localConfig (\config@CommandConfig{ configCwd } -> config{ configCwd = relativeTo configCwd path })

relativeTo :: FilePath -> FilePath -> FilePath
relativeTo from to
    | isAbsolute to = to
    | isAbsolute from =
        error $ concat ["Cannot compute path to \"", to, "\" relative to \"", from, "\" without knowing the current working directory."]
    | otherwise =
        relativeTo' (parts from) (parts to)
    where
        isAbsolute = startsWith '/'
        parts = filter (\x -> x /= "" && x /= ".") . split (== '/')
        relativeTo' [] [] = "."
        relativeTo' from [] = joinPath $ map (const "..") from
        relativeTo' [] to = joinPath to
        relativeTo' from@(a:as) to@(b:bs)
            | a == b = relativeTo' as bs
            | otherwise = joinPath $ map (const "..") from ++ to

startsWith :: Eq a => a -> [a] -> Bool
startsWith _ [] = False
startsWith x (y:_) = x == y

instance Show CommandConfig where
    show CommandConfig{ .. } =
        intercalate ", "
            [ "Config"
            , "CWD = " ++ configCwd
            , "stdout? " ++ show configStdout
            , "stderr? " ++ show configStderr
            , "No abort on failure? " ++ show configNoAbortOnFail
            , "Run in background? " ++ show configRunInBackground
            ]

defaultCommandConfig :: IO CommandConfig
defaultCommandConfig = do
    cwd <- getCurrentDirectory
    return $ CommandConfig
        { configCwd = cwd
        , configStdin = UsingReadHandle stdin
        , configStdout = UsingWriteHandle stdout
        , configStderr = UsingWriteHandle stderr
        , configNoAbortOnFail = False
        , configRunInBackground = False
        }

captureStream :: StreamType s => (Handle -> CommandConfig -> CommandConfig) -> Command a -> Command (s, a)
captureStream updateConfig command = do
    (readHandle, writeHandle) <- liftIO P.createPipe
    log $ "Created handle: " ++ show writeHandle
    outputVar <- liftIO $ collectFromHandle readHandle
    r <- try $ localConfig (updateConfig writeHandle) command
    liftIO $ hClose writeHandle
    log $ "Closed handle: " ++ show writeHandle
    output <- liftIO $ takeMVar outputVar
    case r of
        Left error ->
            abort error
        Right r ->
            return (wrapStream output, r)

captureStdout :: Command a -> Command (Stdout, a)
captureStdout =
    captureStream (\handle config ->
        config{ configStdout = UsingWriteHandle handle })

captureStdout_ :: Command a -> Command Stdout
captureStdout_ =
    fmap fst . captureStdout

captureStderr :: Command a -> Command (Stderr, a)
captureStderr =
    captureStream (\handle config ->
        config{ configStderr = UsingWriteHandle handle })

captureStderr_ :: Command a -> Command Stderr
captureStderr_ =
    fmap fst . captureStderr

try :: Command a -> Command (Either CommandAbort a)
try command = do
    config <- getCommandConfig
    liftIO $ runCommandWithConfig config command

(&>) :: Command a -> Command b -> Command (a, b)
(&>) commandA commandB = do
    (stdin, stdout) <- liftIO P.createPipe
    mvarA <- runInBackground (setStdout stdout) commandA
    mvarB <- runInBackground (setStdin stdin) commandB
    resultA <- liftIO $ takeMVar mvarA
    resultB <- liftIO $ takeMVar mvarB
    case (resultA, resultB) of
        (Left err, _) ->
            abort err
        (_, Left err) ->
            abort err
        (Right a, Right b) ->
            return (a, b)
    where
        runInBackground :: (CommandConfig -> CommandConfig) -> Command a -> Command (MVar (Either CommandAbort a))
        runInBackground modifyConfig command = do
            mvar <- liftIO newEmptyMVar
            config <- getCommandConfig
            liftIO . forkIO $ do
                result <- runCommandWithConfig (modifyConfig config) command
                putMVar mvar result
            return mvar
        setStdout :: Handle -> CommandConfig -> CommandConfig
        setStdout handle config =
            config{ configStdout = UsingWriteHandle handle }
        setStdin :: Handle -> CommandConfig -> CommandConfig
        setStdin handle config =
            config{ configStdin = UsingReadHandle handle }

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) =
    (a, f b)

data CommandAbort
    = ProcessError String Int
    | ManualAbort String
    | InvalidCallError String

instance Show CommandAbort where
        show (ProcessError cmd code) =
            "\"" ++ cmd ++ "\" failed with code " ++ show code
        show (ManualAbort message) =
            "Command aborted with message: " ++ message
        show (InvalidCallError message) =
            "Invalid call error: " ++ message

fail :: String -> Command ()
fail message =
    abort (ManualAbort message)

logMVar :: MVar ()
logMVar =
    unsafePerformIO (newMVar ())
{-# NOINLINE logMVar #-}

log :: String -> Command ()
log message =
    liftIO $ do
        takeMVar logMVar
        putStrLn message
        putMVar logMVar ()

data CallArgAcc = CallArgAcc
    { callCmdAndArgs :: [String]
    , callCwd :: Inherited String
    , callStdin :: Inherited ReadStreamConfig
    }

applyCallArgAcc :: CallArgAcc -> CommandConfig -> CommandConfig
applyCallArgAcc CallArgAcc{ .. } config@CommandConfig{ .. } =
    config
        { configCwd = evalInherited callCwd configCwd
        , configStdin = evalInherited callStdin configStdin
        }

defaultAcc :: CallArgAcc
defaultAcc = CallArgAcc
    { callCmdAndArgs = []
    , callCwd = Inherit
    , callStdin = Inherit
    }

addArgs :: [String] -> CallArgAcc -> CallArgAcc
addArgs new acc@CallArgAcc{ callCmdAndArgs } =
    acc{ callCmdAndArgs = callCmdAndArgs ++ new }

setCallCwd :: String -> CallArgAcc -> CallArgAcc
setCallCwd newCwd acc@CallArgAcc{ callCwd } =
    case callCwd of
        Inherit ->
            acc{ callCwd = Override newCwd }
        Override oldCwd ->
            error $ "Cannot set CWD to both \"" ++ oldCwd ++ "\" and \"" ++ newCwd ++ "\"."

setCallStdin :: ReadStreamConfig -> CallArgAcc -> CallArgAcc
setCallStdin newStdin acc@CallArgAcc{ callStdin } =
    case callStdin of
        Inherit ->
            acc{ callStdin = Override newStdin }
        Override oldStdin ->
            error $ "Cannot set stdin to both \"" ++ show oldStdin ++ "\" and \"" ++ show newStdin ++ "\"."


data Inherited a
    = Inherit
    | Override a

evalInherited :: Inherited a -> a -> a
evalInherited Inherit a = a
evalInherited (Override a) _ = a

-- Variadic function
-- Can take a string or an array of strings
-- Can produce a string or a function that consumes an additional argument, depending on the context.
call :: (CallResult r) => r
call =
    getCallResult defaultAcc

call_ :: (CallResult r, Unit r) => r
call_ =
    call

class Unit r

instance {-# OVERLAPPING #-} Unit r => Unit (a -> r)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (Command a)

-- Class of types that can be given to "call" as an argument.
class CallArg v where
    addArg :: v -> CallArgAcc -> CallArgAcc

-- If a string is given as an arg, split it by whitespace and add each word.
instance CallArg String where
    addArg = addArgs . splitByWhiteSpace

-- If a list of strings is given as an arg, add each string without splitting by whitespace.
instance CallArg [String] where
    addArg = addArgs

newtype Cwd = Cwd String

instance CallArg Cwd where
    addArg (Cwd cwd) =
        setCallCwd cwd

newtype StdinHandle = StdinHandle Handle

instance CallArg StdinHandle where
    addArg (StdinHandle handle) =
        setCallStdin (UsingReadHandle handle)

newtype StdoutToFile = StdoutToFile String
newtype StderrToFile = StderrToFile String

instance CallArg StdoutToFile where
    addArg (StdoutToFile file) =
        error "Not implemented"

instance CallArg StderrToFile where
    addArg (StderrToFile file) =
        error "Not implemented"

-- Class of types that can be the output of the "call" function.
-- Since "call" is variadic, it must be able to produce different outputs in different contexts.
-- Usually the final application of "call" will produce a string, but all the previous invocations must produce another function so that the next args can be consumed.
class CallResult r where
    getCallResult :: CallArgAcc -> r

instance CallOutput r => CallResult (Command r) where
    getCallResult acc = do
        config <- getCommandConfig
        let newConfig = (considerImplications (callImplications :: CallImplications r) . applyCallArgAcc acc) config
        let cmdAndArgs = callCmdAndArgs acc
        when (null cmdAndArgs) (abort (InvalidCallError "No command specified!"))
        log (show cmdAndArgs ++ "\n" ++ show newConfig)
        eitherExitCodeOrResults <- liftIO $ doCall newConfig cmdAndArgs
        case eitherExitCodeOrResults of
            Left exitCode ->
                abort (ProcessError (head cmdAndArgs) exitCode)
            Right results ->
                return (extractCallOutput results)

doCall :: CommandConfig -> [String] -> IO (Either Int [CallOutputT])
doCall config [] = error "cmdAndArgs must not be an empty list! We at least need a command!"
doCall config@CommandConfig{ .. } (cmd:args) = do
    let stdinAction = getStdStream configStdin
    let stdoutAction = getStdStream configStdout
    let stderrAction = getStdStream configStderr
    let process = (P.proc cmd args){ P.cwd = Just configCwd, P.std_in = stdinAction, P.std_out = stdoutAction, P.std_err = stderrAction }
    (_, maybeStdout, maybeStderr, handle) <- P.createProcess process
    maybeOutMVar <- for maybeStdout collectFromHandle
    maybeErrMVar <- for maybeStderr collectFromHandle
    stdoutResult <- maybeToList <$> for maybeOutMVar (fmap (CallStdout . Stdout) . takeMVar)
    stderrResult <- maybeToList <$> for maybeErrMVar (fmap (CallStderr . Stderr) . takeMVar)
    exitCode <- P.waitForProcess handle
    case exitCode of
        X.ExitSuccess ->
            return (Right (stdoutResult <> stderrResult <> [ CallExitCode (ExitCode 0) ]))
        X.ExitFailure code ->
            if configNoAbortOnFail then
                return (Right (stdoutResult <> stderrResult <> [ CallExitCode (ExitCode code) ]))
            else
                return (Left code)

collectFromHandle :: Handle -> IO (MVar String)
collectFromHandle handle = do
    mvar <- newEmptyMVar
    forkIO $ do
        text <- hGetContents handle
        let trimmed = if (not . null) text && last text == '\n'
            then init text
            else text
        putMVar mvar trimmed
    return mvar


-- When the result is another function, it should add our arg to the list and then get the next result.
instance (CallArg v, CallResult r) => CallResult (v -> r) where
    getCallResult acc v =
        getCallResult (addArg v acc)

class HasCallImplications r where
    callImplications :: CallImplications r

class HasCallImplications r => CallOutput r where
    extractCallOutput :: [CallOutputT] -> r

-- Uses a phantom type so that a constant of the phantom type can be part of the CallOutput class.
newtype CallImplications a = CallImplications [CallImplication]

data CallImplication
    = CollectStdout
    | CollectStderr
    | NoAbortOnFail

data CallOutputT
    = CallStdout Stdout
    | CallStderr Stderr
    | CallExitCode ExitCode

newtype Stdout = Stdout String
newtype Stderr = Stderr String

class StreamType s where
    wrapStream :: String -> s

instance StreamType Stdout where
    wrapStream = Stdout

instance StreamType Stderr where
    wrapStream = Stderr

newtype ExitCode = ExitCode Int

instance HasCallImplications Stdout where
    callImplications =
        CallImplications [CollectStdout]

instance CallOutput Stdout where
    extractCallOutput [] =
        error "Did not find stdout output in list of outputs!"
    extractCallOutput ((CallStdout stdout):_) =
        stdout
    extractCallOutput (_:rest) =
        extractCallOutput rest

instance HasCallImplications Stderr where
    callImplications =
        CallImplications [CollectStderr]

instance CallOutput Stderr where
    extractCallOutput [] =
        error "Did not find stderr output in list of outputs!"
    extractCallOutput ((CallStderr stderr):_) =
        stderr
    extractCallOutput (_:rest) =
        extractCallOutput rest

instance HasCallImplications ExitCode where
    callImplications =
        CallImplications [NoAbortOnFail]

instance CallOutput ExitCode where
    extractCallOutput [] =
        error "Did not find ExitCode output in list of outputs!"
    extractCallOutput ((CallExitCode exitCode):_) =
        exitCode
    extractCallOutput (_:rest) =
        extractCallOutput rest

instance HasCallImplications () where
    callImplications = CallImplications []

instance CallOutput () where
    extractCallOutput = const ()

instance (HasCallImplications a, HasCallImplications b) => HasCallImplications (a, b) where
    callImplications =
        CallImplications (aImpls ++ bImpls)
        where
            CallImplications aImpls = callImplications :: CallImplications a
            CallImplications bImpls = callImplications :: CallImplications b

instance (CallOutput a, CallOutput b) => CallOutput (a, b) where
    extractCallOutput outputs =
        (extractCallOutput outputs :: a, extractCallOutput outputs :: b)

instance (HasCallImplications a, HasCallImplications b, HasCallImplications c) => HasCallImplications (a, b, c) where
    callImplications =
        CallImplications (aImpls ++ bImpls ++ cImpls)
        where
            CallImplications aImpls = callImplications :: CallImplications a
            CallImplications bImpls = callImplications :: CallImplications b
            CallImplications cImpls = callImplications :: CallImplications c

instance (CallOutput a, CallOutput b, CallOutput c) => CallOutput (a, b, c) where
    extractCallOutput outputs =
        (extractCallOutput outputs :: a, extractCallOutput outputs :: b, extractCallOutput outputs :: c)

inParallel :: [Command a] -> Command [a]
inParallel commands = do
    channel <- liftIO newChan
    config <- getCommandConfig
    for_ commands $ \command -> liftIO . forkIO $ do
        result <- runCommandWithConfig config command
        writeChan channel result
    results <- liftIO $ collectResults (length commands) [] channel
    case results of
        Left error ->
            abort error
        Right results ->
            return (reverse results)
    where
        collectResults :: Int -> [a] -> Chan (Either CommandAbort a) -> IO (Either CommandAbort [a])
        collectResults 0 r _ = return (Right r)
        collectResults n r channel = do
            nextResult <- readChan channel
            case nextResult of
                Left error ->
                    return (Left error)
                Right result ->
                    collectResults (n - 1) (result : r) channel

considerImplications :: CallImplications r -> CommandConfig -> CommandConfig
considerImplications (CallImplications impls) config =
    foldl' consider config impls
    where
        consider :: CommandConfig -> CallImplication -> CommandConfig
        consider config CollectStdout =
            config{ configStdout = Collecting }
        consider config CollectStderr =
            config{ configStderr = Collecting }
        consider config NoAbortOnFail =
            config{ configNoAbortOnFail = True }

splitByWhiteSpace :: String -> [String]
splitByWhiteSpace [] = []
splitByWhiteSpace (c:cs) =
    reverse $ split_ [] [c] cs
    where
        split_ list [] [] = list
        split_ list [] (c:cs) =
            if isSpace c then
                split_ list [] cs
            else
                split_ list [c] cs
        split_ list current [] = reverse current : list
        split_ list current (c:cs) =
            if isSpace c then
                split_ (reverse current : list) [] cs
            else
                split_ list (c : current) cs
