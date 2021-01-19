{-# LANGUAGE FlexibleInstances, NamedFieldPuns, LambdaCase, MultiParamTypeClasses, ScopedTypeVariables #-}

module Command (Command, Cwd(..), CommandAbort(..), Stdout(..), Stderr(..), ExitCode(..), call, call_, fail, log, runCommand) where

import Prelude hiding (fail, log)
import Control.Monad.State.Lazy (MonadIO(..), MonadState(..), evalStateT, MonadTrans(..), StateT(runStateT),  State, evalState)
import Data.Char (isSpace)
import Data.List (foldl', intercalate)
import System.Directory (getCurrentDirectory)
import Data.Either.Extra (mapLeft)

newtype Command a = Command { unCommand :: EitherT CommandAbort (CommandState IO) a }

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

instance MonadState CommandConfig Command where
    get =
        Command (lift get)
    put =
        Command . lift . put
    state =
        Command . lift . state

abort :: CommandAbort -> Command a
abort =
    Command . EitherT . return . Left

runCommand :: Command a -> IO (Either CommandAbort a)
runCommand action = do
    config <- defaultCommandConfig
    ((`evalStateT` config) . runEitherT . unCommand) action


getCommandConfig :: Command CommandConfig
getCommandConfig =
    get

type CommandState m = StateT CommandConfig m

data CommandConfig = CommandConfig
    { configCwd :: String
    , configCollectStdout :: Bool
    , configCollectStderr :: Bool
    , configNoAbortOnFail :: Bool
    }

instance Show CommandConfig where
    show CommandConfig{ configCwd, configCollectStdout, configCollectStderr, configNoAbortOnFail } =
        intercalate ", "
            [ "Config"
            , "CWD = " ++ configCwd
            , "Collect stdout? " ++ show configCollectStdout
            , "Collect stderr? " ++ show configCollectStderr
            , "No abort on failure? " ++ show configNoAbortOnFail
            ]

defaultCommandConfig :: IO CommandConfig
defaultCommandConfig = do
    cwd <- getCurrentDirectory
    return $ CommandConfig
        { configCwd = cwd
        , configCollectStdout = False
        , configCollectStderr = False
        , configNoAbortOnFail = False
        }

data CommandAbort
    = ProcessError Int
    | ManualAbort String

fail :: String -> Command ()
fail message =
    abort (ManualAbort message)

log :: String -> Command ()
log message =
    liftIO (putStrLn message)

data CallArgAcc = CallArgAcc
    { callCmdAndArgs :: [String]
    , callCwd :: Inherited String
    }

applyCallArgAcc :: CallArgAcc -> CommandConfig -> CommandConfig
applyCallArgAcc acc config =
    config
        { configCwd = override (callCwd acc) (configCwd config)
        }

defaultAcc :: CallArgAcc
defaultAcc = CallArgAcc
    { callCmdAndArgs = []
    , callCwd = Inherit
    }

addArgs :: [String] -> CallArgAcc -> CallArgAcc
addArgs new acc@CallArgAcc{ callCmdAndArgs } =
    acc{ callCmdAndArgs = callCmdAndArgs ++ new }

setCwd :: String -> CallArgAcc -> CallArgAcc
setCwd newCwd acc@CallArgAcc{ callCwd } =
    case callCwd of
        Inherit ->
            acc{ callCwd = Override newCwd }
        Override oldCwd ->
            error $ "Cannot set CWD to both \"" ++ oldCwd ++ "\" and \"" ++ newCwd ++ "\"."

data Inherited a
    = Inherit
    | Override a

override :: Inherited a -> a -> a
override Inherit a = a
override (Override a) _ = a

-- Variadic function
-- Can take a string or an array of strings
-- Can produce a string or a function that consumes an additional argument, depending on the context.
call :: (CallArg v, CallResult r) => v -> r
call v =
    getResult (addArg v defaultAcc)

call_ :: (CallArg v, CallNoOutputResult r) => v -> r
call_ v =
    getNoOutputResult (addArg v defaultAcc)

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
        setCwd cwd

-- Class of types that can be the output of the "call" function.
-- Since "call" is variadic, it must be able to produce different outputs in different contexts.
-- Usually the final application of "call" will produce a string, but all the previous invocations must produce another function so that the next args can be consumed.
class CallResult r where
    getResult :: CallArgAcc -> r

instance CallOutputType r => CallResult (Command r) where
    getResult acc = do
        config <- getCommandConfig
        let newConfig = (considerImplications (implications :: Implications r) . applyCallArgAcc acc) config
        liftIO $ print newConfig
        liftIO $ print (callCmdAndArgs acc)
        return (extractOutput [])

-- When the result is another function, it should add our arg to the list and then get the next result.
instance (CallArg v, CallResult r) => CallResult (v -> r) where
    getResult acc v =
        getResult (addArg v acc)

class CallNoOutputResult r where
    getNoOutputResult :: CallArgAcc -> r

instance CallNoOutputResult (Command ()) where
    getNoOutputResult acc = do
        config <- getCommandConfig
        let newConfig = applyCallArgAcc acc config
        liftIO $ print newConfig
        liftIO $ print (callCmdAndArgs acc)

instance (CallArg v, CallNoOutputResult r) => CallNoOutputResult (v -> r) where
    getNoOutputResult acc v =
        getNoOutputResult (addArg v acc)

class CallOutputType r where
    implications :: Implications r
    extractOutput :: [CallOutput] -> r

-- Uses a phantom type so that a constant of the phantom type can be part of the CallOutputType class.
newtype Implications a = Implications [Implication]

data Implication
    = CollectStdout
    | CollectStderr
    | NoAbortOnFail

data CallOutput
    = OutputStdout String
    | OutputStderr String
    | OutputExitCode Int

newtype Stdout = Stdout String
newtype Stderr = Stderr String
newtype ExitCode = ExitCode Int

instance CallOutputType Stdout where
    implications =
        Implications [CollectStdout]
    extractOutput _ =
        Stdout "Cannot yet extract real stdout"

instance CallOutputType Stderr where
    implications =
        Implications [CollectStderr]
    extractOutput _ =
        Stderr "Cannot yet extract real stderr"

instance CallOutputType ExitCode where
    implications =
        Implications [NoAbortOnFail]
    extractOutput _ =
        ExitCode 64

instance (CallOutputType a, CallOutputType b) => CallOutputType (a, b) where
    implications =
        Implications (aImpls ++ bImpls)
        where
            Implications aImpls = implications :: Implications a
            Implications bImpls = implications :: Implications b
    extractOutput outputs =
        (extractOutput outputs :: a, extractOutput outputs :: b)

instance (CallOutputType a, CallOutputType b, CallOutputType c) => CallOutputType (a, b, c) where
    implications =
        Implications (aImpls ++ bImpls ++ cImpls)
        where
            Implications aImpls = implications :: Implications a
            Implications bImpls = implications :: Implications b
            Implications cImpls = implications :: Implications c
    extractOutput outputs =
        (extractOutput outputs :: a, extractOutput outputs :: b, extractOutput outputs :: c)

considerImplications :: Implications r -> CommandConfig -> CommandConfig
considerImplications (Implications impls) config =
    foldl' consider config impls
    where
        consider :: CommandConfig -> Implication -> CommandConfig
        consider config CollectStdout =
            config{ configCollectStdout = True }
        consider config CollectStderr =
            config{ configCollectStderr = True }
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
