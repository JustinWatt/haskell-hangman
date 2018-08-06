{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Effects where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Data.Function              ((&))
import qualified Data.Map.Strict            as M
import           Data.Map.Strict            (Map)
import           System.Exit                hiding (ExitCode (ExitSuccess))
import           System.Random              (randomRIO)

data Random r where
  RandomNumber :: (Int, Int) -> Random Int

randomNumber :: Member Random effs => (Int, Int) -> Eff effs Int
randomNumber = send . RandomNumber

runRandom :: LastMember IO effs => Eff (Random ': effs) a  -> Eff effs a
runRandom = interpretM (\case RandomNumber minMax -> randomRIO minMax)

runRandomPure :: forall effs a. Int -> Eff (Random ': effs) a -> Eff effs a
runRandomPure n req = interpret go req
  where
    go :: Random v -> Eff effs v
    go (RandomNumber _) = pure n

data FileSystem r where
  ReadFile :: FilePath -> FileSystem String

readFile' :: Member FileSystem effs => FilePath -> Eff effs String
readFile' = send . ReadFile

runFileSystem :: LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
runFileSystem = interpretM (\case ReadFile fp -> readFile fp)

runFileSystemPure ::
  forall effs a. Map String String
  -> Eff (FileSystem ': effs) a
  -> Eff effs a
runFileSystemPure fs req = interpret go req
  where
    go :: FileSystem v -> Eff eff v
    go (ReadFile fp) = case M.lookup fp fs of
      Nothing       -> error "file not found"

      Just contents -> pure contents

data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console effs => String -> Eff effs ()
putStrLn' = send . PutStrLn

getLine' :: Member Console effs => Eff effs String
getLine' = send GetLine

exitSuccess' :: Member Console effs => Eff effs ()
exitSuccess' = send ExitSuccess

runConsole :: forall effs a. LastMember IO effs
           => Eff (Console ': effs) a
           -> Eff effs a
runConsole = interpretM (\case
  PutStrLn msg -> putStrLn msg
  GetLine      -> getLine
  ExitSuccess  -> exitSuccess)

nextVal :: Member (State [a]) effs => String -> Eff effs a
nextVal errorMessage = get >>= \case
  []     -> error errorMessage
  (x:xs) -> put xs >> pure x

runConsolePure :: forall effs w. [String]
               -> Eff (Console ': effs) w
               -> Eff effs (Maybe w, [String], [String])
runConsolePure inputs req = do
  ((x, inputs'), output) <- reinterpret3 go req
    & runError & runState inputs & runWriter

  pure (either (const Nothing) Just x, inputs', output)
  where
    go :: Console v -> Eff (Error () ': State [String] ': Writer [String] ': effs) v
    go (PutStrLn msg) = tell [msg]
    go GetLine        = nextVal "not enough lines"
    go ExitSuccess    = throwError ()
