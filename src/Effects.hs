{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

module Effects where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           System.Exit                hiding (ExitCode (ExitSuccess))
import Data.Map.Strict as M
import Data.Map.Strict (Map)
import           System.Random       (randomRIO)

data Random r where
  RandomNumber :: (Int, Int) -> Random Int

randomNumber :: Member Random effs => (Int, Int) -> Eff effs Int
randomNumber = send . RandomNumber

runRandom :: LastMember IO effs => Eff (Random ': effs) a  -> Eff effs a
runRandom = interpretM (\case RandomNumber minMax -> randomRIO minMax)

runRandomPure :: Int -> Eff '[Random] Int -> Int
runRandomPure n req = run $ interpret go req
  where
    go :: Random v -> Eff '[] v
    go (RandomNumber _) = pure n

data FileSystem r where
  ReadFile :: FilePath -> FileSystem String

readFile' :: Member FileSystem effs => FilePath -> Eff effs String
readFile' = send . ReadFile

runFileSystem :: LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
runFileSystem = interpretM (\case ReadFile fp -> readFile fp)

runFileSystemPure :: Map String String -> Eff '[FileSystem] String -> String
runFileSystemPure fs req = run $ interpret go req
  where
    go :: FileSystem v -> Eff '[] v
    go (ReadFile fp) = case M.lookup fp fs of
      Nothing -> error "file not found"
      Just contents -> pure contents

data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console()

putStrLn' :: Member Console effs => String -> Eff effs ()
putStrLn' = send . PutStrLn

getLine' :: Member Console effs => Eff effs String
getLine' = send GetLine

exitSuccess' :: Member Console effs => Eff effs ()
exitSuccess' = send ExitSuccess

runConsole :: LastMember IO effs => Eff (Console ': effs) a -> Eff effs a
runConsole = interpretM (\case
  PutStrLn msg -> putStrLn msg
  GetLine      -> getLine
  ExitSuccess  -> exitSuccess)

runConsolePure :: [String] -> Eff '[Console] w -> [String]
runConsolePure inputs req = snd . fst $
    run (runWriter (runState inputs (runError (reinterpret3 go req))))
  where
    go :: Console v -> Eff '[Error (), State [String], Writer [String]] v
    go (PutStrLn msg) = tell [msg]
    go GetLine        = get >>= \case
      []     -> error "not enough lines"
      (x:xs) -> put xs >> pure x
    go ExitSuccess    = throwError ()
