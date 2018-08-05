{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           Control.Monad       (forever, when)
import           Control.Monad.Freer
import           Data.Char           (toLower)
import           Data.List           (intersperse)
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Monoid         ((<>))
import           Effects
import           System.Exit         (exitSuccess)
import           System.Random       (randomRIO)

newtype WordList = WordList [String]
  deriving (Eq, Show)

data Puzzle =
  Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    fmap renderPuzzleChar discovered
    <> "\nGuessed so far: " <> guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

freshPuzzle :: String -> Puzzle
freshPuzzle s =
  Puzzle s discovered []
  where
    discovered = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle answer _ _) c = c `elem` answer

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle answer filledInSoFar s) c =
  Puzzle answer newFilledInSoFar (c:s)
  where
    zipper :: Eq a => a -> a -> Maybe a -> Maybe a
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      zipWith (zipper c) answer filledInSoFar

handleGuess :: Member Console eff => Puzzle -> Char -> Eff eff Puzzle
handleGuess puzzle guess = do
  putStrLn' $ "Your guess was: " <> [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn' "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn' "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn' "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Member Console eff => Puzzle -> Eff eff ()
gameOver (Puzzle answer _ guessed) =
  when (length guessed > 7) $ do
    putStrLn' "You lose!"
    putStrLn' $ "The word was: " <> answer
    exitSuccess'

gameWin :: Member Console eff => Puzzle -> Eff eff ()
gameWin (Puzzle _ filledInSoFar _) =
  when (not (null filledInSoFar) && all isJust filledInSoFar) $ do
    putStrLn' "You Win!"
    exitSuccess'

allWords :: Member FileSystem eff => Eff eff WordList
allWords = do
  dict <- readFile' "data/dict.txt"
  return (WordList $ lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

randomWord :: Member Random eff => WordList -> Eff eff String
randomWord (WordList wl) = do
  randomIndex <- randomNumber (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: Members '[FileSystem, Random] eff => Eff eff String
randomWord' = gameWords >>= randomWord

gameWords :: Member FileSystem eff => Eff eff WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let
        l = length (w :: String)
      in
           l >= minWordLength
        && l <  maxWordLength

runGame :: Member Console eff => Puzzle -> Eff eff ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn' $ "Current puzzle is: " <> show puzzle
  putStrLn' "Guess a letter: "
  guess <- getLine'
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn' "Your guess must be a single character"

main :: IO ()
main = runM $ (runConsole (runFileSystem (runRandom mainGame)))

mainGame :: Eff '[Random, FileSystem, Console, IO] ()
mainGame = do
  word <- randomWord'
  let puzzle = freshPuzzle (toLower <$> word)
  runGame puzzle
