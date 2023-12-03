module Main where

import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.List (find, findIndex, isPrefixOf, tails, elemIndex, sortBy, reverse)
import Data.Function (on)
import Data.Char (isDigit, digitToInt)
import Data.Maybe(fromJust, isNothing, fromMaybe)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input    = [String]  -- default to Bytestring, but very likely you'll need to change it
type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser string =  lines string

combineInts :: (Int, Int) -> Int
combineInts (x, y) = case x of
                        0 -> y + 0
                        _ -> 10 * x + y

findInts :: String -> (Int, Int)
findInts string = (first, second)
                  where first = case find isDigit string of
                                  Just char -> digitToInt char
                                  _ -> 0
                        second = case find isDigit $ reverse string of
                                   Just char -> digitToInt char
                                   _ -> 0


findString :: (Eq a) => [a] -> [a] -> (Maybe[a], Maybe Int)
findString search str = (found, index)
                        where index = findIndex (isPrefixOf search) (tails str)
                              found = case index of
                                        Just i -> Just search
                                        Nothing -> Nothing

findStringNumber ::  String ->  [String] -> (Maybe String, Maybe Int)
findStringNumber string nums = (value, index)
                           where
                                 stuff = map (\n -> findString n string) nums
                                 tuples = filter (\(str, idx) -> not $ isNothing idx) stuff
                                 (value, index) = case sortBy (compare `on` snd) tuples of
                                                    [] -> (Nothing, Nothing)
                                                    sortedTuples -> head sortedTuples


findCharNum :: String -> (Maybe Char, Maybe Int)
findCharNum string = (firstDigit, index)
                     where
                       firstDigit = find isDigit string
                       index = case firstDigit of
                                 Just d -> elemIndex d string
                                 Nothing -> Nothing


--zipCharAndString :: String -> [((Char, Maybe Int), (String, Maybe Int))]
--zipCharAndString = zip chars strings
                   --where chars = map findChar string
--                         strings = map findString

hasNumberWord :: String -> Int
hasNumberWord string = case string of
                         "one" -> 1
                         "two" -> 2
                         "three" -> 3
                         "four" -> 4
                         "five" -> 5
                         "six" -> 6
                         "seven" -> 7
                         "eight" -> 8
                         "nine" -> 9

reversedHasNumberWord :: String -> Int
reversedHasNumberWord string = case string of
                                 "eno" -> 1
                                 "owt" -> 2
                                 "eerht" -> 3
                                 "ruof" -> 4
                                 "evif" -> 5
                                 "xis" -> 6
                                 "neves" -> 7
                                 "thgie" -> 8
                                 "enin" -> 9

determineNumber :: ((Maybe String, Maybe Int), (Maybe Char, Maybe Int))-> (String -> Int)-> Int
determineNumber ((Just word, Just wordIndex), (Just char, Just charIndex))f = if wordIndex < charIndex
                                                                 then f word
                                                                 else digitToInt char

determineNumber ( (_, Nothing), (Just char, Just charIndex))f = digitToInt char
determineNumber ((Just word, Just wordIndex), (_, Nothing))f = f word
determineNumber ((_, Nothing), (_, Nothing))f = 0


-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 strings = sum $ map ( combineInts . findInts ) strings

-- | The function which calculates the solution for part two

solve2 strings = sum total
                 where
                   nums =  ["one", "two", "three", "four", "five", "six", "seven", "eight","nine"]
                   listOfChars = map findCharNum strings
                   listOfStrings = map (\string -> findStringNumber string nums) $ strings
                   listOfCharStrings = zip listOfStrings listOfChars
                   reversedStrings = map reverse strings
                   reversedListOfChars = map findCharNum reversedStrings
                   reversedListOfStrings = map (\string -> findStringNumber string (map reverse nums)) reversedStrings
                   reversedListOfCharStrings = zip reversedListOfStrings reversedListOfChars
                   firstNums = map (*10) $map (\tuple -> determineNumber tuple hasNumberWord ) listOfCharStrings
                   secondNums = map (\tuple -> determineNumber tuple reversedHasNumberWord ) reversedListOfCharStrings
                   total = map (\(x,y) -> x + y )$ zip firstNums secondNums
                 --where numbers = findStringNumber strings
                       --chars = findChars strings

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath -- use parser <$> readFile filepath if String is better
  print input
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
