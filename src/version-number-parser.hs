{-
  Version Number Parser
  (C) 2019 David Lettier
  lettier.com
-}

import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe

main
  ::  IO ()
main
  = do
  putStrLn "What is the version output file path?"
  filePath   <- getLine
  text       <- readFile filePath
  let result =
        case readP_to_S (parseVersionNumber []) text of
          []      -> []
          r@(_:_) -> map readInt $ fst $ last r
  putStrLn ""
  print result

parseVersionNumber
  ::  [String]
  ->  ReadP [String]
parseVersionNumber
  nums
  = do
  _         <- parseNotNumber
  num       <- parseNumber
  let nums' = nums ++ [num]
  parseSeparator nums' parseVersionNumber

parseSeparator
  ::  [String]
  ->  ([String] -> ReadP [String])
  ->  ReadP [String]
parseSeparator
  nums
  f
  = do
  next <- look
  case next of
    ""    -> return nums
    (c:_) ->
      case c of
        '.' -> f nums
        '-' -> if length nums == 1 then f [] else f nums
        _   -> if length nums == 1 then f [] else return nums

parseNotNumber
  ::  ReadP String
parseNotNumber
  =
  munch (not . isNumber)

parseNumber
  ::  ReadP String
parseNumber
  =
  munch1 isNumber

readInt
  ::  String
  ->  Int
readInt
  =
  read
