{-
  SRT File Parser
  (C) 2019 David Lettier
  lettier.com
-}

{-# LANGUAGE
    NamedFieldPuns
#-}

import Control.Applicative ((<|>))
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe

type TagAttribute = (String, String)

data Tag =
  Tag
    { name       :: String
    , attributes :: [TagAttribute]
    }
  deriving (Show, Read)

data TaggedText =
  TaggedText
    { text :: String
    , tags :: [Tag]
    }
  deriving (Show, Read)

data Timestamp =
  Timestamp
    { hours        :: Int
    , minutes      :: Int
    , seconds      :: Int
    , milliseconds :: Int
    }
  deriving (Show, Read)

data SrtSubtitleCoordinates =
  SrtSubtitleCoordinates
    { x1 :: Int
    , x2 :: Int
    , y1 :: Int
    , y2 :: Int
    }
  deriving (Show, Read)

data SrtSubtitle =
  SrtSubtitle
    { index       :: Int
    , start       :: Timestamp
    , end         :: Timestamp
    , coordinates :: Maybe SrtSubtitleCoordinates
    , taggedText  :: [TaggedText]
    }
  deriving (Show, Read)

main
  ::  IO ()
main
  = do
  putStrLn "What is the SRT file path?"
  filePath   <- getLine
  text       <- readFile filePath
  let result =
        case readP_to_S parseSrt text of
          []      -> []
          r@(_:_) -> fst $ last r
  putStrLn ""
  print result

parseSrt
  ::  ReadP [SrtSubtitle]
parseSrt
  =
  manyTill parseBlock (skipSpaces >> eof)

parseBlock
  ::  ReadP SrtSubtitle
parseBlock
  = do
  i      <- parseIndex
  (s, e) <- parseTimestamps
  c      <- parseCoordinates
  t      <- parseTextLines
  return
    SrtSubtitle
      { index       = i
      , start       = s
      , end         = e
      , coordinates = c
      , taggedText  = t
      }

parseBlock'
  ::  ReadP SrtSubtitle
parseBlock'
  =
      SrtSubtitle
  <$> parseIndex
  <*> parseStartTimestamp
  <*> parseEndTimestamp
  <*> parseCoordinates
  <*> parseTextLines

parseIndex
  ::  ReadP Int
parseIndex
  =
      skipSpaces
  >>  readInt <$> parseNumber

parseTimestamps
  ::  ReadP (Timestamp, Timestamp)
parseTimestamps
  = do
  _   <- char '\n'
  s   <- parseTimestamp
  _   <- skipSpaces
  _   <- string "-->"
  _   <- skipSpaces
  e   <- parseTimestamp
  return (s, e)

parseStartTimestamp
  ::  ReadP Timestamp
parseStartTimestamp
  =
      char '\n'
  >>  parseTimestamp

parseEndTimestamp
  ::  ReadP Timestamp
parseEndTimestamp
  =
      skipSpaces
  >>  string "-->"
  >>  skipSpaces
  >>  parseTimestamp

parseTimestamp
  ::  ReadP Timestamp
parseTimestamp
  = do
  h  <- parseNumber
  _  <- char ':'
  m  <- parseNumber
  _  <- char ':'
  s  <- parseNumber
  _  <- char ',' <|> char '.'
  m' <- parseNumber
  return
    Timestamp
      { hours        = readInt h
      , minutes      = readInt m
      , seconds      = readInt s
      , milliseconds = readInt m'
      }

parseCoordinates
  ::  ReadP (Maybe SrtSubtitleCoordinates)
parseCoordinates
  =
  option Nothing $ do
    _  <- skipSpaces1
    x1 <- parseCoordinate 'x' 1
    _  <- skipSpaces1
    x2 <- parseCoordinate 'x' 2
    _  <- skipSpaces1
    y1 <- parseCoordinate 'y' 1
    _  <- skipSpaces1
    y2 <- parseCoordinate 'y' 2
    return
      $ Just
        SrtSubtitleCoordinates
          { x1 = readInt x1
          , x2 = readInt x2
          , y1 = readInt y1
          , y2 = readInt y2
          }

parseCoordinate
  ::  Char
  ->  Int
  ->  ReadP String
parseCoordinate
  c
  n
  = do
  _  <- char (Data.Char.toUpper c) <|> char (Data.Char.toLower c)
  _  <- string $ show n ++ ":"
  parseNumber

parseTextLines
  ::  ReadP [TaggedText]
parseTextLines
  =
      char '\n'
  >>  (getTaggedText <$> manyTill parseAny parseEndOfTextLines)

getTaggedText
  ::  String
  ->  [TaggedText]
getTaggedText
  s
  =
  fst
    $ foldl
      folder
      ([], [])
      parsed
  where
    parsed
      ::  [String]
    parsed
      =
      case readP_to_S (parseTaggedText []) s of
        []      -> [s]
        r@(_:_) -> (fst . last) r
    folder
      ::  ([TaggedText], [Tag])
      ->  String
      ->  ([TaggedText], [Tag])
    folder
      (tt, t)
      x
      | isTag x   = (tt, updateTags t x)
      | otherwise = (tt ++ [TaggedText { text = x, tags = t}], t)

updateTags
  ::  [Tag]
  ->  String
  ->  [Tag]
updateTags
  tags
  x
  | isClosingTag x = remove compare' tags (makeTag x)
  | isOpeningTag x = add    compare' tags (makeTag x)
  | otherwise      = tags
  where
    compare'
      ::  Tag
      ->  Tag
      ->  Bool
    compare'
      a
      b
      =
      name a /= name b

makeTag
  ::  String
  ->  Tag
makeTag
  s
  =
  Tag
    { name       = getTagName       s
    , attributes = getTagAttributes s
    }

parseEndOfTextLines
  ::  ReadP ()
parseEndOfTextLines
  =
  void (string "\n\n") <|> eof

parseTaggedText
  ::  [String]
  ->  ReadP [String]
parseTaggedText
  strings
  = do
  s <- look
  case s of
    "" -> return strings
    _  -> do
      r <- munch1 (/= '<') <++ parseClosingTag <++ parseOpeningTag
      parseTaggedText $ strings ++ [r]

parseOpeningTag
  ::  ReadP String
parseOpeningTag
  = do
  _ <- char '<'
  t <- munch1 (\ c -> c /= '/' && c /= '>')
  _ <- char '>'
  return $ "<" ++ t ++ ">"

parseClosingTag
  ::  ReadP String
parseClosingTag
  = do
  _ <- char '<'
  _ <- char '/'
  t <- munch1 (/= '>')
  _ <- char '>'
  return $ "</" ++ t ++ ">"

getTagAttributes
  ::  String
  ->  [TagAttribute]
getTagAttributes
  s
  =
  if isOpeningTag s
    then
      case readP_to_S (parseTagAttributes []) s of
        []    -> []
        (x:_) -> fst x
    else
      []

getTagName
  ::  String
  ->  String
getTagName
  s
  =
  case readP_to_S parseTagName s of
    []    -> ""
    (x:_) -> toLower' $ fst x

parseTagName
  ::  ReadP String
parseTagName
  = do
  _ <- char '<'
  _ <- munch (== '/')
  _ <- skipSpaces
  n <- munch1 (\ c -> c /= ' ' && c /= '>')
  _ <- munch  (/= '>')
  _ <- char '>'
  return n

parseTagAttributes
  ::  [TagAttribute]
  ->  ReadP [TagAttribute]
parseTagAttributes
  tagAttributes
  = do
  s <- look
  case s of
    "" -> return tagAttributes
    _  -> do
      let h = head s
      case h of
        '>' -> return tagAttributes
        '<' -> trimTagname >> parseTagAttributes'
        _   -> parseTagAttributes'
  where
    parseTagAttributes'
      ::  ReadP [TagAttribute]
    parseTagAttributes'
      = do
      tagAttribute <- parseTagAttribute
      parseTagAttributes
        ( add
            (\ a b -> fst a /= fst b)
            tagAttributes
            tagAttribute
        )

trimTagname
  :: ReadP ()
trimTagname
  =
      char '<'
  >> skipSpaces
  >> munch1 (\ c -> c /= ' ' && c /= '>')
  >> return ()

parseTagAttribute
  ::  ReadP TagAttribute
parseTagAttribute
  = do
  _ <- skipSpaces
  k <- munch1 (/= '=')
  _ <- string "=\""
  v <- munch1 (/= '\"')
  _ <- char '\"'
  _ <- skipSpaces
  return (toLower' k, v)

parseAny
  ::  ReadP Char
parseAny
  =
  satisfy (const True)

parseNumber
  ::  ReadP String
parseNumber
  =
  munch1 isNumber

skipSpaces1
  ::  ReadP ()
skipSpaces1
  =
  void $ skipMany1 (char ' ')

isTag
  ::  String
  ->  Bool
isTag
  s
  =
  isOpeningTag s || isClosingTag s

isOpeningTag
  ::  String
  ->  Bool
isOpeningTag
  s
  =
  isPresent $ readP_to_S parseOpeningTag s

isClosingTag
  ::  String
  ->  Bool
isClosingTag
  s
  =
  isPresent $ readP_to_S parseClosingTag s

readInt
  ::  String
  ->  Int
readInt
  =
  read

toLower'
  ::  String
  ->  String
toLower'
  =
  map toLower

remove
  ::  (a -> a -> Bool)
  ->  [a]
  ->  a
  ->  [a]
remove
  f
  xs
  x
  =
  filter
    (f x)
    xs

add
  ::  (a -> a -> Bool)
  ->  [a]
  ->  a
  ->  [a]
add
  f
  xs
  x
  | isPresent xs = remove f xs x ++ [x]
  | otherwise    = [x]

isPresent
  ::  Foldable t
  =>  t a
  ->  Bool
isPresent
  =
  not . null
