{-
Copyright (C) 2011 Takahiro Himura <taka@himura.jp>,
               and John MacFarlane <jgm@berkeley.edu>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.RD
   Copyright   : Copyright (C) 2011 Takahiro Himura and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Takahiro Himura <taka@himura.jp>
   Stability   : alpha
   Portability : portable

Conversion from RD-formatted text to 'Pandoc' document,
based on the implementation available at <http://github.com/uwabami/rdtool/>.

RD is known as embeddable documentation format in Ruby script file.

-}

module Text.Pandoc.Readers.RD ( readRD ) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.ParserCombinators.Parsec
import Control.Monad

-- | Read RD from an input string and return a Pandoc document.
readRD :: ParserState -- ^ Parser state, including options for parser
       -> String   -- ^ String to parse (assuming @'\n'@ line endings)
       -> Pandoc
readRD state s = (readWith parseRD) state (s ++ "\n\n")

parseRD :: GenParser Char ParserState Pandoc
parseRD = do
  updateState (\state -> state { stateParseRaw = True })
  blocks <- manyTill toplevel eof
  let doc = Pandoc (Meta [] [] []) $ filter (/= Null) blocks
  return doc

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

blockElems :: [GenParser Char ParserState Block]
blockElems =
  [ verbatim
  , headline
  , list
  , textblock
    -- , include -- FIXME
  ]

toplevel :: GenParser Char ParserState Block
toplevel = choice (headline:blockElems) <?> "block"

block :: GenParser Char ParserState Block
block = choice blockElems <?> "block"

textblock :: GenParser Char ParserState Block
textblock = many1 inline >>~ eatTrailingSpaces >>= return . Plain . normalizeSpaces

eatTrailingSpaces :: GenParser Char ParserState ()
eatTrailingSpaces = skipMany1 (try (manyTill space newline))

verbatim :: GenParser Char ParserState Block
verbatim = try $ do
  content <- indentedBlock
  return $ CodeBlock ("",[],[]) $ stripTrailingNewlines content

-- blockquote :: GenParser Char ParserState Block
-- blockquote = try $ do
--   raw <- indentedBlock
--   contents <- parseFromString parseBlocks $ raw ++ "\n\n"
--   return $ BlockQuote contents

headline :: GenParser Char ParserState Block
headline = headline' 0 '=' <|> headline' 4 '+'

headline' :: Int -> Char -> GenParser Char ParserState Block
headline' base c = try $ do
  level <- many1 (char c) >>= return . length
  skipSpaces
  text <- manyTill inline blanklines >>= return . normalizeSpaces
  return $ Header (base + level) text

list :: GenParser Char ParserState Block
list = choice [ itemList
              , enumList
              , descList
              -- , methodList -- FIXME
              ] <?> "list"

-- parses list start and returns its indent depth
itemListStart :: GenParser Char st Int
itemListStart = try $ do
  char '*'
  white <- many1 spaceChar
  return $ 1 + length white

-- parses enumerator list start and returns its attributes
enumListMarker :: GenParser Char st ListAttributes
enumListMarker = try $ do
  char '('
  itemNum <- many1 digit
  char ')'
  return $ (read itemNum, Decimal, TwoParens)

-- parse a line of a list item
listLine :: Int -> GenParser Char ParserState [Char]
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  line <- manyTill anyChar newline
  return $ line ++ "\n"

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Int -> GenParser Char ParserState [Char]
indentWith num = do
  state <- getState
  let tabStop = stateTabStop state
  if (num < tabStop)
    then count num (char ' ')
    else choice [ try (count num (char ' '))
                , try (char '\t' >> count (num - tabStop) (char ' '))
                ]

-- read a line indented by a given string
indentedLine :: String -> GenParser Char st [Char]
indentedLine indents = try $ do
  string indents
  manyTill anyChar newline

-- one or more indented lines, possibly separated by blank lines.
-- any amount of indentation will work.
indentedBlock :: GenParser Char st [Char]
indentedBlock = try $ do
  indents <- lookAhead $ many1 spaceChar
  lns <- many1 $ indentedLine indents
  optional blanklines
  return $ unlines lns

-- one or more indented lines, possibly separated by blank lines.
-- any amount of indentation will work.
indentedBlock' :: Int -> GenParser Char st [Char]
indentedBlock' depth = try $ do
  -- indents <- lookAhead $ count depth spaceChar >> many spaceChar
  indents <- lookAhead $ many spaceChar
  when (length indents < depth) $ fail "depth"
  lns <- many1 $ indentedLine indents
  optional blanklines
  return $ unlines lns

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: GenParser Char ParserState Int
            -> GenParser Char ParserState (Int, [Char])
rawListItem start = try $ do
  markerLength <- start
  firstLine <- manyTill anyChar newline
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or
-- (in compact lists) endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int -> GenParser Char ParserState [Char]
listContinuation markerLength = try $ do
  blanks <- many1 blankline
  result <- many1 (listLine markerLength)
  return $ blanks ++ concat result

listItem :: GenParser Char ParserState Int
         -> GenParser Char ParserState [Block]
listItem start = try $ do
  (markerLength, first) <- rawListItem start
  rest <- many (listContinuation markerLength)
  blanks <- choice [ try (many blankline >>~ lookAhead start),
                     many blankline ]
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may itself contain block elements
  parsed <- parseFromString parseBlocks $ concat (first:rest) ++ blanks
  updateState (\st -> st {stateParserContext = oldContext})
  return parsed

itemList :: GenParser Char ParserState Block
itemList = many1 (listItem itemListStart) >>= return . BulletList . compactify

enumListStart :: GenParser Char ParserState Int
enumListStart = do
  (_, markerLen) <- withHorizDisplacement enumListMarker
  white <- many1 spaceChar
  return $ markerLen + length white

enumList :: GenParser Char ParserState Block
enumList = do
  (start, style, delim) <- lookAhead (enumListMarker >>~ spaceChar)
  items <- many1 (listItem enumListStart)
  return $ OrderedList (start, style, delim) (compactify items)

descListStart :: GenParser Char ParserState Int
descListStart = do
  char ':'
  white <- many spaceChar
  return $ 1 + length white

descListItem :: GenParser Char ParserState ([Inline], [[Block]])
descListItem = do
  termDepth <- descListStart
  term <- manyTill anyChar newline
  content <- indentedBlock' termDepth
  parsedTerm <- parseFromString (manyTill inline eof) term
  parsedContent <- parseFromString parseBlocks content
  return $ (parsedTerm, [parsedContent])

descList :: GenParser Char ParserState Block
descList = do
  items <- many1 descListItem
  return $ DefinitionList items

--
-- inline
--

inlineElems :: [GenParser Char ParserState Inline]
inlineElems =
  [ str
  , whitespace
  , emphasis
  , code
  , var
  , keyboard
  , index
    -- , reference
  , footnote
  , verb
  , symbol
  ]

inline :: GenParser Char ParserState Inline
inline = choice inlineElems <?> "inline"

inline' :: GenParser Char ParserState Inline
inline' = choice (endline:inlineElems) <?> "inline"

whitespace :: GenParser Char ParserState Inline
whitespace = many1 spaceChar >> return Space <?> "whitespace"

-- an endline character that can be treated as a space, not a structural break
endline :: GenParser Char ParserState Inline
endline = try $ do
  newline
  notFollowedBy blankline
  return Space

specialChars :: [Char]
specialChars = "*{}|%:-'()"

str :: GenParser Char ParserState Inline
str = rawStr >>= return . Str

symbol :: GenParser Char ParserState Inline
symbol = do
  result <- oneOf specialChars
  return $ Str [result]

rawStr :: GenParser Char ParserState String
rawStr = many1 (noneOf (specialChars ++ "\t\n "))

enclosed' :: GenParser Char st t   -- ^ start parser
          -> GenParser Char st end  -- ^ end parser
          -> GenParser Char st a    -- ^ content parser (to be used repeatedly)
          -> GenParser Char st [a]
enclosed' start end parser = try $
  start >> many1Till parser end

inlineParser :: String -> String -> GenParser Char ParserState a -> GenParser Char ParserState [a]
inlineParser beg end p = enclosed' (string beg) (string end) p

emphasis :: GenParser Char ParserState Inline
emphasis = inlineParser "((*" "*))" inline' >>= return . Emph . normalizeSpaces

code' :: String -> String -> GenParser Char ParserState [String]
code' beg end = inlineParser beg end (many1Till anyChar (lookAhead (string end)))

code :: GenParser Char ParserState Inline
code = code' "(({" "}))" >>= return . Code ("",[],[]) . concat

var :: GenParser Char ParserState Inline
var = code' "((|" "|))" >>= return . Code ("",["var"],[]) . concat

keyboard :: GenParser Char ParserState Inline
keyboard = code' "((%" "%))" >>= return . Code ("",["keyboard"],[]) . concat

index :: GenParser Char ParserState Inline
index = inlineParser "((:" ":))" inline' >>= return . Strikeout

reference :: GenParser Char ParserState Inline
reference = undefined

footnote :: GenParser Char ParserState Inline
footnote = inlineParser "((-" "-))" inline' >>= return . Note . (:[]) . Plain

verb :: GenParser Char ParserState Inline
verb = code' "(('" "'))" >>= return . Code ("",["verb"],[]) . concat
