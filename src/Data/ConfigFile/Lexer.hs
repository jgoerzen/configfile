{- arch-tag: ConfigParser lexer support
Copyright (C) 2004, 2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify it, as
specified in the COPYRIGHT file, under the terms of either version 2.1 of
the LGPL (or, at your option, any later version) or the 3-clause BSD license.
-}

{- |
   Module     : Data.ConfigFile.Lexer
   Copyright  : Copyright (C) 2004, 2008 John Goerzen
   License    : Either LGPL or BSD3, as specified in the COPYRIGHT file.

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Lexer support for "Data.ConfigFile".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004, 2008 John Goerzen, jgoerzen\@complete.org

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}
module Data.ConfigFile.Lexer
(
       -- -- * Temporary for testing
       --comment_chars, eol, optionsep, whitespace_chars, comment_line,
       --empty_line, sectheader_chars, sectheader, oname_chars, value_chars,
       --extension_line, optionkey, optionvalue, optionpair
       loken,
       CPTok(..)
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Utils

data CPTok = IGNOREDATA
           | NEWSECTION String
           | NEWSECTION_EOF String
           | EXTENSIONLINE String
           | NEWOPTION (String, String)
             deriving (Eq, Show, Ord)

comment_chars :: CharParser st Char
comment_chars = oneOf "#;"
eol :: GenParser Char st String
eol = string "\n" <|> string "\r\n" <|> string "\r" <?> "End of line"
eoleof :: GenParser Char st ()
eoleof = eof <|> do {eol; return ()}
optionsep :: GenParser Char st Char
optionsep = oneOf ":=" <?> "option separator"
whitespace_chars :: GenParser Char st Char
whitespace_chars = oneOf " \t" <?> "whitespace"
comment_line :: GenParser Char st ()
comment_line = do skipMany whitespace_chars
                  comment_chars             <?> "start of comment"
                  (many $ noneOf "\r\n")   <?> "content of comment"
                  eoleof
eolstuff :: GenParser Char st ()
eolstuff = (try comment_line) <|> (try empty_line)
empty_line :: GenParser Char st ()
empty_line = do many whitespace_chars
                eoleof
             <?> "empty line"
sectheader_chars :: CharParser st Char
sectheader_chars = noneOf "]\r\n"
sectheader :: GenParser Char st String
sectheader = do char '['
                sname <- many1 $ sectheader_chars
                char ']'
                eolstuff
                return sname
             <?> "start of section"
oname_chars :: CharParser st Char
oname_chars = noneOf ":=\r\n"
value_chars :: CharParser st Char
value_chars = noneOf "\r\n"
extension_line :: GenParser Char st String
extension_line = do many1 whitespace_chars
                    c1 <- noneOf "\r\n#;"
                    remainder <- many value_chars
                    eolstuff
                    return (c1 : remainder)

optionkey, optionvalue :: GenParser Char st String
optionkey = many1 oname_chars
optionvalue = many value_chars
optionpair :: GenParser Char st (String, String)
optionpair = do key <- optionkey
                optionsep
                value <- optionvalue
                eolstuff
                return (key, value)
             <?> "key/value option"

iloken :: Parser (GeneralizedToken CPTok)
iloken =
    -- Ignore these things
    try (do {comment_line; togtok $ IGNOREDATA})
    <|> try (do {empty_line; togtok $ IGNOREDATA})

    -- Real stuff
    <|> (do {sname <- sectheader; togtok $ NEWSECTION sname})
    <|> try (do {extension <- extension_line; togtok $ EXTENSIONLINE extension})
    <|> try (do {pair <- optionpair; togtok $ NEWOPTION pair})
--    <?> "Invalid syntax in configuration file"

loken :: Parser [GeneralizedToken CPTok]
loken = filter ((/= IGNOREDATA) . snd) <$> manyTill iloken eof
