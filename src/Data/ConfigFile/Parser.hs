{-
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify it, as
specified in the COPYRIGHT file, under the terms of either version 2.1 of
the LGPL (or, at your option, any later version) or the 3-clause BSD license.

-}

{- |
   Module     : Data.ConfigFile.Parser
   Copyright  : Copyright (C) 2004-2008 John Goerzen
   License    : Either LGPL or BSD3, as specified in the COPYRIGHT file.

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Parser support for "Data.ConfigFile".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004-2008 John Goerzen, jgoerzen\@complete.org

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Data.ConfigFile.Parser
(
 parse_string, parse_file, parse_handle, interpmain, ParseOutput
       --satisfyG,
       --main
) where
import Text.ParserCombinators.Parsec
import Control.Monad.Error(throwError, MonadError)
import Data.String.Utils
import Data.ConfigFile.Lexer
import System.IO(Handle, hGetContents)
import Text.ParserCombinators.Parsec.Utils
import Data.ConfigFile.Types

----------------------------------------------------------------------
-- Exported funcs
----------------------------------------------------------------------

parse_string :: MonadError CPError m =>
                String -> m ParseOutput
parse_string s =
    detokenize "(string)" $ parse loken "(string)" s

--parse_file :: FilePath -> IO (CPResult ParseOutput)
parse_file :: MonadError CPError m => FilePath -> IO (m ParseOutput)
parse_file f =
    do o <- parseFromFile loken f
       return $ detokenize f o

--parse_handle :: Handle -> IO (CPResult ParseOutput)
parse_handle :: MonadError CPError m => Handle -> IO (m ParseOutput)
parse_handle h =
    do s <- hGetContents h
       let o = parse loken (show h) s
       return $ detokenize (show h) o

----------------------------------------------------------------------
-- Private funcs
----------------------------------------------------------------------
detokenize :: (Show t, MonadError (CPErrorData, [Char]) m) => SourceName
           -> Either t [GeneralizedToken CPTok]
           -> m ParseOutput
detokenize fp l =
    let conv msg (Left err) = throwError $ (ParseError (show err), msg)
        conv _ (Right val) = return val
        in do r <- conv "lexer" l
              conv "parser" $ runParser main () fp r

main :: GeneralizedTokenParser CPTok () ParseOutput
main =
    do {s <- sectionlist; return s}
    <|> try (do
             o <- optionlist
             s <- sectionlist
             return $ ("DEFAULT", o) : s
            )
    <|> do {o <- optionlist; return $ [("DEFAULT", o)] }
    <?> "Error parsing config file tokens"

sectionlist :: GeneralizedTokenParser CPTok () ParseOutput
sectionlist = do {eof; return []}
              <|> try (do
                       s <- sectionhead
                       eof
                       return [(s, [])]
                      )
              <|> do
                  s <- section
                  sl <- sectionlist
                  return (s : sl)

section :: GeneralizedTokenParser CPTok () (String, [(String, String)])
section = do {sh <- sectionhead; ol <- optionlist; return (sh, ol)}

sectionhead :: GeneralizedTokenParser CPTok () String
sectionhead =
    let wf (NEWSECTION x) = Just x
        wf _ = Nothing
        in
        do {s <- tokeng wf; return $ strip s}

optionlist :: GeneralizedTokenParser CPTok () [(String, String)]
optionlist = many coption

coption :: GeneralizedTokenParser CPTok () (String, String)
coption =
    let wf (NEWOPTION x) = Just x
        wf _ = Nothing
        wfx (EXTENSIONLINE x) = Just x
        wfx _ = Nothing
        in
        do o <- tokeng wf
           l <- many $ tokeng wfx
           return (strip (fst o), valmerge ((snd o) : l))

valmerge :: [String] -> String
valmerge vallist =
    let vl2 = map strip vallist
        in join "\n" vl2

----------------------------------------------------------------------
-- Interpolation
----------------------------------------------------------------------

interpval :: Parser String
interpval  = do
            string "%("
            s <- (many1 $ noneOf ")") <?> "interpolation name"
            string ")s"               <?> "end of interpolation name"
            return s

percentval :: Parser String
percentval = do
             string "%%"
             return "%"

interpother :: Parser String
interpother = do
              c <- noneOf "%"
              return [c]

interptok :: (String -> Either CPError String) -> Parser String
interptok lookupfunc = (try percentval)
                       <|> interpother
                       <|> do s <- interpval
                              case lookupfunc s of
                                 Left (InterpolationError x, _) -> fail x
                                 Left _ -> fail $ "unresolvable interpolation reference to \"" ++ s ++ "\""
                                 Right x -> return x


interpmain :: (String -> Either CPError String) -> Parser String
interpmain lookupfunc =
    do r <- manyTill (interptok lookupfunc) eof
       return $ concat r
