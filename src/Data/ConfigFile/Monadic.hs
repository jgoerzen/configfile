module Data.ConfigFile.Monadic (
  -- * Overview
  -- $overview
  
  module Reexporting,
  simpleAccess,
  interpolatingAccess,
  readfile, readhandle, readstring,
  has_section, options, has_option, items,
  set, setshow, remove_option, add_section, remove_section
) where

{- $overview
This module reexports a slightly different version of the standard API which makes it more convenient for chaining monadically.  Everywhere a 'ConfigParser' was the first argument in a function in the standard API, it is now the last.  This lets you rewrite 

> do let cp = emptyCP
>    cp <- add_section cp "sect1"
>    cp <- set cp "sect1" "opt1" "foo"
>    cp <- set cp "sect1" "opt2" "bar"
>    options cp "sect1"

as

> return emptyCP >>=
>  add_section "sect1" >>=
>  set "sect1" "opt1" "foo" >>=
>  set "sect1" "opt2" "bar" >>=
>  options "sect1"

which may be more elegant in some cases.  A future development might be to chain the 'ConfigParser' implicitly with a state monad, which would be yet more elegant.

-}

import Control.Monad.Error
import System.IO(Handle)
import Data.ConfigFile as Reexporting (SectionSpec, OptionSpec, ConfigParser(..),
                                  CPErrorData, CPError, emptyCP, Get_C(..), sections, merge, to_string)
import qualified Data.ConfigFile as C

simpleAccess ::  MonadError CPError m =>
                 SectionSpec -> OptionSpec -> ConfigParser -> m String
simpleAccess s o cp = C.simpleAccess cp s o

interpolatingAccess :: MonadError CPError m =>
                       Int ->
                       SectionSpec -> OptionSpec -> ConfigParser
                       -> m String
interpolatingAccess maxdepth s o cp = C.interpolatingAccess maxdepth cp s o

readfile :: MonadError CPError m => FilePath -> ConfigParser -> IO (m ConfigParser)
readfile fp cp = C.readfile cp fp

readhandle :: MonadError CPError m => Handle -> ConfigParser -> IO (m ConfigParser)
readhandle h cp = C.readhandle cp h

readstring ::  MonadError CPError m =>
               String -> ConfigParser -> m ConfigParser
readstring cp s = C.readstring s cp

has_section :: SectionSpec -> ConfigParser -> Bool
has_section x cp = C.has_section cp x

add_section :: MonadError CPError m =>
               SectionSpec -> ConfigParser -> m ConfigParser
add_section s cp = C.add_section cp s

options ::  MonadError CPError m =>
            SectionSpec -> ConfigParser -> m [OptionSpec]
options x cp = C.options cp x

has_option :: SectionSpec -> OptionSpec -> ConfigParser -> Bool
has_option s o cp = C.has_option cp s o

items ::  MonadError CPError m =>
          SectionSpec -> ConfigParser -> m [(OptionSpec, String)]
items s cp = C.items cp s

set ::  MonadError CPError m =>
        SectionSpec -> OptionSpec -> String -> ConfigParser -> m ConfigParser
set s passedo val cp = C.set cp s passedo val

setshow :: (Show a, MonadError CPError m) =>
           SectionSpec -> OptionSpec -> a -> ConfigParser -> m ConfigParser
setshow s o val cp = C.setshow cp s o val

remove_option ::  MonadError CPError m =>
                  SectionSpec -> OptionSpec -> ConfigParser -> m ConfigParser
remove_option s passedo cp = C.remove_option cp s passedo


remove_section ::  MonadError CPError m =>
                   SectionSpec -> ConfigParser -> m ConfigParser
remove_section s cp = C.remove_section cp s

