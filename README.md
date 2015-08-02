Welcome to ConfigFile.

For more information, visit http://software.complete.org/configfile

# Data.ConfigFile

Configuration file parsing, generation, and manipulation

Copyright (c) 2004-2008 John Goerzen, jgoerzen@complete.org

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This module contains extensive documentation. Please scroll down to the Introduction section to continue reading.

### Contents

* [Introduction](#introduction)
  * [Features](#features)
  * [History](#history)
* [Configuration File Format](#configuration_file)
  * [White Space](#white_space)
  * [Comments](#comments)
  * [Case Sensitivity](#case_sensitivity)
  * [Interpolation](#interpolation)
* [Usage Examples](#usage_examples)
  * [Non-Monadic Usage](#non-monadic_usage)
  * [Error Monad Usage](#error_monad_usage)
  * [Combined Error/IO Monad Usage](#combined_error_io_monad_usage)
* [Types](#types)
* [Initialization](#initialization)
* [Configuring the ConfigParser](#configuring_the_configparser)
  * [Access Functions](#access_functions)
* [Reading](#reading)
* [Accessing Data](#accessing_data)
* [Modifying Data](#modifying_data)
* [Output Data](#output_data)


## <a name="introduction"></a>Introduction

Many programs need configuration files. These configuration files are typically used to configure
certain runtime behaviors that need to be saved across sessions. Various different configuration
file formats exist.

The ConfigParser module attempts to define a standard format that is easy for the user to edit,
easy for the programmer to work with, yet remains powerful and flexible.

## <a name="features"></a>Features

For the programmer, this module provides:

* Simple calls to both read and write configuration files
* Call that can generate a string version of a file that is re-parsable by this module (useful for,
for instance, sending the file down a network)
* Segmented configuration files that let you separate configuration into distinct sections, each with
its own namespace. This can be used to configure multiple modules in one file, to configure multiple
instances of a single object, etc.
* On-the-fly parsing of integer, boolean, float, multi-line string values, and anything else Haskell's
  read can deal with
* It is possible to make a configuration file parsable by this module, the Unix shell, and/or Unix make,
though some feautres are, of course, not compatible with these other tools.
* Syntax checking with error reporting including line numbers
* Implemented in pure Haskell. No dependencies on modules outside the standard library distributed with
Haskell compilers or interpreters. All calls except those that read directly from a handle are
pure calls and can be used outside the IO monad.
* Comprehensive documentation
* Extensible API
* Complete compatibility with Python's ConfigParser module, or my ConfigParser module for OCaml,
part of my MissingLib package.

For the user, this module provides:

* Easily human-editable configuration files with a clear, concise, and consistent format
* Configuration file format consistent with other familiar formats (/etc/passwd is a valid
ConfigParser file)
* No need to understand semantics of markup languages like XML

## <a name="history"></a>History

This module is based on Python's [ConfigParser][] module.
I had earlier developed an OCaml implementation as part of my MissingLib library at
gopher://gopher.quux.org/devel/missinglib.

[configparser]: http://www.python.org/doc/current/lib/module-ConfigParser.html

While the API of these three modules is similar, and the aim is to preserve all useful
features of the original Python module, there are some differences in the implementation
details. This module is a complete, clean re-implementation in Haskell, not a Haskell
translation of a Python program. As such, the feature set is slightly different.

## <a name="configuration_file"></a>Configuration File Format

The basic configuration file format resembles that of an old-style Windows .INI file.
Here are two samples:

```
debug = yes
inputfile = /etc/passwd
names = Peter, Paul, Mary, George, Abrahaham, John, Bill, Gerald, Richard,
        Franklin, Woodrow
color = red
```

This defines a file without any explicit section, so all items will occur within
the default section DEFAULT. The debug option can be read as a boolean or a string.
The remaining items can be read as a string only.
The names entry spans two lines -- any line starting with whitespace, and containing
something other than whitespace or comments, is taken as a continuation of
the previous line.

Here's another example:
```
# Default options
[DEFAULT]
hostname: localhost
# Options for the first file
[file1]
location: /usr/local
user: Fred
uid: 1000
optionaltext: Hello, this  entire string is included
[file2]
location: /opt
user: Fred
uid: 1001
```

This file defines three sections. The DEFAULT section specifies an entry hostname.
If you attempt to read the hostname option in any section, and that section doesn't
define hostname, you will get the value from DEFAULT instead. This is a nice time-saver.
You can also note that you can use colons instead of the = character to separate
option names from option entries.

## <a name="white_space"></a>White Space

Whitespace (spaces, tabs, etc) is automatically stripped from the beginning and
end of all strings. Thus, users can insert whitespace before/after the colon or
equal sign if they like, and it will be automatically stripped.

Blank lines or lines consisting solely of whitespace are ignored.

A line giving an option or a section name may not begin with white space.
This requirement is necessary so there is no ambiguity between such lines and
continuation lines for multi-line options.

## <a name="comments"></a>Comments

Comments are introduced with the pound sign # or the semicolon ;. They cause
the parser to ignore everything from that character to the end of the line.

Comments may not occur within the definitions of options; that is, you may
not place a comment in the middle of a line such as user: Fred. That is because
the parser considers the comment characters part of the string; otherwise,
you'd be unable to use those characters in your strings. You can, however,
"comment out" options by putting the comment character at the start of the line.

## <a name="case_sensitivity"></a>Case Sensitivity

By default, section names are case-sensitive but option names are not.
The latter can be adjusted by adjusting optionxform.

## <a name="interpolation"></a>Interpolation

Interpolation is an optional feature, disabled by default. If you replace
the default accessfunc (simpleAccess) with interpolatingAccess, then you
get interpolation support with get and the other get-based functions.

As an example, consider the following file:

```
arch = i386
project = test
filename = test_%(arch)s.c
dir = /usr/src/%(filename)s
percent = 5%%
```

With interpolation, you would get these results:

```haskell
get cp "DEFAULT" "filename" -> "test_i386.c"
get cp "DEFAULT" "dir" -> "/usr/src/test_i386.c"
get cp "DEFAULT" "percent" -> "5%"
```

For more details on interpolation, please see the documentation for the interpolatingAccess
function.

## <a name="usage_examples"></a>Usage Examples

The basic theory of working with ConfigParser is this:

1. Parse or build a ConfigParser object
2. Work with it in one of several ways
3. To make changes, you discard the original object and use a new one.
Changes can be chained through one of several monads.

The default ConfigParser object that you always start with is emptyCP. From here,
you load data into it (merging data into the empty object), set up structures
yourself, or adjust options.

Let's take a look at some basic use cases.

## <a name="non-monadic_usage"></a>Non-Monadic Usage

You'll notice that many functions in this module return a `MonadError CPError` over
some type. Although its definition is not this simple, you can consider this
to be the same as returning `Either CPError a`.

That is, these functions will return `Left error` if there's a problem or
`Right result` if things are fine. The documentation for individual functions describes
the specific circumstances in which an error may occur in more detail.

Some people find it annoying to have to deal with errors manually. You can transform
errors into exceptions in your code by using forceEither. Here's an example of this
style of programming:

```haskell
 import Data.Either.Utils
 do
    val <- readfile emptyCP "/etc/foo.cfg"
    let cp = forceEither val
    putStrLn "Your setting is:"
    putStrLn $ forceEither $ get cp "sect1" "opt1"
```

In short, you can just put `forceEither $` in front of every call that returns something
that is a `MonadError`. This is still a pure functional call, so it can be used outside
of the IO monads. The exception, however, can only be caught in the IO monad.

If you don't want to bother with forceEither, you can use the error monad. It's simple
and better... read on.

## <a name="error_monad_usage"></a>Error Monad Usage

The return type is actually defined in terms of the Error monad, which is itself based
on the Either data type.

Here's a neat example of chaining together calls to build up a ConfigParser object:

```haskell
do let cp = emptyCP
   cp <- add_section cp "sect1"
   cp <- set cp "sect1" "opt1" "foo"
   cp <- set cp "sect1" "opt2" "bar"
   options cp "sect1"
```

The return value of this little snippet is `Right ["opt1", "opt2"]`.
(Note to beginners: unlike the IO monad, you can escape from the Error monad.)

Although it's not obvious, there actually was error checking there. If any of those
calls would have generated an error, processing would have stopped immediately
and a Left value would have been returned. Consider this example:

```haskell
do let cp = emptyCP
   cp <- add_section cp "sect1"
   cp <- set cp "sect1" "opt1" "foo"
   cp <- set cp "sect2" "opt2" "bar"
   options cp "sect1"
```

The return value from this is `Left (NoSection "sect2", "set")`. The second call
to set failed, so the final call was skipped, and the result of the entire
computation was considered to be an error.

You can combine this with the non-monadic style to get a final, pure value out of it:

```haskell
forceEither $ do let cp = emptyCP
                 cp <- add_section cp "sect1"
                 cp <- set cp "sect1" "opt1" "foo"
                 cp <- set cp "sect1" "opt2" "bar"
                 options cp "sect1"
```

This returns `["opt1", "opt2"]`. A quite normal value.

## <a name="combined_error_io_monad_usage"></a>Combined Error/IO Monad Usage

You've seen a nice way to use this module in the Error monad and get an Either value out.
But that's the Error monad, so IO is not permitted. Using Haskell's monad transformers,
you can run it in the combined Error/IO monad. That is, you will get an IO result back.
Here is a full standalone example of doing that:

```haskell
import Data.ConfigFile
import Control.Monad.Error

main = do
          rv <- runErrorT $
              do
              cp <- join $ liftIO $ readfile emptyCP "/etc/passwd"
              let x = cp
              liftIO $ putStrLn "In the test"
              nb <- get x "DEFAULT" "nobody"
              liftIO $ putStrLn nb
              foo <- get x "DEFAULT" "foo"
              liftIO $ putStrLn foo
              return "done"
          print rv
```

On my system, this prints:

```
In the test
x:65534:65534:nobody:/nonexistent:/bin/sh
Left (NoOption "foo","get")
```

That is, my /etc/passwd file contains a nobody user but not a foo user.

Let's look at how that works.

First, main always runs in the IO monad only, so we take the result
from the later calls and put it in rv. Note that the combined block
is started with `runErrorT $` do instead of just do.

To get something out of the call to readfile, we use `join $ liftIO $ readfile`.
This will bring the result out of the IO monad into the combined monad and
process it like usual. From here on, everything looks normal, except for IO calls.
They are all executed under `liftIO` so that the result value is properly
brought into the combined monad. This finally returns "done". Since we are
in the Error monad, that means that the literal value is Right "done".
Since we are also in the IO monad, this is wrapped in IO. So the final return
type after applying `runErrorT` is `IO (Either CPError String)`.

In this case, there was an error, and processing stopped at that point just like
the example of the pure Error monad. We print out the return value, so you see
the error displayed as a Left value.

It all works quite easily.

## <a name="types"></a>Types

The code used to say this:

```haskell
type CPResult a = MonadError CPError m => m a
simpleAccess :: ConfigParser -> SectionSpec -> OptionSpec -> CPResult String
```
But Hugs did not support that type declaration. Therefore, types are now given like this:

```haskell
simpleAccess :: MonadError CPError m =>
                ConfigParser -> SectionSpec -> OptionSpec -> m String
```

Although it looks more confusing than before, it still means the same. The return value
can still be treated as `Either CPError String` if you so desire.

See [Types.hs](src/Data/ConfigFile/Types.hs) for more extensive documentation.

## <a name="initialization"></a>Initialization

```haskell
{- | The default empty 'Data.ConfigFile' object.

The content contains only an empty mandatory @DEFAULT@ section.

'optionxform' is set to @map toLower@.

'usedefault' is set to @True@.

'accessfunc' is set to 'simpleAccess'.
-}
emptyCP :: ConfigParser
emptyCP = ConfigParser { content = fromAL [("DEFAULT", [])],
                       defaulthandler = defdefaulthandler,
                       optionxform = map toLower,
                       usedefault = True,
                       accessfunc = simpleAccess}
```
See [ConfigFile.hs](src/Data/ConfigFile.hs) for more extensive documentation.

## <a name="configuring_the_configparser"></a>Configuring the ConfigParser

You may notice that the `ConfigParser` object has some configurable parameters, such as
`usedefault`. In case you're not familiar with the Haskell syntax for working with these,
you can use syntax like this to set these options:

```haskell
let cp2 = cp { usedefault = False }
```

This will create a new `ConfigParser` that is the same as cp except for the `usedefault` field,
which is now always False. The new object will be called `cp2` in this example.

### <a name="access_functions"></a>Access Functions

See [ConfigFile.hs](src/Data/ConfigFile.hs) for more extensive documentation.

## <a name="reading"></a>Reading

You can use these functions to read data from a file.

A common idiom for loading a new object from stratch is:

```haskell
cp <- readfile emptyCP "/etc/foo.cfg"
```

Note the use of `emptyCP`; this will essentially cause the file's data to be merged with
the empty `ConfigParser`.

See [ConfigFile.hs](src/Data/ConfigFile.hs) for more extensive documentation.

## <a name="accessing_data"></a>Accessing Data

See [ConfigFile.hs](src/Data/ConfigFile.hs).

## <a name="modifying_data"></a>Modifying Data

See [ConfigFile.hs](src/Data/ConfigFile.hs).

## <a name="output_data"></a>Output Data

See [ConfigFile.hs](src/Data/ConfigFile.hs).
