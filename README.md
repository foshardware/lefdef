# DEF parser

Parses Design Exchange Format (DEF) into an Abstract Syntax Tree (AST). Intended for use with LibreSilicon Compiler (lsc).

## Usage

```haskell

import qualified Data.Text as Text

import Language.DEF.Parser (parseDEF)

main = do
  file <- Text.readFile "my.def"
  putStrLn $ show $ parseDEF file


```

## Building DEF files from AST

`def` also allows for efficient construction of DEF files through operations on the `Builder` type.

```haskell

import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder

import Language.DEF.Syntax
import Language.DEF.Parser (parseDEF)
import Language.DEF.Builder

main = do
  file <- Text.readFile "my.def"
  case parseDEF file of
    Left err -> putStrLn $ show err
    Right def -> Lazy.putStr $ toLazyText $ builderDEF def
  
```

## Tests

```bash

stack test

```
