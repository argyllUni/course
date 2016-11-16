{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
    do
        a <- getArgs
        case a of
            x:._ -> run x
            _ -> putStrLn "please pass an argument"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run name =
--    printFiles (undefined)
    --printFiles (getFiles (lines (readFile name)))
    readFile name >>= \r ->
    getFiles (lines r) >>= \s ->
    printFiles s
    {-
    do
        r <- readFile name
        s <- getFiles (lines r)
        printFiles s
    -}


getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles f =
    sequence (getFile <$> f)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
    lift2 (<$>) (,) readFile
    -- \f -> (<$>) ((,) f) (readFile f)
    --(\c -> (f, c)) <$> readFile f
    {-do
        c <- readFile f
        return (f, c)-}
    --readFile <$> undefined

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
    -- \list -> void (sequence ((<$>) (uncurry printFile) list))
    -- \x -> f (g (h x))
    -- \x -> (f . g . h) x
    -- f . g . x
    void . sequence . (<$>) (uncurry printFile)
{-
printFiles list =
    --void (sequence ((\(n, c) -> printFile n c) <$> list))
    void (sequence (uncurry printFile <$> list))
-}
{-
printFiles Nil = pure ()
--printFiles (h:.t) =
    --printFiles t >> printFile (fst h) (snd h)
printFiles ((n, c):.t) =
    printFile n c *> printFiles t
printFiles =
    foldRight (\(n, c) t -> printFile n c *> t) (pure ())
-}

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile f c =
--    putStrLn c
    --putStrLn ("============" ++ f) >>= \_ -> putStrLn c
    -- \_ -> putStrLn c =<< putStrLn ("============" ++ f)
    --putStrLn ("============" ++ f) <* putStrLn c
    --putStrLn ("============" ++ f) *> putStrLn c
    putStrLn ("============" ++ f) >> putStrLn c
    --putStrLn ("============" ++ f >> c)
    --putStrLn ("============" ++ f ++ c)
    {- do
        putStrln (f)
        putStrln c
    -}

--  putStrLn :: Chars -> IO ()
--  readFile :: Chars -> IO Chars
--  lines :: Chars -> List Char
--  joshua-morris @ GH
--
--  monads
--      anything with bind and return
--      reduces code duplication
--
--  lenses
--  Transducer is lense without good stuff
--  https://www.reddit.com/r/haskell/comments/2cv6l4/clojures_transducers_are_perverse_lenses/
--  https://en.wikipedia.org/wiki/Finite-state_transducer
--
