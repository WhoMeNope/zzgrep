module Flags
  ( Flag(..)
  , parse
  ) where

import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
  = Number
  | NumberOut
  | Filename
  | FilenameOut
  | Help
  deriving (Eq, Ord, Enum, Show, Bounded)

flags =
  [ Option ['n'] [] (NoArg Number) "Number the lines while filtering, starting at 1."
  , Option ['N'] [] (NoArg NumberOut) "Same as -n, but keeps the line numbers in output."
  , Option ['f'] [] (NoArg Filename) "Show filenames at start of lines. Always on for multiple inputs."
  , Option ['F'] [] (NoArg FilenameOut) "Keep filenames in output."
  , Option ['h'] ["help"] (NoArg Help) "Print this help message."
  ]

imply :: Flag -> [Flag]
imply NumberOut = [NumberOut, Number]
imply FilenameOut = [FilenameOut, Filename]
imply f = [f]

parse :: [String] -> IO ([Flag], [String])
parse argv =
  case getOpt Permute flags argv of
    (args, fs, []) -> do
      let files =
            if null fs
              then ["-"]
              else nub fs
      if Help `elem` args
        then do
          hPutStrLn stderr (usageInfo header flags)
          exitWith ExitSuccess
        else do
          let options =
                if length files > 1
                  then (Filename : args)
                  else args
          return (nub (concatMap imply options), files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: igrep [OPTION ...] [file ...]"
