module Main where

import qualified Contents as C
import qualified Draw as D
import qualified Flags as F

import Data.Char
import Data.List
import System.Environment
import System.Exit
import UI.NCurses

readInput :: [String] -> IO [C.Contents]
readInput [] = fmap (return . C.create "(standard input)") getContents
readInput ["-"] = fmap (return . C.create "(standard input)") getContents
readInput fs
  | "-" `elem` fs =
    let withoutStdin = delete "-" fs
     in (readInput withoutStdin) `mappend` fmap (return . C.create "(standard input)") getContents
  | otherwise =
    let reads = map (\f -> fmap (C.create f) $ readFile f) fs
     in sequence reads

outputLines :: [F.Flag] -> C.Contents -> IO ()
outputLines flags contents = putStr $ unlines . fmap formatLine $ C.lines contents
  where
    formatLine (lno, parts) =
      let string = foldr (\line acc -> acc ++ line) [] parts
          prefix =
            if F.FilenameOut `elem` flags
              then C.filename contents ++ ":"
              else ""
          number =
            if F.NumberOut `elem` flags
              then (show lno) ++ ":"
              else ""
       in prefix ++ number ++ string

main = do
  (as, fs) <- F.parse =<< getArgs
  -- putStrLn $ "Flags: " ++ show as
  -- putStrLn $ "Files: " ++ show fs
  contents <- readInput fs
  filtered <- runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- D.initWindow
    D.draw w as contents
    f <- runLoop w as contents
    closeWindow w
    return f
  mapM_ (outputLines as) filtered

runLoop :: Window -> [F.Flag] -> [C.Contents] -> Curses ([C.Contents])
runLoop w fs contents = do
  ev <- getEvent w Nothing -- blocking
  handleEvent ev w fs contents

handleEvent :: Maybe Event -> Window -> [F.Flag] -> [C.Contents] -> Curses ([C.Contents])
handleEvent Nothing w fs contents = runLoop w fs contents
handleEvent (Just ev) w fs contents =
  case ev of
    EventCharacter c ->
      case c of
        -- all cases of newline
        '\FF' -> handleDone
        '\CR' -> handleDone
        '\LF' -> handleDone
        -- exit
        '\ESC' -> handleExit
        -- delete & backspace
        '\BS' -> handleDelete w fs contents
        '\DEL' -> handleDelete w fs contents
        -- other characters
        _ ->
          if isPrint c
            then handleFilter w fs contents c
            else runLoop w fs contents
    _ -> runLoop w fs contents
  where
    handleDone = return contents
    handleExit = return contents
    handleDelete w fs contents = do
      let newContents = fmap C.pop contents
      D.draw w fs newContents
      runLoop w fs newContents
    handleFilter w fs contents c = do
      let newContents = fmap (flip C.push $ c) contents
      D.draw w fs newContents
      runLoop w fs newContents
