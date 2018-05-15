module Draw
  ( initWindow
  , draw
  ) where

import qualified Contents as C
import qualified Flags as F

import UI.NCurses

initWindow :: Curses (Window)
initWindow = do
  dw <- defaultWindow
  w <- newWindow 0 0 0 0
  updateWindow dw $ overlay w OverlayReplace
  render
  return w

draw :: Window -> [F.Flag] -> [C.Contents] -> Curses ()
draw w flags contents =
  -- drawString throws if text overflows screen - catch and ignore
  (tryCurses $ do
     updateWindow w $ do
       clear
       moveCursor 0 0
       drawString "$ "
       drawString $ C.filter . head $ contents
       moveCursor 2 0
       mapM_ (\c -> drawLines (C.filename c) flags (C.lines c)) contents
     render) >>
  return ()

drawLines :: C.Filename -> [F.Flag] -> [C.Line] -> Update ()
drawLines filename flags lines =
  let prefix =
        if F.Filename `elem` flags
          then filename ++ ":"
          else ""
      number =
        if F.Number `elem` flags
          then \n -> (show n) ++ ":"
          else \_ -> ""
   in mapM_ (drawLine prefix number) lines
  where
    drawLine prefix number (lno, parts) = do
      drawString (prefix ++ (number lno))
      let renderSeq =
            foldr
              (\p acc ->
                let strn = init p
                    bold = last p
                 in if p == "\n"
                      then acc
                      else acc ++ [drawString strn, drawGlyph $ Glyph bold [AttributeBold]])
              []
              $ tail parts
      sequence_ renderSeq
      drawString $ head parts
      drawString "\n"
