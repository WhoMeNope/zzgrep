module Draw
  ( initWindow
  , draw
  ) where

import qualified Contents as C
import qualified Flags as F

import UI.NCurses

initWindow :: Curses (Window)
initWindow = do
  (rows, columns) <- screenSize
  dw <- defaultWindow
  --   newWindow rows columns beginY beginX
  w <- newWindow rows columns 0 0
  updateWindow dw $ overlay w OverlayMerge
  maxColorID <- maxColorID
  customColors <- canDefineColor
  if customColors
    then defineColor ColorDefault 10 10 10
    else return ()
  colorid <- newColorID ColorWhite ColorDefault (maxColorID - 1)
  updateWindow w $ setBackground $ Glyph ' ' [AttributeColor colorid]
  return w

draw :: Window -> [F.Flag] -> [C.Contents] -> Curses ()
draw w flags contents =
  -- drawString throws if text overflows screen - catch and ignore
  (tryCurses $ do
     updateWindow w $ do
       (rows, columns) <- windowSize
       clear
       -- Filter string
       moveCursor 0 0
       drawString "$ "
       drawString $ C.filter . head $ contents
       -- Separator
       moveCursor 1 0
       drawLineH (Just glyphLineH) columns
       -- Content
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
                 if p == "\n"
                   then acc
                   else acc ++
                        [ drawString $ init p
                        , drawGlyph $ Glyph (last p) [AttributeBold]
                        ])
              []
              (tail parts)
      sequence_ renderSeq
      drawString $ head parts
      drawString "\n"
