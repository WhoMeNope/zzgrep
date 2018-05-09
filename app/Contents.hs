module Contents
  ( LineNum
  , Filename
  , Filter
  , Line
  , Contents
  , create
  , filename
  , Contents.filter
  , Contents.lines
  , push
  , pop
  ) where

type LineNum = Int
type Filename = String
type Filter = [Char]

type Line = (LineNum, [String])
type Contents = (Filename, Filter, [Line])

create :: String -> String -> Contents
create filename input =
  let parsed = zipWith (\n l -> (n, [l])) [1,2 ..] (Prelude.lines input)
   in (filename, [], parsed)

filename :: Contents -> Filename
filename (fn, ft, ls) = fn

filter :: Contents -> Filter
filter (fn, ft, ls) = ft

lines :: Contents -> [Line]
lines (fn, ft, ls) = Prelude.filter predicate ls
  where
    predicate (n, w:ws)
      | w /= [] = True
      | otherwise = False

push :: Contents -> Char -> Contents
push (fn, ft, ls) c =
  let filterLine (lno, (l:r)) =
        let (found, rest) = breakInclusive (\c' -> c' == c) l
         in if c `elem` found && rest == ""
              then (lno, found : r)
              else (lno, rest : found : r)
   in (fn, ft ++ [c], fmap filterLine ls)

pop :: Contents -> Contents
pop (fn, ft, ls)
  | ft == [] = (fn, ft, ls)
  | otherwise = (fn, init ft, fmap reconnectLast ls)
  where
    reconnectLast (n, x:y:ls) = (n, (y ++ x):ls)
    reconnectLast (n, ls) = (n, ls)

breakInclusive :: (Eq a) => (a -> Bool) -> [a] -> ([a], [a])
breakInclusive _ [] = ([], [])
breakInclusive p (x:xs)
  | p x = ([x], xs)
  | otherwise =
    let (ys, zs) = breakInclusive p xs
     in (x : ys, zs)
