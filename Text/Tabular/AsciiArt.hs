module Text.Tabular.AsciiArt where

import Data.List (intersperse, transpose)
import Text.Tabular

-- | for simplicity, we assume that each cell is rendered
--   on a single line
render :: (a -> String) -> Table a -> String
render f (Table rh ch cells) =
  unlines $ [ renderColumns sizes ch2
            , concat $ renderHLine sizes ch2 DoubleLine
            ] ++
            (renderRs
             $ zipHeader (zipWith renderR rhStrings cells) rh)
 where
  -- ch2 and cell2 include the row and column labels
  ch2 = Group DoubleLine [Header "", ch]
  cells2 = headerStrings ch2
         : zipWith (\h cs -> h : map f cs) rhStrings cells
  --
  renderR h cs = renderColumns sizes $ Group DoubleLine
                    [Header h, zipHeader (map f cs) ch]
  rhStrings = headerStrings rh
  -- maximum width for each column
  sizes   = map (maximum . map length) . transpose $ cells2
  renderRs (Header s)   = [s]
  renderRs (Group p hs) = concat . intersperse sep . map renderRs $ hs
    where sep = renderHLine sizes ch2 p

-- | We stop rendering on the shortest list!
renderColumns :: [Int] -- ^ max width for each column
              -> Header
              -> String
renderColumns is h = concat $ zipOnHeader renderVLine padLeft is h

renderVLine :: Properties -> String
renderVLine NoLine     = ""
renderVLine SingleLine = " | "
renderVLine DoubleLine = " || "

renderHLine :: [Int] -- ^ width specifications
            -> Header
            -> Properties
            -> [String]
renderHLine _ _ NoLine = []
renderHLine w h SingleLine = [renderHLine' w '-' h]
renderHLine w h DoubleLine = [renderHLine' w '=' h]

renderHLine' :: [Int] -> Char -> Header -> String
renderHLine' is sep h = concat $ zipOnHeader vsep dashes is h
 where
  dashes i _ = replicate i sep
  vsep NoLine     = ""
  vsep SingleLine = sep : "+"  ++ [sep]
  vsep DoubleLine = sep : "++" ++ [sep]

padLeft :: Int -> String -> String
padLeft l s = padding ++ s
 where padding = replicate (l - length s) ' '
