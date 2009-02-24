module Text.Tabular.AsciiArt where

import Data.List (intersperse, transpose)
import Text.Tabular

-- | for simplicity, we assume that each cell is rendered
--   on a single line
render :: (rh -> String)
       -> (ch -> String)
       -> (a -> String)
       -> Table rh ch a
       -> String
render fr fc f (Table rh ch cells) =
  unlines $ [ renderColumns sizes ch2
            , concat $ renderHLine sizes ch2 DoubleLine
            ] ++
            (renderRs $ fmap renderR $ zipHeader [] cells $ fmap fr rh)
 where
  -- ch2 and cell2 include the row and column labels
  ch2 = Group DoubleLine [Header "", fmap fc ch]
  cells2 = headerContents ch2
         : zipWith (\h cs -> h : map f cs) rhStrings cells
  --
  renderR (cs,h) = renderColumns sizes $ Group DoubleLine
                    [ Header h
                    , fmap fst $ zipHeader "" (map f cs) ch]
  rhStrings = map fr $ headerContents rh
  -- maximum width for each column
  sizes   = map (maximum . map length) . transpose $ cells2
  renderRs (Header s)   = [s]
  renderRs (Group p hs) = concat . intersperse sep . map renderRs $ hs
    where sep = renderHLine sizes ch2 p

-- | We stop rendering on the shortest list!
renderColumns :: [Int] -- ^ max width for each column
              -> Header String
              -> String
renderColumns is h =
  concatMap helper $ flattenHeader $ zipHeader 0 is h
 where
  helper = either hsep (uncurry padLeft)
  hsep :: Properties -> String
  hsep NoLine     = " "
  hsep SingleLine = " | "
  hsep DoubleLine = " || "

renderHLine :: [Int] -- ^ width specifications
            -> Header String
            -> Properties
            -> [String]
renderHLine _ _ NoLine = []
renderHLine w h SingleLine = [renderHLine' w '-' h]
renderHLine w h DoubleLine = [renderHLine' w '=' h]

renderHLine' :: [Int] -> Char -> Header String -> String
renderHLine' is sep h = concatMap helper
                      $ flattenHeader $ zipHeader 0 is h
 where
  helper          = either vsep dashes
  dashes (i,_)    = replicate i sep
  vsep NoLine     = [sep]
  vsep SingleLine = sep : "+"  ++ [sep]
  vsep DoubleLine = sep : "++" ++ [sep]

padLeft :: Int -> String -> String
padLeft l s = padding ++ s
 where padding = replicate (l - length s) ' '
