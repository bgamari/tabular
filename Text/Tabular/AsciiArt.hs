module Text.Tabular.AsciiArt where

import Control.Monad.State (evalState, State, get, put)
import Data.List (intersperse)
import Text.Tabular

-- | for simplicity, we assume that each cell is rendered
--   on a single line
render :: (a -> String) -> Table a -> String
render f (Table rh ch cells) =
  unlines $ [ renderColumns biggest ch
            , concat $ renderHLine biggest ch DoubleLine
            ] ++
            (renderRs
             $ zipHeader (map renderR cells) rh)
 where
  renderR cs = renderColumns biggest $ zipHeader (map f cs) ch
  biggest = maximum . map length $ strings
  strings = (map f . concat $ cells)
          ++ headerStrings rh
          ++ headerStrings ch
  renderRs (Header s)   = [s]
  renderRs (Group p hs) = concat . intersperse sep . map renderRs $ hs
    where sep = renderHLine biggest ch p

-- | Retrieve the strings in a header
headerStrings :: Header -> [String]
headerStrings (Header s) = [s]
headerStrings (Group _ hs) = concatMap headerStrings hs

-- | 'zipHeader' @h@ @ss@ returns the same structure
--   as @h@ except with all the text replaced by the
--   contents of @ss@
--
-- Think of this as copying the contents
--
--   If the row has too many cells, the excess is ignored.
--   If it has too few cells, the missing ones (at the end)
--   and replaced with the empty string
zipHeader :: [String] -> Header -> Header
zipHeader ss h = evalState (helper h) ss
 where
  helper (Header s) =
   do cells  <- get
      string <- case cells of
                  []     -> return ""
                  (s:ss) -> put ss >> return s
      return $ Header string
  helper (Group s hs) =
   Group s `fmap` mapM helper hs

renderColumns :: Int -- ^ width
              -> Header
              -> String
renderColumns i (Header s)   = padLeft i s
renderColumns i (Group l hs) =
   concat . intersperse sep $ map (renderColumns i) hs
 where sep = renderVLine l

renderVLine :: Properties -> String
renderVLine NoLine     = ""
renderVLine SingleLine = " | "
renderVLine DoubleLine = " || "

renderHLine :: Int -- ^ cell width
            -> Header
            -> Properties
            -> [String]
renderHLine _ _ NoLine = []
renderHLine w h SingleLine = [renderHLine' w '-' h]
renderHLine w h DoubleLine = [renderHLine' w '=' h]

renderHLine' :: Int -> Char -> Header -> String
renderHLine' w sep h =
  case h of
   Header s    -> replicate w sep
   Group vp hs -> concat . intersperse (vsep vp)
                         . map (\h -> renderHLine' w sep h) $ hs
 where
   vsep NoLine     = ""
   vsep SingleLine = sep : "+"  ++ [sep]
   vsep DoubleLine = sep : "++" ++ [sep]

padLeft :: Int -> String -> String
padLeft l s = padding ++ s
 where padding = replicate (l - length s) ' '
