-- | Note: the code from this module is from to Toxaris
--   on #haskell in a conversation from 2008-08-24
module Text.Tabular where

data Properties = NoLine | SingleLine | DoubleLine
data Header = Header String | Group Properties [Header]

-- |
-- > example = Table
-- >   (Group SingleLine
-- >      [ Group NoLine [Header "A 1", Header "A 2"]
-- >      , Group NoLine [Header "B 1", Header "B 2", Header "B 3"]
-- >      ])
-- >   (Group DoubleLine
-- >      [ Group SingleLine [Header "memtest 1", Header "memtest 2"]
-- >      , Group SingleLine [Header "time test 1", Header "time test 2"]
-- >      ])
-- >   [ ["hog", "terrible", "slow", "slower"]
-- >   , ["pig", "not bad",  "fast", "slowest"]
-- >   , ["good", "awful" ,  "intolerable", "bearable"]
-- >   , ["better", "no chance", "crawling", "amazing"]
-- >   , ["meh",  "well...", "worst ever", "ok"]
-- >   ]
--
-- > -- Text.Tabular.AsciiArt.render example id
-- > --
-- > --   memtest 1 |   memtest 2 || time test 1 | time test 2
-- > -- ============+=============++=============+============
-- > --         hog |    terrible ||        slow |      slower
-- > --         pig |     not bad ||        fast |     slowest
-- > -- ------------+-------------++-------------+------------
-- > --        good |       awful || intolerable |    bearable
-- > --      better |   no chance ||    crawling |     amazing
-- > --         meh |     well... ||  worst ever |          ok
data Table a = Table Header Header [[a]]

-- ----------------------------------------------------------------------
-- * Combinators
-- ----------------------------------------------------------------------

-- | Just one row (or column)
data SemiTable a = SemiTable Header [a]

empty :: Table a
empty = Table (Group NoLine []) (Group NoLine []) []

uncol :: SemiTable a -> Table a
uncol (SemiTable cols data0) = Table (Group NoLine []) cols [data0]

col :: String -> [a] -> SemiTable a
col header cells = SemiTable (Header header) cells

row :: String -> [a] -> SemiTable a
row = col

beside :: Properties -> Table a -> SemiTable a -> Table a
beside prop (Table rows cols1 data1)
            (SemiTable  cols2 data2) =
  Table rows (Group prop [cols1, cols2])
             (zipWith (++) data1 [data2])

below :: Properties -> Table a -> SemiTable a -> Table a
below prop (Table     rows1 cols data1)
           (SemiTable rows2      data2) =
  Table (Group prop [rows1, rows2]) cols (data1 ++ [data2])

(^..^) = beside NoLine
(^|^)  = beside SingleLine
(^||^) = beside DoubleLine

(+.+) = below NoLine
(+----+) = below SingleLine
(+====+) = below DoubleLine
