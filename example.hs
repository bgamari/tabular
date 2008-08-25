import Text.Tabular
import Text.Tabular.AsciiArt

main = putStr $ unlines $ map (render id) [example2, example]

example = Table
  (Group SingleLine
     [ Group NoLine [Header "A 1", Header "A 2"]
     , Group NoLine [Header "B 1", Header "B 2", Header "B 3"]
     ])
  (Group DoubleLine
     [ Group SingleLine [Header "memtest 1", Header "memtest 2"]
     , Group SingleLine [Header "time test 1", Header "time test 2"]
     ])
  [ ["hog", "terrible", "slow", "slower"]
  , ["pig", "not bad",  "fast", "slowest"]
  , ["good", "awful" ,  "intolerable", "bearable"]
  , ["better", "no chance", "crawling", "amazing"]
  , ["meh",  "well...", "worst ever", "ok"]
  ]

example2 =
  empty ^..^ colH "memtest 1" ^|^ colH "memtest 2"
        ^||^ colH "time test" ^|^ colH "time test 2"
  +.+ row "A 1" ["hog", "terrible", "slow", "slower"]
  +.+ row "A 2" ["pig", "not bad", "fast", "slowest"]
  +----+
      row "B 1" ["good", "awful", "intolerable", "bearable"]
  +.+ row "B 2" ["better", "no chance", "crawling", "amazing"]
  +.+ row "B 3" ["meh",  "well...", "worst ever", "ok"]
