toInts :: String -> [Int]
toInts = map read . lines

-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let numbers = toInts userInput
--   print (sum numbers)

calc :: [String] -> Int
calc (val1 : "+" : val2 : rest) = read val1 + read val2
calc (val1 : "*" : val2 : rest) = read val1 * read val2
calc _ = 0

main :: IO ()
main = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)
