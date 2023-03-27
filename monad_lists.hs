powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)

powersOfTwo' :: Int -> [Int]
powersOfTwo' n = [2 ^ value | value <- [1 .. n]]

powersOfTwoAndThree' :: Int -> [(Int, Int)]
powersOfTwoAndThree' n = [(2 ^ value, 3 ^ value) | value <- [1 .. n]]

allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n = [(evenValue, oddValue) | evenValue <- [2, 4 .. n], oddValue <- [1, 3 .. n]]

monthsEnds :: [Int]
monthsEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [(Int, Int)]
dates ends = do
  month <- [1 .. 12]
  day <- [1 .. ends !! (month - 1)]
  return (month, day)

datesComprehension :: [Int] -> [(Int, Int)]
datesComprehension ends = [(month, day) | month <- [1 .. 12], day <- [1 .. ends !! (month - 1)]]

datesLambda :: [Int] -> [(Int, Int)]
datesLambda ends = [1 .. 12] >>= (\month -> [1 .. ends !! (month - 1)] >>= (\day -> return (month, day)))
