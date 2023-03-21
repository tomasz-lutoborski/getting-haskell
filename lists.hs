import Data.Char (toLower)

repeat2 x = cycle [x]

subseq s e list = drop s (take e list)

inFirstHalf el list = el `elem` firstHalf
  where
    firstHalf = take (length list `div` 2) list

myGCD a b =
  if remainder == 0
    then b
    else myGCD b remainder
  where
    remainder = a `mod` b

myTail (_ : xs) = xs
myTail [] = []

myGCD2 a b =
  case remainder of
    0 -> b
    n -> myGCD2 b remainder
  where
    remainder = a `mod` b

myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : rest
  where
    rest = myTake (n - 1) xs

myReverse [] = []
myReverse (x : []) = [x]
myReverse (x : xs) = (myReverse xs) ++ [x]

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

fib n = fastFib 1 1 n

myMap f [] = []
myMap f (x : xs) = (f x) : myMap f xs

myFilter test [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

myRemove test [] = []
myRemove test (x : xs) =
  if test x
    then myRemove test xs
    else x : myRemove test xs

rcons x y = y : x

myReverse2 xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x

myElem val list = length filteredList /= 0
  where
    filteredList =
      filter (== val) list

isPalindrome text = processedText == reverse processedText
  where
    noSpaces = filter (/= ' ') text
    processedText = map toLower noSpaces

harmonic n = foldl (+) 0 (take n seriesValues)
  where
    seriesPairs = zip (cycle [1.0]) [1.0, 2.0 ..]
    seriesValues =
      map
        (\pair -> (fst pair) / (snd pair))
        seriesPairs
