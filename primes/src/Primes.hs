module Primes where

data PrimeError
  = LessThanTwo
  | TooLarge
  deriving (Eq)

instance Show PrimeError where
  show LessThanTwo = "Number less than 2 are not candidates for primality"
  show TooLarge = "Number too large to be tested for primality"

displayResult :: Either PrimeError Bool -> String
displayResult (Left err) = show err
displayResult (Right True) = "Prime"
displayResult (Right False) = "Not prime"

primes :: [Int]
primes = sieve [2 .. 100000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where
    noFactors = filter (\x -> x `mod` nextPrime /= 0) rest

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left LessThanTwo
  | n >= length primes = Left TooLarge
  | otherwise = Right (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (next : rest) =
  if n `mod` next == 0
    then next : unsafePrimeFactors (n `div` next) (next : rest)
    else unsafePrimeFactors n rest

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where
    primesLessThanN = filter (<= n) primes
