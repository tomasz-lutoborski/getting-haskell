fastFib :: Integer -> Integer -> Integer -> Integer
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib a b 3 = a + b
fastFib a b n = fastFib (a + b) a (n - 1)

fib :: Integer -> Integer
fib n = fastFib 1 1 n

main :: IO ()
main = do
  n <- readLn
  print $ fib n
