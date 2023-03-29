import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] $ repeat True

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  stArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray stArray i val
  return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort arr = runSTUArray $ do
  let (lo, hi) = bounds arr
  stArr <- thaw arr
  forM_ [lo .. hi] $ \i -> do
    forM_ [lo .. hi - 1] $ \j -> do
      a <- readArray stArr j
      b <- readArray stArr (j + 1)
      when (a > b) $ do
        writeArray stArr j b
        writeArray stArr (j + 1) a
  return stArr
