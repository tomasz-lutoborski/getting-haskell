import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import System.Environment
import System.Random

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 256

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> BC.ByteString -> Int -> BC.ByteString
replaceByte location content byte = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt location content
    after = BC.drop 1 rest
    newChar = BC.pack [intToChar byte]

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte content = do
  let bytesLength = BC.length content
  location <- randomRIO (1, bytesLength)
  char <- randomRIO (0, 255)
  return (replaceByte location content char)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size content = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start content
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection content = do
  let size = 25
  let bytesLength = BC.length content
  start <- randomRIO (0, bytesLength - size)
  return (sortSection start size content)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size content = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start content
    (target, after) = BC.splitAt size rest
    changed = BC.reverse target

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection content = do
  let size = 25
  let bytesLength = BC.length content
  start <- randomRIO (0, bytesLength - size)
  return (reverseSection start size content)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <-
    foldM
      (\bytes func -> func bytes)
      imageFile
      [randomReverseSection]
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"
