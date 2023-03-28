module Main where

import Data.Text.IO qualified as TIO
import Palindrome qualified

main :: IO ()
main = do
  print "Enter a string:"
  text <- TIO.getLine
  let response =
        if Palindrome.isPalindrome text
          then "It's a palindrome!"
          else "Nope!"
  print response
