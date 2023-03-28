{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( isPalindrome,
  )
where

import Data.Char (isPunctuation, isSpace, toLower)
import Data.Text qualified as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

preProcess :: T.Text -> T.Text
preProcess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preProcess text
