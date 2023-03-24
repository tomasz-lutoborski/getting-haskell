{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO

type Author = T.Text

type Title = T.Text

type Html = T.Text

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

type MarcDirectoryRaw = B.ByteString

type MarcDirectoryEntryRaw = B.ByteString

type FieldText = T.Text

data Book = Book
  { author :: Author,
    title :: Title
  }
  deriving (Show)

data FieldMetadata = FieldMetadata
  { tag :: T.Text,
    fieldLength :: Int,
    fieldStart :: Int
  }
  deriving (Show)

leaderLength :: Int
leaderLength = 24

dirEntryLength :: Int
dirEntryLength = 12

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

book1 :: Book
book1 =
  Book
    { title = "The Conspiracy Against the Human Race",
      author = "Ligotti, Thomas"
    }

book2 :: Book
book2 =
  Book
    { title = "A Short History of Decay",
      author = "Cioran, Emil"
    }

book3 :: Book
book3 =
  Book
    { title = "The Tears of Eros",
      author = "Bataille, Georges"
    }

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where
    remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty then [] else nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) = B.splitAt dirEntryLength directory

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where
    (rawTag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 rawTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if length results < 1
    then Nothing
    else Just (head results)
  where
    metadata = getFieldMetadata (splitDirectory (getDirectory record))
    results = filter (\x -> tag x == aTag) metadata

lookupSubfield :: Char -> (Maybe FieldMetadata) -> MarcRecordRaw -> Maybe T.Text
lookupSubfield _ Nothing _ = Nothing
lookupSubfield subfieldCode (Just metadata) record =
  if results == []
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record metadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter (\x -> T.head x == subfieldCode) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield subfield entryMetadata record
  where
    entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream
  | marcStream == B.empty = []
  | otherwise = next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
    authorInTags = mconcat ["<em>", (author book), "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n",
      "<head><title>books</title>",
      "<meta charset='utf-8'/>",
      "</head>\n",
      "<body>\n",
      booksHtml,
      "</body>\n",
      "</html>"
    ]
  where
    booksHtml = (mconcat . map bookToHtml) books

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(t, a) -> Book {title = fromJust t, author = fromJust a}) justPairs
  where
    justPairs = filter (\(t, a) -> isJust t && isJust a) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n marcStream = booksToHtml (take n books)
  where
    books = pairsToBooks (marcToPairs marcStream)

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed
