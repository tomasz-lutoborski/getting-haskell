import Data.Map qualified as Map

type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep"),
      (2, "KINGinYELLOW"),
      (3, "dagon1997"),
      (4, "rcarter1919"),
      (5, "xCTHULHUx"),
      (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000),
      ("KINGinYELLOW", 15000),
      ("dagon1997", 300),
      ("rcarter1919", 12),
      ("xCTHULHUx", 50000),
      ("yogSOThoth", 150000)
    ]

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB =
  Map.fromList
    [ (1001, 1),
      (1002, 2),
      (1003, 3),
      (1004, 4),
      (1005, 5),
      (1006, 6)
    ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits name = Map.lookup name creditsDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id =
  lookupGamerId id
    >>= lookupUserName
    >>= lookupCredits

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = mconcat ["Hello, ", name, "!"]

helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f m = m >>= (\x -> return (f x))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf m = mf >>= (\f -> m >>= (\x -> return (f x)))

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int,
    codeReview :: Grade,
    cultureFit :: Grade,
    education :: Degree
  }
  deriving (Show)

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return
    ( Candidate
        { candidateId = cId,
          codeReview = codeGrade,
          cultureFit = cultureGrade,
          education = degree
        }
    )

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

candidate1 :: Candidate
candidate1 =
  Candidate
    { candidateId = 1,
      codeReview = A,
      cultureFit = A,
      education = BA
    }

candidate2 :: Candidate
candidate2 =
  Candidate
    { candidateId = 2,
      codeReview = C,
      cultureFit = A,
      education = PhD
    }

candidate3 :: Candidate
candidate3 =
  Candidate
    { candidateId = 3,
      codeReview = A,
      cultureFit = B,
      education = MS
    }

candidateDB :: Map.Map Int Candidate
candidateDB =
  Map.fromList
    [ (1, candidate1),
      (2, candidate2),
      (3, candidate3)
    ]

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate mCandidate = do
  candidate <- mCandidate
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

data Pizza = Pizza
  { size :: Int,
    cost :: Double
  }
  deriving (Show)

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if cost p1 < cost p2
    then p1
    else p2

main :: IO ()
main =
  getLine
    >>= ( \size1 ->
            getLine
              >>= ( \cost1 ->
                      getLine
                        >>= ( \size2 ->
                                getLine
                                  >>= ( \cost2 ->
                                          ( \pizza1 ->
                                              ( \pizza2 ->
                                                  (\betterPizza -> putStrLn (show betterPizza))
                                                    (comparePizzas pizza1 pizza2)
                                              )
                                                (Pizza (read size2) (read cost2))
                                          )
                                            (Pizza (read size1) (read cost1))
                                      )
                            )
                  )
        )
