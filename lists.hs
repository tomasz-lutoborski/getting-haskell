repeat2 x = cycle [x]

subseq s e list = drop s (take e list)

inFirstHalf el list = el `elem` firstHalf
  where
    firstHalf = take (length list `div` 2) list
