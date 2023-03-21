import Distribution.TestSuite (TestInstance (name))

inc n = n + 1

double n = n * 2

square n = n * n

ifEven x =
  if even x
    then substractTwo x
    else multiplyAndAdd x
  where
    substractTwo y = y - 2
    multiplyAndAdd y = y * 3 + 1

counter x = (\x -> x + 1) ((\x -> x + 1) x)

cube x = x * x * x

ifEven2 f x = if even x then f x else x

names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
  ]

compareLastNames name1 name2 =
  if lastName2 > lastName1
    then LT
    else
      if lastName1 > lastName2
        then GT
        else
          if firstName1 > firstName2
            then LT
            else
              if firstName2 > firstName1
                then GT
                else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = snd name1
    firstName2 = snd name2

compareLastName2 name1 name2 =
  if result == EQ
    then compare (fst name1) (fst name2)
    else result
  where
    result = compare (snd name1) (snd name2)

sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

washOffice name = nameText ++ " - PO Box 456 - Washingtion, DC 89821"
  where
    nameText = (fst name) ++ " " ++ (snd name) ++ " " ++ "Esq"

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "wash" -> washOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location

genIfEven f = (\x -> ifEven2 f x)

genIfXEven x = (\f -> ifEven2 f x)

getRequestURL host apiKey resource id =
  host
    ++ "/"
    ++ resource
    ++ "/"
    ++ id
    ++ "?token="
    ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

ifEvenInc = ifEven2 inc

ifEvenDouble = ifEven2 double

ifEvenSquare = ifEven2 square

binaryPartialApplication f x = (\y -> f x y)
