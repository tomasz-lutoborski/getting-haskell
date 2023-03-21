halve :: Integer -> Integer
halve x = fromIntegral x `div` 2

printDouble :: Integer -> String
printDouble x = show (x * 2)

data ABO = A | B | AB | O

data Rh = Pos | Neg

data BloodType = BloodType ABO Rh

showRh :: Rh -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABO -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

data Sex = Male | Female

type MiddleName = String

type FirstName = String

type LastName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

data Patient = Patient {name :: Name, sex :: Sex, age :: Integer, height :: Integer, weight :: Integer, bloodType :: BloodType}

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l

canDonateTo' :: Patient -> Patient -> Bool
canDonateTo' p1 p2 = canDonateTo (bloodType p1) (bloodType p2)
