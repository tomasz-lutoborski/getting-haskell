cup ml = \message -> message ml

getVol aCup = aCup (\ml -> ml)

drink aCup volDrank =
  if volDiff >= 0
    then cup volDiff
    else cup 0
  where
    volDiff = getVol aCup - volDrank

isEmpty aCup = getVol aCup == 0

robot (name, hp, attack) = \message -> message (name, hp, attack)

name (n, _, _) = n

attack (_, _, a) = a

hp (_, h, _) = h

getName aRobot = aRobot name

getHp aRobot = aRobot hp

getAttack aRobot = aRobot attack

setName aRobot newName = aRobot (\(n, h, a) -> robot (newName, h, a))

setAttack aRobot newAttack = aRobot (\(n, h, a) -> robot (n, h, newAttack))

setHp aRobot newHp = aRobot (\(n, h, a) -> robot (n, newHp, a))

printRobot aRobot = aRobot (\(n, h, a) -> n ++ ", attack: " ++ show a ++ ", hp: " ++ show h)

damage aRobot attackDamage = aRobot (\(n, h, a) -> robot (n, h - attackDamage, a))

fight aRobot defender = damage defender attack
  where
    attack =
      if getHp aRobot > 10
        then getAttack aRobot
        else 0

getHps robots = map getHp robots
