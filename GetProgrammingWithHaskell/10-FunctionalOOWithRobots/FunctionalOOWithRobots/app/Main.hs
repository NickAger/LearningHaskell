module Main where

import           Lib

main :: IO ()
main = someFunc


cup f1Oz = \message -> message f1Oz

coffeeCup = cup 12

getOz aCup = aCup (\f1Oz -> f1Oz)

drink aCup ozDrank = if ozDiff >= 0 then cup ozDiff else cup 0
 where
  flOz   = getOz aCup
  ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]

--

robot (name, attack, hp) = \message -> message (name, attack, hp)
killerRobot = robot ("Kill3r", 25, 200)
name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

-- `object method`
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- `method object`
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

-- experiment to change to `object method`
setName2 (_, a, h) newName = robot (newName, a, h)

nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))
printRobot2 (n, a, h) = mconcat [n, " attack:", show a, " hp:", show h]


damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack where attack = if getHP aRobot > 10 then getAttack aRobot else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
