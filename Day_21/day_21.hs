module Main where

import Data.Map (Map, (!), insert, empty, fromList, adjust)

data Monkey = Number Int 
            | Operation { left :: String, op :: (Int -> Int -> Int), inv :: (Int -> Int -> Int), right :: String, commutative :: Bool} 
            | Unsure

parseMonkey :: String -> (String, Monkey)
parseMonkey = go . words
    where go [name, num]             = (init name, Number (read num))
          go [name, left, op, right] = (init name, Operation left (operations ! op) (inverses ! op) right (op == "+" || op == "*"))
          operations                 = fromList [("*", (*)), ("/", (div)), ("+", (+)), ("-", (-))]
          inverses                   = fromList [("*", (div)), ("/", (*)), ("+", (-)), ("-", (+))]

computeMonkey :: String -> Map String Monkey -> (Int, Map String Monkey)
computeMonkey name monkeys | Number a <- monkey = (a, monkeys)
                           | otherwise          = (res, insert name (Number res) monkeys'')
                           where monkey         = monkeys ! name
                                 (l, monkeys')  = computeMonkey (left  monkey) monkeys
                                 (r, monkeys'') = computeMonkey (right monkey) monkeys'
                                 res            = (op monkey) l r

isUnsure :: String -> Map String Monkey -> Bool
isUnsure name monkeys | Number _ <- monkey = False
                      | Unsure   <- monkey = True
                      | otherwise          = unsureLeft || unsureRight
                      where monkey         = monkeys ! name
                            unsureLeft     = isUnsure (left monkey)  monkeys
                            unsureRight    = isUnsure (right monkey) monkeys

computeUnsure :: String -> Map String Monkey -> Int -> Int
computeUnsure name monkeys target | Unsure   <- monkey = target
                                  | unsureLeft         = computeUnsure (left monkey) msR targetL
                                  | unsureRight        = computeUnsure (right monkey) msL targetR
                                  where monkey         = monkeys ! name
                                        unsureLeft     = isUnsure (left  monkey) monkeys
                                        unsureRight    = isUnsure (right monkey) monkeys
                                        (resL, msL)    = computeMonkey (left monkey) monkeys
                                        (resR, msR)    = computeMonkey (right monkey) monkeys
                                        targetL        = (inv monkey) target resR
                                        targetR        = if commutative monkey then (inv monkey) target resL else (op monkey) resL target

main = do
    input <- fromList . map parseMonkey . lines <$> readFile "input"
    let unsure = adjust (\m -> m { op = (-), inv = (+)}) "root" $ adjust (\_ -> Unsure) "humn" input
    print $ fst $ computeMonkey "root" input
    print $ computeUnsure "root" unsure 0
