data IntOrArray = 
    IntValue Int
  | ArrayValue [IntOrArray]

-- a = [1,2,[3],[4,[5,6]],[[7]], 8] 
a = [IntValue 1, IntValue 2, ArrayValue [IntValue 4, ArrayValue [IntValue 5, IntValue 6]], ArrayValue [ArrayValue [IntValue 7]], IntValue 8]

flatten :: [IntOrArray] -> [Int]
flatten a = a >>= flattenAnElement
  where 
    flattenAnElement :: IntOrArray -> [Int]
    flattenAnElement (IntValue v) = [v]
    flattenAnElement (ArrayValue a) = flatten a
