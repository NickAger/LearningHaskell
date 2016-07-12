module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state key value aKey = if aKey == key then value else state aKey

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var stringVar) = state stringVar
evalE _ (Val intVal) = intVal
evalE state (Op expression1 bop expression2) = opFunc bop  (evalE state expression1) (evalE state expression2)
  where 
    opFunc :: Bop -> Int -> Int -> Int
    opFunc Plus = (+)
    opFunc Minus = (-)
    opFunc Times = (*)
    opFunc Divide = div
    opFunc Gt = \x y -> fromBool (x > y)
    opFunc Ge = \x y -> fromBool (x >= y)
    opFunc Lt = \x y -> fromBool (x < y)
    opFunc Le = \x y -> fromBool (x <= y)
    opFunc Eql = \x y -> fromBool (x == y)

    fromBool :: Bool -> Int
    fromBool True = 1
    fromBool False = 0


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign variable expression) = DAssign variable expression
desugar (If condition statement1 statement2) = DIf condition (desugar statement1) (desugar statement2)
desugar (While expression statement) = DWhile expression (desugar statement)
desugar (Sequence statement1 statement2) = DSequence (desugar statement1) (desugar statement2)
desugar Skip = DSkip
desugar (Incr variable) = DAssign variable (Op (Var variable) Plus (Val 1)) 
desugar (For initialisationStatement loopConditional counterUpdateStatement statement) = 
  DSequence (desugar initialisationStatement) whileLoop
    where
      whileLoop = DWhile loopConditional loopBody
      loopBody = DSequence (desugar statement) (desugar counterUpdateStatement)

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign variable expression) = extend state variable  (evalE state expression)
evalSimple state (DIf expression statement1 statement2) = 
  if evalE state expression == 1 then evalSimple state statement1 else evalSimple state statement2
evalSimple state (DWhile expression statement) = 
  if evalE state expression == 0 then state
    else 
      evalSimple newState (DWhile expression statement)
         where newState = evalSimple state statement
evalSimple state (DSequence statement1 statement2) = 
  evalSimple (evalSimple state statement1) statement2
evalSimple state DSkip = state

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
