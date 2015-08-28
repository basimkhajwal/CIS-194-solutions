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
extend oldState var val x = if x == var then val else oldState x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE currentState (Var var)              = currentState var
evalE _            (Val val)              = val
evalE currentState (Op first func second) = getBinaryFunction func (evalE currentState first) (evalE currentState second)
    where boolToInt f a b = if f a b then 1 else 0
          getBinaryFunction f = case f of
                                Plus    -> (+)
                                Minus   -> (-)
                                Times   -> (*)
                                Divide  -> quot
                                Gt      -> boolToInt (>)
                                Ge      -> boolToInt (>=)
                                Lt      -> boolToInt (<)
                                Le      -> boolToInt (<=)
                                Eql     -> boolToInt (==)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar statement = case statement of
                    (Assign str expr           ) -> DAssign str expr
                    (Incr str                  ) -> DAssign str (Op (Var str) Plus (Val 1))
                    (If expr first second      ) -> DIf expr (desugar first) (desugar second)
                    (While expr stm            ) -> DWhile expr (desugar stm)
                    (For start test loop inner ) -> DSequence (desugar start) (DWhile test (DSequence (desugar inner) (desugar loop)) )
                    (Sequence first second     ) -> DSequence (desugar first) (desugar second)
                    (Skip                      ) -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple currentState statement = case statement of
                    (DAssign var val)           -> extendState var (evalExpression val)
                    (DIf test correct wrong)    -> evalStatement $ if evalExpression test == 0 then wrong else correct
                    (DWhile test loop)          -> if evalExpression test == 0 then currentState else evalSimple (evalStatement loop) statement
                    (DSequence a b)             -> evalSimple (evalStatement a) b
                    (DSkip)                     -> currentState

                    where evalExpression = evalE currentState
                          evalStatement = evalSimple currentState
                          extendState = extend currentState

run :: State -> Statement -> State
run state = evalSimple state . desugar

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
