-- import qualified Control.Monad.RWS as them
-- Prepositional Formulas

data Form = T | F |Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1::Form
f1 = Dsj (Neg T) (Cnj F T)

removeNegs ::Form -> Form
removeNegs = \form -> case form of 
                      T -> T
                      F -> F
                      Neg phi -> removeNegs phi
                      -- Neg phi -> phi
                      -- This would only remove one phi
                      Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                      Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- meaningless function ^

-- type signatures are useful, Haskell will tell you if your signature is wrong


-- denotations
denotation :: Form -> Bool
denotation = \form -> case form of
    T -> True
    F -> False
    --Neg phi -> if (phi == true) False else True
    Neg phi -> case (denotation phi) of {True -> False; False -> True} -- 'case (denotation phi)' is interesting. could have used 'not'
    Cnj phi psi -> case (denotation phi) of {True -> (denotation psi); False -> False}  -- and. could have used '&&'.
    Dsj phi psi -> case (denotation phi) of {True -> True; False -> (denotation psi)}-- or. could have used '||'.



-- Natural Numbers
-- data Result = Draw | Win Shape deriving Show

data Numb = Z | S Numb deriving Show

-- z 0
-- s z 1
-- s (s z) 2
-- s (s(s z)) 3
-- n n' n'' 
-- called piano arithetic

isZero :: Numb -> Bool
isZero = \n -> case n of
    Z -> True
    S n -> False

lessThanTwo::Numb->Bool
-- lessThanTwo = \n case n of
--                 Z -> True
--                 S n' -> case n' of {Z -> True; S n'' -> False}
lessThanTwo = \n -> case n of
                    Z -> True
                    S n' -> case n' of {Z -> True; S n'' -> False}


-- this function does not take advantsge of the recursive type

-- double z -> z 
-- double (s z) -> s (s z)

double = \n -> case n of 
                Z -> Z
                -- S nn -> case nn of { S nnn -> 1+ double nnn}
                S nn -> S (S (double nn))
                -- look at the fundamental steps of a function when writing recursive functions
                -- write the output in terms of the input

isOdd::Numb -> Bool
isOdd = \n -> case n of
                Z -> False
                S n' -> not (isOdd n')

add :: Numb -> (Numb -> Numb)
-- functiond only take one argument at a time
add = \n -> case n of
            Z -> \m -> m
            S n' -> \m -> add n' (S m)
-- see photo3

-- go back and make all s into S

-- [add S S z] s s s z
-- \n -> \m -> case n of [z -> \m; S n' -> add n' [S m]] [s s z] s s s z 
-- \m -> case (s s z) of [ z -> \m; S n' -> add n' [S m]] (s s s z)
-- case ( s s z) of [ z -> s s s z; S n' -> add n' [s s s s z]]
-- add [s z] [s s s s z]


-- ----------------------------------------------------
-- -- 3. Another recursive type: lists/strings

data IntList = Empty | NonEmpty Int IntList deriving Show

myList :: IntList
myList = NonEmpty 5 (NonEmpty 7 (NonEmpty 2 Empty))

-- Two versions of the 'total' function from the handout
total :: IntList -> Int
total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

-- built-in haskell lists
-- [] empty
-- : non-empty 'cons' non-empty
otherTotal :: [Int] -> Int
otherTotal = \l -> case l of
                   [] -> 0
                   x:rest -> x + otherTotal rest

-- total( 5: ...)
-- ....

-- can write [ 5, 7, 2] instead of (5 : (7 : (2 : [])))



-- ----------------------------------------------------
-- 3.1 Polymorphism

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

myIntList :: [Int]
myIntList = [5,7,2]

myBoolList :: [Bool]
myBoolList = [True, False, True, True]

myShapeList :: [Shape]
myShapeList = [Rock, Scissors, Scissors, Rock, Paper]

-- see the many different types of char lists
-- :t "abc"
-- -> [ Char]

-- containsRock :: [Shape] -> Bool
-- containsRock = case dan of 
--                 [] ->  False
--                 dan : rest -> case dan of {Rock -> True; Paper -> containsRock rest ; Scissors -> containsRock rest;}


size = \l -> case l of
                [] -> Z
                x : rest -> S (size rest)

-- size :: [a] -> Numb

-- see photo for polymorphic type






-- Suppose we wanted to write a function for checking whether something 
-- is an element of a list, which would work with various types, like this:
--      elementOf Rock [Rock,Paper] ==>* True
--      elementOf 3 [1,2,4,5] ==>* False

-- We might start with something like the following. 
-- elementOf :: a -> ([a] -> Bool)
-- elementOf = \x -> \l -> case l of
--                         [] -> False
--                         y : rest -> case (??? x ??? y ???) of
--                                     ??? -> True
--                                     ??? -> elementOf x rest

-- Here is one solution that works: we can ``outsource'' the equality-checking to the 
-- caller of the elementOf function.
-- elementOf :: (a -> a -> Bool) -> (a -> ([a] -> Bool))
-- elementOf = \isEqual -> \x -> \l -> case l of
--                                     [] -> False
--                                     y : rest -> case (isEqual x y) of
--                                                 True -> True
--                                                 False -> elementOf isEqual x rest

-- Haskell gives us a more convenient way of doing something which is 
-- equivalent (under the hood) to the outsourcing solution.

elementOf :: (Eq a) => (a -> ([a] -> Bool))
elementOf = \x -> \l -> case l of
                        [] -> False
                        y : rest -> case (x == y) of
                                    True -> True
                                    False -> elementOf x rest

-- notice the '(Eq a) =>' in the type definition
-- this only works with types that have Eq defined in them.

-- Can we make Shape part of the Eq type class
-- Yes, by making the type definition contain
-- 'deriving (Eq, Show)' instead of ...









