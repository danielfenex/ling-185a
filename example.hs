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