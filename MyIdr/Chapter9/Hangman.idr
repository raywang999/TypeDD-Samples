import Data.Vect

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
    MkWordState : (word : String) -> 
        (missing : Vect letters Char) -> 
        WordState guesses_remaining letters

data Finished : Type where
    Won : WordState (S guesses) 0 -> Finished
    Lost : WordState 0 (S letters) -> Finished

data ValidInput : List Char -> Type where
    Letter : (c : Char) -> ValidInput [c]

nilNotValid : ValidInput [] -> Void
nilNotValid (Letter _) impossible

consNotValid : {x : Char} -> {y : Char} -> {ys : List Char} -> ValidInput (x :: (y :: ys)) -> Void
consNotValid (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No nilNotValid
isValidInput (x :: xs) = case xs of
                              [] => Yes (Letter x)
                              (y :: ys) => No consNotValid

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x) --Why not just use IO (ValidInput x)? 
readGuess = do
    putStr "Guess:"
    x <- getLine
    (case isValidString x of
          (Yes prf) => pure (_ ** prf)
          (No contra) => do
            putStrLn "Invalid guess"    
            readGuess)

removeElem : DecEq a => (value : a) -> 
    (xs: Vect (S k) a) -> 
    {auto prf: Elem value xs} -> 
    Vect k a 
removeElem value (value :: ys) {prf = Here} = ys
removeElem {k = Z} value (y :: []) {prf = There later} = absurd later
removeElem {k = (S n)} value (y :: ys) {prf = There later} = y :: removeElem value ys

processGuess : (letter : Char) -> 
    WordState (S guesses) (S letters) -> 
    Either (WordState guesses (S letters))
           (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = 
    (case isElem letter missing of
          (Yes prf) => Right (MkWordState word (removeElem letter missing))
          (No contra) => Left (MkWordState word missing))

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do 
    (_ ** Letter letter) <- readGuess
    (case processGuess letter st of
          (Left l) => do 
            putStrLn "Incorrect guess"
            (case guesses of
                  Z => pure (Lost l)
                  (S k) => game l)
          (Right r) => do
            putStrLn "Correct!"
            (case letters of
                  Z => pure (Won r)
                  (S k) => game r))

main : IO ()
main = do
    result <- game {guesses = 1} (MkWordState "Test" ['T','E','S'])
    (case result of
          (Won (MkWordState word missing)) => putStrLn "Ez W."
          (Lost (MkWordState word missing)) => putStrLn ("L. Word was: " ++ word))
