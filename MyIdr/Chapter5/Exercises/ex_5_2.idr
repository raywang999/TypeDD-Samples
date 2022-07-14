module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
        then pure (Just (cast input))
        else pure Nothing

-- Exercise 1
guess_v1 : (target : Nat) -> IO ()
guess_v1 target = do
    putStr "guess_v1 a number: "
    Just inp <- readNumber | Nothing => do
        putStrLn "Invalid Number"
        guess_v1 target
    case compare inp target of
          LT => do 
            putStrLn "Too small"
            guess_v1 target
          EQ => do
            putStrLn "Correct!"
          GT => do
            putStrLn "Too large"
            guess_v1 target

-- Exercise 3
guess : (target : Nat) -> (attempt : Nat) -> IO ()
guess target attempt = do
    putStr ("Attempt " ++ (show attempt) ++ ": ")
    Just inp <- readNumber | Nothing => do
        putStrLn "Invalid Number"
        guess target attempt
    case compare inp target of
          LT => do 
            putStrLn "Too small"
            guess target (S attempt)
          EQ => do
            putStrLn "Correct!"
          GT => do
            putStrLn "Too large"
            guess target (S attempt)

-- Exercise 2
main : IO ()
main = do
    currTime <- time
    guess ((modNat (cast currTime) 100) + 1) 1

-- Exercise 3
my_repl : (prompt : String) -> (onInput : String -> String) -> IO ()
my_repl prompt onInput = do
    putStr prompt
    inp <- getLine
    putStrLn (onInput inp)
    my_repl prompt onInput

my_replWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
my_replWith state prompt onInput = do
    putStr prompt
    inp <- getLine
    case onInput state inp of 
        Nothing => pure ()
        (Just (output, newState)) => do
            putStrLn output
            my_replWith newState prompt onInput

testReplWithOnInput : Nat -> String -> Maybe (String, Nat)
testReplWithOnInput state inp = if inp == "n" then Nothing else Just ("Working!" ++ (show state), state + 1)

