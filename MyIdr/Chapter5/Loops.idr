module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do 
    putStrLn (show (S secs))
    usleep 1000000
    countdown secs

readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
        then pure (Just (cast input))
        else pure Nothing

countdowns : IO ()
countdowns = do 
    putStr "Enter starting number: "
    Just secs <- readNumber | 
        Nothing => do 
            putStrLn "Invalid Input" 
            countdowns
    countdown secs
    putStr "Another? (y/n)? "
    yn <- getLine 
    if yn == "y" then countdowns else pure ()
