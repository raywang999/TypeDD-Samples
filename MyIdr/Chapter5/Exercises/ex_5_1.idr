printLonger : IO ()
printLonger = do
    putStr "Input String 1: "
    s1 <- getLine
    putStr "\nInput String 2: "
    s2 <- getLine
    putStrLn ("\nLonger String has length: " ++ show (max (length s1) (length s2)))

printLongerWithoutDo : IO ()
printLongerWithoutDo = putStr "Input String 1: " >>= 
    \_ => getLine >>= 
    \s1 => putStr "\nInput String 2: " >>= 
    \_ => getLine >>= 
    \s2 => putStrLn ("\nLonger String has length: " ++ show (max (length s1) (length s2)))
