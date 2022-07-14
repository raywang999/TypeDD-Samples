printLength : IO ()
printLength = putStr "Input String: " >>= \_ => 
    getLine >>= \input => let len = length input in 
    putStrLn (show len)

