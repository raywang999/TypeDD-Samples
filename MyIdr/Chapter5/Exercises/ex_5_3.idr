import Data.Vect

readToBlank : IO (List String)
readToBlank = do
    x <- getLine
    if x == "" 
        then pure []
        else do xs <- readToBlank
                pure (x :: xs)

readAndSave : IO ()
readAndSave = do
    strs <- readToBlank
    putStr "Filename: "
    filename <- getLine
    Right _ <- writeFile filename (unlines strs) | Left err => putStrLn (show err)
    putStrLn "Success!"

readVectFileHelper : (file : File) -> IO (n : Nat ** Vect n String)
readVectFileHelper file = do
    isEOF <- fEOF file
    (case isEOF of
          False => do
            Right line <- fGetLine file | Left _ => pure (_ ** [])
            (_ ** rest) <- readVectFileHelper file
            pure (_ ** (line :: rest))
          True => pure (_ ** []))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    Right file <- openFile filename Read | Left _ => pure (_ ** [])
    readVectFileHelper file 

