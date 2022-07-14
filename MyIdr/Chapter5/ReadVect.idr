import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do 
    x <- getLine
    xs <- readVectLen k
    pure (x :: xs)

data VectUnknown : Type -> Type where
    MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (VectUnknown String)
readVect = do 
    x <- getLine
    if (x == "")
        then pure (MkVect _ [])
        else do MkVect _ xs <- readVect
                pure (MkVect _ (x :: xs))

printVect: Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = 
    putStrLn (show xs ++ " (length " ++ show len ++ ")")

anyVect : (n ** Vect n String)
anyVect = (3 ** ["You", "Are", "Fat"])

readVect_v2 : IO (len ** Vect len String)
readVect_v2 = do x <- getLine 
                 if (x == "")
                    then pure (_ ** [])
                    else do (_ ** xs) <- readVect_v2
                            pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do
    putStrLn "Vector 1 (blank line to end): "
    (len1 ** vect1) <- readVect_v2
    putStrLn "Vector 2 (blank line to end): "
    (len2 ** vect2) <- readVect_v2
    (case exactLength len1 vect2 of
          Nothing => putStrLn "Vectors are different length"
          (Just vect2') => printLn (zip vect1 vect2'))

