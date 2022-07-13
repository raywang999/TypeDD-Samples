sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = 
    let val = cast inp in 
        if val < 0 
            then Nothing
            else let newTot = tot + val in
                Just ("Subtotal: " ++ show newTot ++"\n", newTot)

main : IO ()
main = replWith 0 "Value: " sumInputs

