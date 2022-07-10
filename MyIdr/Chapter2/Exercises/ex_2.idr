{- Exercise 1: 
("A", "B", "C") is a tuple of String with length 3. I.e.: ("A",("B","C"))
["A", "B", "C"] is a list of Strings
(('A', "B"), 'C') is a Tuple2 of Tuple2 of Char and String, and a Char
-}

-- Exercise 2
palindrome : String -> Bool
palindrome str = str == reverse str

palindrome_q2 : String -> Bool
palindrome_q2 str = let s = toLower str in
    s == reverse s

palindrome_q3 : String -> Bool
palindrome_q3 str = if length str <= 10 
    then False 
    else str == reverse str
    
palindrome_q4 : Nat -> String -> Bool
palindrome_q4 minLength str = if length str <= minLength 
    then False 
    else str == reverse str

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten list = let sortedList = sort list in
    take 10 (reverse sortedList)

over_length : Nat -> List String -> Nat
over_length maxLength strs = sum (map isGreater strs) where
    isGreater : String -> Nat
    isGreater str = if length str > maxLength then 1 else 0
