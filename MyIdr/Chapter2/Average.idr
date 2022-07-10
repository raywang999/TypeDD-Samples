module Average 

||| Calculate average length of words in a string.
||| @str a string containing words separated by whitespaces
export 
average: (str: String) -> Double
average str = 
    let 
        numWords = wordCount str
        totalLength = sum (lengths (words str)) 
    in 
        cast totalLength / cast numWords
    where
        wordCount : String -> Nat
        wordCount str = length (words str)

        lengths : List String -> List Nat
        lengths strs = map length strs