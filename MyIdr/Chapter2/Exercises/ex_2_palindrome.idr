module Main

palindrome : String -> Bool
palindrome str = str == reverse str

main : IO ()
main = repl "Enter a String: " showPalindrome where
    showPalindrome : String -> String
    showPalindrome str = show (palindrome str) ++ "\n"
