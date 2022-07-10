module Main

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

main : IO ()
main = repl "Enter a String: " showCounts where
    showCounts : String -> String
    showCounts str = show (counts str) ++ "\n"