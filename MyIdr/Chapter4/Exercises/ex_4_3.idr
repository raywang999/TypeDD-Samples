module Main

import Data.Vect

data DataStore : Type where
    MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items) where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
    | Get Integer 
    | Size -- Exercise 1
    | Search String -- Exercise 2
    | Quit


parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" ind = case all isDigit (unpack ind) of
                              False => Nothing
                              True => Just (Get (cast ind))
parseCommand "size" "" = Just Size -- Exercise 1
parseCommand "search" substr = Just (Search substr) -- Exercise 2
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input: String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
    case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          (Just ind) => Just (index ind store_items ++ "\n", store)

          -- Exercise 2
searchStore_2 : (substr : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchStore_2 substr store = let store_items = items store in
    Just (show (filter (isInfixOf substr) store_items), store)


searchList : (id: Integer) -> (substr : String) -> (store_items : Vect n String) -> String
searchList id substr [] = ""
searchList id substr (x :: xs) = let rest = searchList (id + 1) substr xs in
    if isInfixOf substr x then show id ++ ": " ++ x ++ "\n" ++ rest 
    else rest

          -- Exercise 3
searchStore : (substr : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchStore substr store = let 
    store_items = items store in
        Just (searchList 0 substr store_items, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invallid Command\n", store)
                              (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store 
                              (Just Size) => Just ("Size is: " ++ show (size store) ++ "\n", store)
                              (Just (Search substr)) => searchStore substr store 
                              (Just Quit) => Nothing


main : IO ()
main = replWith (MkData 0 []) "Command: " processInput
