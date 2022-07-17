module Main

import Data.Vect

infixr 5 .+.

data Schema = SString 
            | SInt 
            | SChar 
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

data DataStore : Type where
    MkData : (schema : Schema) -> (size : Nat) -> (items : Vect size (SchemaType schema)) -> DataStore 

size : DataStore -> Nat
size (MkData schema size items) = size

schema : (store : DataStore) -> Schema
schema (MkData schema size items) = schema

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData schema size items) = items

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store) where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (item1, item2) = display item1 ++ ", " ++ (display item2)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
    case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          (Just id) => Just (display (index id (items store)) ++ "\n", store)

data Command : Schema -> Type where
    SetSchema : (newSchema : Schema) -> Command schema
    Add : SchemaType schema -> Command schema
    Get : Integer -> Command schema
    GetAll : Command schema
    Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = case unpack input of
                                 '"' :: cs => case span (/= '"') cs of
                                                    (str, '"' :: rest) => Just (pack str, ltrim (pack rest))
                                                    _ => Nothing
                                 _ => Nothing
parsePrefix SChar input = case unpack input of
                               c :: rest => Just (c, ltrim (pack rest))
                               _ => Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", _) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (x .+. y) input = do -- Exercise 3
    (xok, input') <- parsePrefix x input
    (yok, input'') <- parsePrefix y input'
    Just (((xok, yok), input''))
    
parseBySchema : (schema : Schema) -> (rest : String) -> Maybe (SchemaType schema)
parseBySchema schema rest = do --Exercise 3
    (res, rest') <- parsePrefix schema rest 
    (case rest' of
          "" => Just res
          _ => Nothing)

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: rest) = case rest of
                                      [] => Just SString
                                      _ => do --Exercise 3
                                        restSchema <- parseSchema rest
                                        Just (SString .+. restSchema)
parseSchema ("Int" :: rest) = case rest of
                                   [] => Just SInt
                                   _ => do --Exercise 3
                                    restSchema <- parseSchema rest 
                                    Just (SInt .+. restSchema)
parseSchema ("Char" :: rest) = case rest of
                                    [] => Just SChar
                                    _ => do --Exercise 3
                                        restSchema <- parseSchema rest 
                                        Just (SChar .+. restSchema)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = do --Exercise 3
    item <- parseBySchema schema rest 
    Just (Add item)
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" ind = case all isDigit (unpack ind) of
                              False => Nothing
                              True => Just (Get (cast ind))
parseCommand _ "schema" newSchemaStr = do --Exercise 3
    newSchema <- parseSchema (words newSchemaStr)
    Just (SetSchema newSchema)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input: String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

displayAll : Vect n (SchemaType schema) -> String
displayAll [] = ""
displayAll (item :: rest) = display item ++ "\n" ++ displayAll rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                              Nothing => Just ("Invalid Command\n", store)
                              (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store 
                              (Just GetAll) => Just (displayAll (items store), store)
                              (Just (SetSchema newSchema)) => (case size store of
                                                                    Z => Just ("OK\n", MkData newSchema _ [])
                                                                    (S k) => Just ("Store not empty, Can't update schema\n", store))
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData (SString .+. SInt) _ []) "Command: " processInput