import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty           

listItems : DataStore schema -> List (SchemaType schema)
listItems store with (storeView store)
  listItems empty | SNil = []
  listItems (addToStore value rest) | (SAdd rec) 
    = value :: listItems rest | rec

filterKeys : (test : SchemaType val_schema -> Bool) -> 
    DataStore (SString .+. val_schema) -> List String
filterKeys test store with (storeView store)
  filterKeys test empty | SNil = []
  filterKeys test (addToStore (key, value) rest) | (SAdd rec) 
    = case test value of
           False => filterKeys test rest | rec
           True => key :: filterKeys test rest | rec
