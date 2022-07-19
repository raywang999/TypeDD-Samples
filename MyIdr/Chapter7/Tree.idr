data Tree elem = Empty 
    | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty =  Empty
  map func (Node left elem right) = Node (map func left) (func elem) (map func right)

Foldable Tree where  
  foldr func init Empty = init
  foldr func init (Node left e right) = let 
    lfold = foldr func init left
    rfold = foldr func lfold right in
        func e rfold
  foldl func init Empty = init
  foldl func init (Node left e right) = let 
    lfold = foldl func init left in
        foldl func (func lfold e) right
