data SnocList : List a -> Type where
    Empty : SnocList []
    Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (acc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelper {input} acc [] = rewrite appendNilRightNeutral input in acc
snocListHelper {input} acc (x :: xs) = rewrite appendAssociative input [x] xs in 
    snocListHelper (Snoc acc {x}) xs

snocList : (input : List a) -> SnocList input
snocList input = snocListHelper Empty input

myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverseViaHelper : (input : List a) -> List a
myReverseViaHelper input = myReverseHelper input (snocList input)

myReverse : (input : List a) -> List a
myReverse input with (snocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc rec) = x :: myReverse xs | rec
