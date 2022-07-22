module RemoveElem

import Data.Vect

export
removeElem_v1 : DecEq a => (value : a) -> 
    (xs: Vect (S k) a) -> 
    (prf: Elem value xs) -> 
    Vect k a 
removeElem_v1 value (value :: ys) Here = ys
removeElem_v1 {k = Z} value (y :: []) (There later) = absurd later
removeElem_v1 {k = (S n)} value (y :: ys) (There later) = y :: removeElem_v1 value ys later

removeElem : DecEq a => (value : a) -> 
    (xs: Vect (S k) a) -> 
    {auto prf: Elem value xs} -> 
    Vect k a 
removeElem value (value :: ys) {prf = Here} = ys
removeElem {k = Z} value (y :: []) {prf = There later} = absurd later
removeElem {k = (S n)} value (y :: ys) {prf = There later} = y :: removeElem value ys

removeElem_auto : DecEq a => (value : a) -> 
    (xs : Vect (S k) a) -> 
    {auto prf : Elem value xs} -> 
    Vect k a
removeElem_auto {prf} value xs = removeElem_v1 value xs prf