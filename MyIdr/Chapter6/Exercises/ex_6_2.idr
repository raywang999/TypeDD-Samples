-- Exercise 1

import Data.Vect

Matrix : (rows : Nat) -> (cols: Nat) -> Type
Matrix rows cols = Vect rows (Vect cols Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- Exercise 2
data Format = Number Format
    | Str Format
    | Lit String Format
    | Chr Format
    | Dbl Format
    | End
    
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Chr fmt) = (char : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (double : Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

PrintfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
PrintfFmt (Number fmt) acc = \i => PrintfFmt fmt (acc ++ (show i)) 
PrintfFmt (Str fmt) acc = \str => PrintfFmt fmt (acc ++ str)
PrintfFmt (Chr fmt) acc = \char => PrintfFmt fmt (strCons '\'' (strCons char ("'" ++ acc)))
PrintfFmt (Dbl fmt) acc = \double => PrintfFmt fmt (acc ++ (show double))
PrintfFmt (Lit str fmt) acc = PrintfFmt fmt (acc ++ str)
PrintfFmt End acc = acc


toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: cs) = Number (toFormat cs)
toFormat ('%' :: 's' :: cs) = Str (toFormat cs)
toFormat ('%' :: 'c' :: cs) = Chr (toFormat cs)
toFormat ('%' :: 'f' :: cs) = Dbl (toFormat cs)
toFormat (c :: cs) = case toFormat cs of
                          (Lit lit fmt) => Lit (strCons c lit) fmt
                          fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt)) 
printf fmt = PrintfFmt _ ""

-- Exercise 3
TupleVect : (len : Nat) -> Type -> Type
TupleVect Z _ = ()
TupleVect (S k) a = (a, TupleVect k a)

test : TupleVect 4 Nat
test = (1,2,3,4,())
