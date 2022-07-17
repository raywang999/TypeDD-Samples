data Format = Number Format
    | Str Format
    | Lit String Format
    | End
    
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

PrintfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
PrintfFmt (Number fmt) acc = \i => PrintfFmt fmt (acc ++ (show i)) 
PrintfFmt (Str fmt) acc = \str => PrintfFmt fmt (acc ++ str)
PrintfFmt (Lit str fmt) acc = PrintfFmt fmt (acc ++ str)
PrintfFmt End acc = acc


toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: cs) = Number (toFormat cs)
toFormat ('%' :: 's' :: cs) = Str (toFormat cs)
toFormat (c :: cs) = case toFormat cs of
                          (Lit lit fmt) => Lit (strCons c lit) fmt
                          fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt)) 
printf fmt = PrintfFmt _ ""
