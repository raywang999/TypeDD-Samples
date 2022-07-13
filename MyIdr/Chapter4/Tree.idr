data Tree elem = Empty
    | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2
insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left y right) = case compare x y of
                                    LT => (insert x left) y right
                                    EQ => Node left y right
                                    GT => left y (insert x right)

