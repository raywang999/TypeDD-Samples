data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num : Nat) -> EqNat num num 

SameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
SameS j j (Same j) = Same (S j)

checkEqNat_v1 : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat_v1 Z Z = Just (Same _)
checkEqNat_v1 (S k) (S j) = case checkEqNat_v1 k j of
                                 Nothing => Nothing
                                 (Just eq) => Just (SameS _ _ eq)
checkEqNat_v1 _ _ = Nothing

checkEqNat_v2 : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat_v2 Z Z = Just (Same _)
checkEqNat_v2 (S k) (S j) = do 
    Same x <- checkEqNat_v2 k j
    Just (Same (S x))
checkEqNat_v2 _ _ = Nothing

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat (S k) (S j) = do 
    prf <- checkEqNat k j
    Just (cong prf)
checkEqNat _ _ = Nothing

exactLength_v1 : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength_v1 {m} len input = case checkEqNat_v2 m len of
                                 Nothing => Nothing
                                 (Just (Same len)) => Just input

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 (Just Refl) => Just input
