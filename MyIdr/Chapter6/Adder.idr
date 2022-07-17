AdderType_v1 : (numargs : Nat) -> Type
AdderType_v1 Z = Int
AdderType_v1 (S k) = (next : Int) -> AdderType_v1 k

adder_v1 : (numargs : Nat) -> (acc : Int) -> AdderType_v1 numargs
adder_v1 Z acc = acc
adder_v1 (S k) acc = \num => adder_v1 k (acc + num)

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = numType -> AdderType k numType

adder : (Num numType) => (numargs : Nat) -> (acc : numType) -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (acc + next)
