interface MyEq ty where
    eq : ty -> ty -> Bool
    neq: ty -> ty -> Bool

interface MyFunctor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b


    