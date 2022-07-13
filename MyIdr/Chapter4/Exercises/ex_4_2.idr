import Data.Vect

data PowerSource = Petrol | Pedal 
    | Battery -- Exercise 2

data Vehicle : PowerSource -> Type where
    Unicycle : Vehicle Pedal -- Exercise 1
    Bicycle : Vehicle Pedal
    Car : (fuel : Double) -> Vehicle Petrol
    Bus : (fuel : Double) -> Vehicle Petrol
    Motorcycle : (fuel : Double) -> Vehicle Petrol -- Exercise 1
    Tram : (charge : Double) -> Vehicle Battery -- Exercise 2
    EV: (charge : Double) -> Vehicle Battery -- Exercise 2

wheels : Vehicle power -> Nat 
wheels Bicycle = 2
wheels Unicycle = 1 -- Exercise 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2 -- Exercise 1
wheels (Tram charge) = 8 -- Exercise 2
wheels (EV charge) = 4 -- Exercise 2

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50 -- Exercise 1

-- Exercise 3
vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a -- No way I would have gotten that
-- Exercise 4: Define vectTake
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         (Just idx) => Just (index idx xs)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = let i = integerToFin pos n in
    case i of 
        Nothing => Nothing
        (Just idx) => Just (index idx xs + index idx ys)
