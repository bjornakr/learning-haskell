import Data.Char

string1 = "hello"
string2 = "world"
greeting = string1 ++ " " ++ string2

maxl::[Int] -> Int
maxl (x:y:z:xs) = max x (maxl (y:z:xs))
maxl (x:y:[]) = max x y
maxl (x:[]) = x
maxl [] = error "empty list"

posOrNeg x =
    if x >= 0
    then "Positive"
    else "Negative"

purez x =
    if x == 1
    then "Cannot modify state"
    else if x == 2
         then "Cannot depend on state"
         else if x == 3
              then "Given the same arguments, always returns the same output"
              else "USAGE: pure 1-3"

purex 1 = "Cannot modify state"
purex 2 = "Cannot depend on state"
purex 3 = "Given the same arguments, always returns the same output"
purex x = "USAGE: purex 1-3"

purexz n
    | n == 1 = "Cannot modify state"
    | n == 2 = "Cannot depend on state"
    | n == 3 = "Given the same arguments, always returns the same output"
    | otherwise = "USAGE: purex 1-3"

purexx n = case n of
    1 -> "Cannot modify state"
    2 -> "Cannot depend on state"
    3 -> "Given the same arguments, always returns the same output"
    _ -> "USAGE: purex 1-3"


-- bottom up
helloLet =
    let 
        a :: String
        a = "hola"
        b :: String
        b = "world" in
    a ++ " " ++ b

-- top down
helloWhere = "hello " ++ a
    where 
        a :: String
        a = "world"

helloHo world modfy =
    "Hello " ++ (modfy world)

helloShout = helloHo "world" (map toUpper)

toUpperString s = map (\ c -> toUpper c) s
toUpperStringx s = (map toUpper) s

doublex [] = []
doublex (x:xs) = 2*x : doublex xs

doubleSum = sum . doublex




-- TOYPS

-- TYPE SYNONYM
-- Used in type definitions for readability.
-- Compiler not involved.
type Point = (Double, Double)

-- NEWTYPE
-- Create a new type represented by an existing type.
-- Add meaning that is enforced by the compiler.
-- MakeCustomerId = Constructor
newtype CustomerId = MakeCustomerId Int

-- Getting the int
customerToInt :: CustomerId -> Int
customerToInt (MakeCustomerId i) = i

-- RECORD
-- Shitty data type, AVOID!!
data Customer = MakeCustomer
    {
        customerId  :: CustomerId,
        name        :: String,
        luckyNumber :: Int
    }

customerHarry = MakeCustomer
    {
       customerId  = MakeCustomerId 101,
       name        = "Harri",
      luckyNumber = 18
    }

customerHarryCorrected = customerHarry { name = "Harry" }


-- ALGEBRAIC DATA TYPES
-- (where it is at)

-- Newtype, but with more arguments
-- Tuples, but with names

data CustomerX = CustomerX CustomerId String Int
roger = CustomerX (MakeCustomerId 102) "Roger" 19

getCustomerXId :: CustomerX -> CustomerId
getCustomerXId (CustomerX customerId _ _) = customerId



type WeaponId = Int
type WeaponName = String
type WeaponDamage = Double

data Weapon = CreateWeapon WeaponId WeaponName WeaponDamage

instance Eq Weapon where
    (CreateWeapon id1 _ _) == (CreateWeapon id2 _ _) = (id1 == id2)

instance Show Weapon where
    show (CreateWeapon _ name damage) =
        "This is the mighty " ++ name ++ ", dealing " ++ (show damage) ++ " damage."

class Attack a where
    attack :: a -> String
    getDamage :: a -> WeaponDamage
    attack a = "You strike for " ++ (show $ getDamage a) ++ " damage."

instance Attack Weapon where
    getDamage (CreateWeapon _ _ damage) = damage


weapon1 = CreateWeapon 1 "Sword" 8.6
weapon2 = CreateWeapon 2 "Lance" 19.3
weapon1a = CreateWeapon 1 "Schword" 8.6


bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f' (gx, gs) = 
    let (fx, fs) = f' gx in
        (fx, gs ++ fs)