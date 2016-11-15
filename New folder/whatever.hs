x=4

k :: Int -> Int
k z = z + 10
-- k(z) { return z + 10; }

g :: Int -> Int -> Int
g a b = a * b + 23

h :: (Int -> Int) -> Int
h p = p 99

(***) :: Int -> Int -> Int
(***) a b = a * b

data Shape =
    Circle {
        radius :: Int
    } | Square {
        side :: Int
    } | Rectangle {
        width :: Int
        , height :: Int
    }
    deriving (Eq, Show)

pie = 3

perimeter :: Shape -> Int
perimeter (Circle r) = r * 2 * pie
perimeter (Square s) = s * 4
perimeter (Rectangle w h) = (w + h) * 2

data Order =
  LessThan
  | EqualTo
  | GreaterThan
  deriving (Eq, Show)

class Ordered a where
  order :: a -> a -> Order

instance Ordered Bool where
  order False False = EqualTo
  order True True = EqualTo
  order False True = LessThan
  order True False = GreaterThan

instance Ordered Int where
  order x y =
    if x == y then EqualTo
    else if x < y then LessThan
    else GreaterThan

sort :: Ordered a => [a] => [a]
sort = undefined

wx :: [Int]
wx = sort [1,2,3]

