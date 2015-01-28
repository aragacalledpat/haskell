import Data.Ord
import Data.List

third (_,_,x) = x

combos  = [(x,y,z) | x <- [-999..999], y <-[-999..999], let z = primeTime (x,y) ]

primeTime (x,y) = primeListLength 0 x y

primeListLength n a b
  | not (isPrime n a b) = n
  | otherwise = primeListLength (n+1) a b

isPrime  n
  | n > 1 =  null ([x | x <- [2..n-1], (mod n x) == 0])
  | otherwise = False

formula n a b = n * n + a*n + b
