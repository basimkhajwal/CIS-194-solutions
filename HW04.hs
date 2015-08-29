{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List (intercalate)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P m) (P n) = m == n

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P n) = (intercalate " + " . filter (not . null). snd) $ foldl (\(size, val) coeff -> (size + 1, calculateNext size coeff:val )) (0 :: Integer, []) n
        where calculateNext size val
                | val == 0      = ""
                | size == 0     = show val
                | size == 1     = (if val == 1 then "" else show val) ++ "x"
                | otherwise     = (if val == 1 then "" else show val) ++ "x" ++ '^':show size

-- Exercise 4 -----------------------------------------

zipWithExcess :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithExcess _ [] b = b
zipWithExcess _ a [] = a
zipWithExcess f (a:as) (b:bs) = f a b : zipWithExcess f as bs


plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ zipWithExcess (+) a b

-- Exercise 5 -----------------------------------------

generateFoil :: (Num a) => [a] -> [a] -> [[a]]
generateFoil a b = snd $ foldl (\(currentB, currentVal) val -> (0:currentB, map (val*) currentB :currentVal )) (b, []) a

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = foldr (plus . P) (P []) (generateFoil a b)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P $ map negate xs
    fromInteger val = P [fromInteger val]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P xs) val = snd $ foldl (\(degree, total) coeff -> (degree + 1, total + coeff * val ^ degree)) (0 :: Integer, 0) xs

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n val = iterate deriv val !! n

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P xs) = (P . tail) $ zipWith (*) xs (map fromInteger [0..])
