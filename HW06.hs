{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a:streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a s) = Cons (f a) (f <$> s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat n = Cons n (sRepeat n)

sIterate :: (a -> a) -> a -> Stream a
sIterate f n = Cons n (sIterate f (f n))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons n ns) b = Cons n (sInterleave b ns)

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = (floor . logBase (2 :: Double) . fromIntegral) <$> nats

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate ((`mod` 2147483648) . (12345 +) . (1103515245 *))

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 235 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just (foldl' go (x, x) xs)
    where go (a, b) n = a `seq` b `seq` (min a n, max b n)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
