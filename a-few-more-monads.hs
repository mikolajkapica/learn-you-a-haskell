-- use Writer Monad to log the number of times a function is called

import Control.Monad.Writer
import Data.Bifunctor (second)
import GHC.Iface.Ext.Debug (Diff)
import System.Clock (getTime, Clock(Monotonic))

countdown :: Int -> Writer [String] Int
countdown n = do
    tell [show n]
    countdown (n-1)

-- main = runWriter $ 
--     tell ["hey"] 
--     >> return (2 + 3) 
--     >>= \x -> tell [show x]
--     >> return (x - 1)

gcd'' :: Int -> Int -> (Int, [String])
gcd'' a b = let sol = runWriter $ actualGcd a b in Data.Bifunctor.second fromDiffList sol where
    actualGcd a b
        | b == 0    = do
            tell (toDiffList ["Finished with " ++ show a])
            return a
        | otherwise = do
            tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
            actualGcd b (a `mod` b)

gcd' :: Int -> Int -> (Int, [String])
gcd' a b = let sol = runWriter $ actualGcd a b in Data.Bifunctor.second fromDiffList sol where
    actualGcd a b
        | b == 0    = do
            tell (toDiffList ["Finished with " ++ show a])
            return a
        | otherwise = do
            result <- actualGcd b (a `mod` b)
            tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
            return result

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]

timeit f x = do
    start <- getTime Monotonic
    result <- f x
    end <- getTime Monotonic
    return (result, end - start)

main = do
    putStrLn "finalCountDown 5000 List"
    print (snd $ timeit runWriter (finalCountDown 5000))
    return ()

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)

(|>) x f = f x