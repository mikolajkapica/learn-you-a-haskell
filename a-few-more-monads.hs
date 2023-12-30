-- use Writer Monad to log the number of times a function is called

import Control.Monad.Writer
import Data.Bifunctor (second)
import GHC.Iface.Ext.Debug (Diff)

countdown :: Int -> Writer [String] Int
countdown n = do
    tell [show n]
    countdown (n-1)

-- main = runWriter $ 
--     tell ["hey"] 
--     >> return (2 + 3) 
--     >>= \x -> tell [show x]
--     >> return (x - 1)

gcd' :: Int -> Int -> (Int, [String])
gcd' a b = let sol = runWriter $ actualGcd a b in Data.Bifunctor.second fromDiffList sol where
    actualGcd a b
        | b == 0    = do
            tell (toDiffList ["Finished with " ++ show a])
            return a
        | otherwise = do
            tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
            actualGcd b (a `mod` b)

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



