-- import Data.IntMap (insert)

-- import qualified Data.Tree
-- module Main where

-- e = 2.7182818284590452353602874713527
-- e = (\x -> (1 + 1/x) ** x) 1000000000


-- main :: IO ()
-- main = do
--     print e 
--     print (foldl (\acc (x,y,z,w) -> x+y+z+w : acc) [] [(2,2,5,2),(3,2,5,2),(3,2,3,2)])

-- infixr 5 >>>
-- (>>>) a b = a * b + 10

-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)

-- class Maybe m where  
--     isJust :: m -> Bool  
--     isNothing :: m -> Bool  
--     isJust Nothing = False  
--     isJust (Just _) = True  
--     isNothing Nothing = True  
--     isNothing (Just _) = False

-- instance (Main.Eq m) => Main.Eq (Main.Maybe m) where  
--     Just x == Just y = x == y  
--     Nothing == Nothing = True  
--     _ == _ = False  


-- class YesNo a where
--     yesno :: a -> Bool


-- instance (Num a) => YesNo a where
--     yesno 0 = False
--     yesno _ = True

-- instance YesNo [a] where
--     yesno [] = False
--     yesno _ = True

-- instance YesNo Bool where
--     yesno = id

-- instance YesNo (Maybe a) where
--     yesno (Just _) = True
--     yesno Nothing = False

-- instance YesNo (Data.Tree.Tree a) where
--     yesno (Data.Tree.Node _ []) = False
--     yesno _ = True


-- data Bool = True | False deriving (Show)

-- class Tofu t where  
--     tofu :: j a -> t a j  

-- class A a where
--     a :: a -> Int

-- class (A a) => B a where
--     b :: a -> String 

-- instance A Int where
--     a x = x

-- instance B Int where
--     b = show



-- main =
--     let a = Main.True in
--     print a

-- factorial 1 = 1;
-- factorial x = x * factorial (x-1)

-- formula x = ((x ** x) / factorial x) ** recip x


-- factorial 0 = 1
-- factorial x = x * factorial (x-1)

-- formula x = x / (factorial x ** recip x)
-- import sleep

-- module Main where

-- import System.IO
-- import System.Posix (sleep)
-- import Control.Concurrent


-- main :: IO ()
-- main = do  
--     threadDelay 1000000
--     foo <- putStrLn "Hello, what's your name?"  
--     name <- getLine  
--     threadDelay 1000000
--     putStrLn ("Hey " ++ name ++ ", you rock!")  


-- module Main where
-- import Data.Char
-- import Control.Monad.Trans.RWS.Lazy (put)

-- main =
--     let a = reads "1a23a3a" :: Int
--     in print a

-- fun2 x = putStrLn "meowmeow"

-- fun x = do
--     a <- fun2 5
--     putStrLn "asf"

module Main where

import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        main  