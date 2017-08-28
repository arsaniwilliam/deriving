{-# LANGUAGE ExistentialQuantification #-}
module Main where

import           Data.Monoid ((<>))
import           Lib

main :: IO ()
main = do
  print . getLogs $ logGCD 9293 13241
  print . getLogs $ logGCD 1000 50
  print . getLogs $ logGCD 256 12


simpleGCD :: (Integral a) => a -> a -> a
simpleGCD a b
  | a < b = simpleGCD b a
  | a `mod` b == 0 = b
  | otherwise = simpleGCD b (a `mod` b)


logGCD :: (Integral a, Show a) => a -> a -> Log [String] a
logGCD a b
  | a < b = logGCD b a
  | a `mod` b == 0 = let
      msg = show a ++ " mod " ++ show b ++ " is 0. GCD is " ++ show b
      in Log [msg] b
  | otherwise = let
      res = a `mod` b
      msg = show a ++ " mod " ++ show b ++ " is " ++ show res
      in Log [msg] b >> logGCD b res

data Log m b = Monoid m => Log { getLogs :: m, getValue :: b }
instance (Show a, Show b) => Show (Log a b) where
  show (Log a b) = show a ++ "\n" ++ show b

instance Functor (Log m) where
  fmap f (Log l v) =  Log l (f v)

instance Monoid m => Applicative (Log m) where
  pure = Log mempty
  (<*>) (Log m1 f) (Log m2 v) = Log (m1 <> m2) (f v)

instance Monoid m => Monad (Log m) where
  return = pure
  (>>=) (Log m1 v1) f = let
    Log m2 v2 = f v1
    in Log (m1 <> m2) v2

logMsg :: a -> Log () a
logMsg = Log ()
