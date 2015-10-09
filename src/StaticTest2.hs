-- | A test of static expressions used across different modules

{-# LANGUAGE StaticPointers #-}

module StaticTest2 where
import GHC.StaticPtr

sqr :: Int -> Int
sqr y = y * y

x :: StaticPtr (Int -> Int)
x = static sqr
