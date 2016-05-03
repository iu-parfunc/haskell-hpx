module Main (main) where

import Data.Foldable (for_)
import Foreign.HPX

import qualified HelloWorld

type Test = ([ActionSpec], HPX ())

tests :: [Test]
tests = [ (HelloWorld.actions, HelloWorld.main)
        ]

main :: IO ()
main = for_ tests $ uncurry withHPX
