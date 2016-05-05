module Main (main) where

import Foreign.HPX

import qualified Fibonacci
import qualified HelloWorld

actions :: [ActionSpec]
actions = HelloWorld.actions
       ++ Fibonacci.actions

main :: IO ()
main = withHPXs actions
    [ HelloWorld.main
    , HelloWorld.main
    , Fibonacci.main
    ]
