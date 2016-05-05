module Main (main) where

import Foreign.HPX

-- import qualified Fibonacci
import qualified HelloWorld

main :: IO ()
main = withHPXs ({- Fibonacci.actions ++ -} HelloWorld.actions)
    [ HelloWorld.main
    , HelloWorld.main
    -- , Fibonacci.actions
    ]
