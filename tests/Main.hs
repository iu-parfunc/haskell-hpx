module Main (main) where

import Foreign.HPX
import Fibonacci (fibonacci)
import HelloWorld (helloWorld)

main :: IO ()
main = withHPX $ \_args -> do
  putStrLn "TESTS BEGIN"
--   helloWorld
  fibonacci
  return ()
