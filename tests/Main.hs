module Main (main) where

import Foreign.HPX
import HelloWorld (helloWorld)

main :: IO ()
main = withHPX $ \args -> do
  print args
  helloWorld
  return ()
