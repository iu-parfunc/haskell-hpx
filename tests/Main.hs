module Main (main) where

import Foreign.HPX
import HelloWorld (helloWorld)

main :: IO ()
main = withHPX $ \_args -> do
  helloWorld
  return ()
