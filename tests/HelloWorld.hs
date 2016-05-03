{-# LANGUAGE StaticPointers #-}
module HelloWorld (actions, main) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.HPX
import GHC.StaticPtr (StaticPtr)

printThatString :: String -> HPX ()
printThatString str = do
    liftIO (putStrLn str)
    threadContinue "World, Hello!"

printThatStringSP :: StaticPtr (String -> HPX ())
printThatStringSP = static printThatString

actions :: [ActionSpec]
actions = [actionSpec printThatStringSP]

main :: HPX ()
main = callCC here "Hello, World!" printThatStringSP
