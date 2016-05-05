{-# LANGUAGE StaticPointers #-}
module HelloWorld (actions, main) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.HPX
import GHC.StaticPtr (StaticPtr)

printThatString :: String -> HPX ()
printThatString = liftIO . putStrLn

printThatStringSP :: StaticPtr (String -> HPX ())
printThatStringSP = static printThatString

actions :: [ActionSpec]
actions = [actionSpec printThatStringSP]

main :: HPX ()
main = callCC Here "Hello, World!" printThatStringSP
