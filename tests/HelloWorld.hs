{-# LANGUAGE StaticPointers #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.HPX
import GHC.StaticPtr (StaticPtr)

printThatString :: String -> HPX ()
printThatString str = do
    liftIO (putStrLn str)
    threadContinue "World, Hello!"

printThatStringSP :: StaticPtr (String -> HPX ())
printThatStringSP = static printThatString

main :: IO ()
main = do
    withHPX [actionSpec printThatStringSP] $
        callCC here "Hello, World!" printThatStringSP
    putStrLn "The show's over"
