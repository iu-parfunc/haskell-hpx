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
    spec <- newActionSpec printThatStringSP
    withHPX [spec] $ do
        _r <- callCC here "Hello, World!" printThatStringSP
        liftIO $ putStrLn "The show's over (or has it just begun?)"
