{-# LANGUAGE ForeignFunctionInterface #-}


module Safe where

import Foreign.C.Types

fibonacci :: Int -> IO Int
fibonacci n =
  do -- putStrLn "Inside HS..."
     print_stack_pointer
     return 99
--     return $ fibs !! n
--  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> IO CInt
fibonacci_hs = fmap (fromIntegral) . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> IO CInt

foreign import ccall unsafe  "print_stack_pointer"
   print_stack_pointer :: IO ()
