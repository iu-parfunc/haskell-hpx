{-# LANGUAGE StaticPointers #-}


import GHC.StaticPtr
import Foreign.Ptr


foo :: IO ()
foo = putStrLn "hello world"


bar :: StaticPtr (IO ())
bar = static (foo >> putStrLn "more")

foreign import ccall "wrapper"
   mkIOAction :: IO () -> IO (FunPtr (IO ()))

foreign import ccall "dynamic" 
   mkHsAction :: FunPtr (IO ()) -> IO ()

main = 
 do 
    let key = staticKey bar
    putStrLn $ "Got key "++ show key
    putStrLn "Running directly:"
    deRefStaticPtr bar

    putStrLn "\nRunning after going through key:"
    Just baz <- unsafeLookupStaticPtr key
    deRefStaticPtr baz

    putStrLn "\nRunning through funptr:"
    fptr <- mkIOAction foo
    mkHsAction fptr

    putStrLn "Done."
