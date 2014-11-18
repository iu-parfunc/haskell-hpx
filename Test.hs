

import Hpx

main :: IO ()
main = do
  putStrLn "Call hpx init..."
  c <- hpxInit []
  putStrLn$  "Done with hpx init, returned "++ show c
