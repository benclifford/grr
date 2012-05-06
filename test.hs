
{-# LANGUAGE ScopedTypeVariables #-}

import QFS

main = do
  putStrLn "dnsloops test program"
  let (v1 :: QFSState Double Double IO) = emptyQFS
  putStrLn $ statsForQFS v1
  v2 <- pushResult v1 (2, 1.41)
  putStrLn $ statsForQFS v2
  v3 <- pushResult v2 (16, -4)
  putStrLn $ statsForQFS v3
  v4 <- pushResult v3 (2, -1.41)
  putStrLn $ statsForQFS v4
  v5 <- pushResult v4 (16, -4)
  putStrLn $ statsForQFS v5
  putStrLn "done"

