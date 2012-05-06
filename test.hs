
{-# LANGUAGE ScopedTypeVariables #-}

import QFS

main = do
  putStrLn "dnsloops test program"
  let (v1 :: QFSState Double Double) = emptyQFS
  print v1
  let v2 = pushResult v1 (2, 1.41)
  print v2
  let v3 = pushResult v2 (16, -4)
  print v3
  let v4 = pushResult v3 (2, -1.41)
  print v4
  let v5 = pushResult v4 (16, -4)
  print v5
  putStrLn "done"

