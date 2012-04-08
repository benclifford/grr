-- (C) Copyright 2012 CQX Limited

{-# LANGUAGE ScopedTypeVariables  #-}

module DNSOps where

  import qualified Network.DNS as DNS
  import Network.DNS hiding (lookup)
  import System.IO
  import System.IO.Unsafe

  defaultrs = unsafePerformIO $ makeResolvSeed defaultResolvConf

  data DNSLookup a = DNSLookup (IO a)

  instance Monad DNSLookup where
    return a = DNSLookup (return a)
    (DNSLookup v :: DNSLookup a) >>= (f :: a -> DNSLookup b) = DNSLookup $ do
       r <- v
       let (DNSLookup act) = f r
       act

  dnslookupDefault name rrtype = DNSLookup $ withResolver defaultrs $
    \resolver -> DNS.lookup resolver name rrtype

-- these two mean to query only a specific DNS server for a value
  dnslookup nsip name rrtype = DNSLookup $ do
    let phn = RCHostName (show nsip)
    res <- makeResolvSeed (ResolvConf phn 3000000 512)
    withResolver res $ \resolver -> DNS.lookup resolver name rrtype

  dnslookupRaw nsip name rrtype = DNSLookup $ do
    let phn = RCHostName (show nsip)
    res <- makeResolvSeed (ResolvConf phn 3000000 512)
    withResolver res $ \resolver -> DNS.lookupRaw resolver name rrtype

  debugline s = DNSLookup $ hPutStrLn stderr s

  runLookup :: DNSLookup a -> IO a
  runLookup (DNSLookup action) = action

