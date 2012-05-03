-- (C) Copyright 2012 CQX Limited

{-# LANGUAGE ScopedTypeVariables  #-}

module DNSOps where

  import Control.Applicative
  import qualified Network.DNS as DNS
  import Network.DNS hiding (lookup)
  import System.IO
  import System.IO.Unsafe

  defaultrs = unsafePerformIO $ makeResolvSeed defaultResolvConf

  data DNSLookup a = DNSLookupUnsafeIOAction (IO a)

  instance Monad DNSLookup where
    return a = DNSLookupUnsafeIOAction (return a)
    (DNSLookupUnsafeIOAction v :: DNSLookup a) >>= (f :: a -> DNSLookup b) = DNSLookupUnsafeIOAction $ do
       r <- v
       let (DNSLookupUnsafeIOAction act) = f r
       act

  instance Functor DNSLookup where
    fmap (f :: u -> v)  (a :: DNSLookup u) = do
      x <- a
      return (f x)

-- This is rfc 1034 s5.2.1 option 3 'General Lookup Function'.
-- It asks "the DNS as a whole" to lookup name,rrtype
-- At present, it only asks the default resolver, but what I want
-- eventually is a full DNS lookup with branching and loop-back to happen.

  queryDNS name rrtype = maybeListToList <$> (DNSLookupUnsafeIOAction $ withResolver defaultrs $ \resolver -> DNS.lookup resolver name rrtype)

-- these two mean to query only a specific DNS server for a value.
-- whatever they do with the output, they need to perform appropriate
-- cache and loopback behaviour on the received results.
-- they are lower level than queryDNS - they are directed at a
-- specific server and won't go off chasing referals.

  queryServerRaw nsip name rrtype = DNSLookupUnsafeIOAction $ do
    let phn = RCHostName (show nsip)
    res <- makeResolvSeed (ResolvConf phn 3000000 512)
    withResolver res $ \resolver -> DNS.lookupRaw resolver name rrtype


  -- for monadic niceness, is there a way to rephrase this case as something
  -- using Maybe as a monad?
  queryServer nsip name rrtype = do
    r <- queryServerRaw nsip name rrtype
    case r of
      Nothing -> return Nothing
      Just (DNSFormat _ _ answers _ _) -> return $ Just $ map (\(ResourceRecord _ _ _ _ r) -> r) answers

  debugline s = DNSLookupUnsafeIOAction $ hPutStrLn stderr s

  runLookup :: DNSLookup a -> IO a
  runLookup (DNSLookupUnsafeIOAction action) = action

  maybeListToList :: Maybe [a] -> [a]
  maybeListToList (Nothing) = []
  maybeListToList (Just l) = l

