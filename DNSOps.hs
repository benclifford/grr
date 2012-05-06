-- (C) Copyright 2012 CQX Limited

{-# LANGUAGE ScopedTypeVariables  #-}

module DNSOps where

  import Control.Applicative
  import Control.Monad.List
  import Control.Monad.Trans
  import qualified Network.DNS as DNS
  import Network.DNS hiding (lookup)
  import System.IO
  import System.IO.Error
  import System.IO.Unsafe

  defaultrs = unsafePerformIO $ makeResolvSeed defaultResolvConf

-- DNSLookup is a monad in which DNS lookups can be performed.
-- For now, it is a straightforward wrapper around IO.

-- What I want to do is:
--   i) non-determinism - probably using ListT
--      non-determinism might happen in a few places. At present, a query
--      returns an RRset. But I'd like a non-deterministic query that returns
--      a single RR.
--  ii) caching with state - StateT?
-- iii) loopback of cache updates - this is a bit novel

-- At the moment, whenever a query to a server returns nothing/Nothing,
-- I'm giving it uniform treatment. But I should perhaps distinguish between
-- the various protocol-level nothings being returns - is it that the server
-- is broken (in which case the protocol would skip onto next server) or is
-- it that these are real 'empty' results which should remain like that in
-- the cache?

  type DNSLookup a = ListT BaseDNSMonad a

  data BaseDNSMonad a = DNSLookupUnsafeIOAction (IO a)


  instance Monad BaseDNSMonad where
    return a = DNSLookupUnsafeIOAction (return a)
    (DNSLookupUnsafeIOAction v :: BaseDNSMonad a) >>= (f :: a -> BaseDNSMonad b) = DNSLookupUnsafeIOAction $ do
       r <- v
       let (DNSLookupUnsafeIOAction act) = f r
       act

  instance Functor BaseDNSMonad where
    fmap (f :: u -> v)  (a :: BaseDNSMonad u) = do
      x <- a
      return (f x)

-- This is rfc 1034 s5.2.1 option 3 'General Lookup Function'.
-- It asks "the DNS as a whole" to lookup name,rrtype
-- At present, it only asks the default resolver, but what I want
-- eventually is a full DNS lookup with branching and loop-back to happen.

  queryDNS name rrtype = lift (maybeListToList <$> (DNSLookupUnsafeIOAction $ maybeIO $ withResolver defaultrs $ \resolver -> DNS.lookup resolver name rrtype))

  -- this returns a single RR from the queried RRset
  -- at the moment, it will fail with a pattern match failure when the
  -- RRset is empty. later on, this should turn into non-deterministic
  -- list-monad-like behaviour.
  queryDNSForSingleRR name rrtype = do
    r <- queryDNS name rrtype
    debugline $ "Going all non-deterministic for " ++ (show name)

    -- below non-determinism magic comes from:
    -- http://stackoverflow.com/questions/6397672/how-to-get-backtracking-and-io-using-listt
    ndr <- ListT (return r)

    return ndr


-- these two mean to query only a specific DNS server for a value.
-- whatever they do with the output, they need to perform appropriate
-- cache and loopback behaviour on the received results.
-- they are lower level than queryDNS - they are directed at a
-- specific server and won't go off chasing referals.

  queryServerRaw nsip name rrtype = lift $ DNSLookupUnsafeIOAction $ do
    let phn = RCHostName (show nsip)
    res <- makeResolvSeed (ResolvConf phn 3000000 512)
    maybeIO $ withResolver res $ \resolver -> DNS.lookupRaw resolver name rrtype


  -- for monadic niceness, is there a way to rephrase this case as something
  -- using Maybe as a monad?
  queryServer nsip name rrtype = do
    r <- queryServerRaw nsip name rrtype
    case r of
      Nothing -> return Nothing
      Just (DNSFormat _ _ answers _ _) -> return $ Just $ map (\(ResourceRecord _ _ _ _ r) -> r) answers

  debugline s = lift $ DNSLookupUnsafeIOAction $ hPutStrLn stderr s

  runLookup b = runBaseLookup (runListT b)

  runBaseLookup :: BaseDNSMonad a -> IO a
  runBaseLookup (DNSLookupUnsafeIOAction action) = action

  maybeListToList :: Maybe [a] -> [a]
  maybeListToList (Nothing) = []
  maybeListToList (Just l) = l

  -- | runs an IO operation which returns a Maybe, and if it
  --   fails, return a Nothing; otherwise return whatever the
  --   IO operation returned (which might itself be a maybe)
  --   The error is printed to stdout (though maybe should go to
  --   some other debug channel)
  maybeIO :: IO (Maybe t) -> IO (Maybe t)
  maybeIO op = catchIOError op (\e -> print e >> return Nothing)

-- the abstraction for IPv4 and v6 address queries
  queryDNSForAddress name = do
-- warning for if/when queryDNS becomes non-deterministic
-- the v6 query shouldn't need to happen repeatedly for every v4 result...
-- I want these to be parallel (so something to do with mplus?)
    v4 <- queryDNS name A
    v6 <- queryDNS name AAAA
--    let v6 = []  -- disable IPv6 because I'm not doing error handling right and I get lots of errors on ipv6 that I don't know how I want to handle...
    ndr <- ListT (return $ v4 ++ v6)
    return ndr

