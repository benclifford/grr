-- (C)Copyright 2012 CQX Limited
-- Not licensed for distribution

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.String
import qualified Data.ByteString.Char8 as BSChar
import qualified Network.DNS as DNS
import Network.DNS hiding (lookup)
import System.Environment
import System.Exit
import System.IO

import System.IO.Unsafe
-- unsafe is to get global resolver - this could feed into the state of a
-- resolver monad, though? (which should also be listy to give us tree-like
-- behaviour?)

maybeListToList :: Maybe [a] -> [a]
maybeListToList (Nothing) = []
maybeListToList (Just l) = l

-- for now, its ok if we get multiple default resolvers or one
-- they should behave similarly enough for what the code wants
-- right now
defaultrs = unsafePerformIO $ makeResolvSeed defaultResolvConf

data DNSLookup a = DNSLookup (IO a)

instance Monad DNSLookup where
  return a = DNSLookup (return a)
  (DNSLookup v :: DNSLookup a) >>= (f :: a -> DNSLookup b) = DNSLookup $ do
       r <- v
       let (DNSLookup act) = f r
       act

-- bind just unwraps and rewraps at the moment - nothing fancy at all.

-- operations that will be in our monad:

-- this means to apply the whole DNS lookup algorithm:
dnslookupDefault name rrtype = DNSLookup $ withResolver defaultrs $ \resolver ->
                                 DNS.lookup resolver name rrtype

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

reportSuccess w = DNSLookup $ (putStrLn ("NS OK - " ++ w) >> exitWith (ExitSuccess))
reportWarning w = DNSLookup $ (putStrLn ("NS WARNING - " ++ w) >> exitWith (ExitFailure 1))

runLookup :: DNSLookup a -> IO a
runLookup (DNSLookup action) = action

main = do
  putStrLn "monitor-dns"
  (domain :: String) <- head <$> (getArgs :: IO [String])
  runLookup (go domain)

go :: String -> DNSLookup ()
go domain = do
  debugline $ "domain to check: "++domain
  let parent = tail $ dropWhile (/= '.') domain
  debugline $ "parent of this domain: "++parent

  parentNSes <- dnslookupDefault (fromString parent) NS

  debugline "nameservers of parent: "
  debugline $ show parentNSes
  let (RD_NS aParentNS) = head $ maybeListToList parentNSes

  debugline $ "Nameserver we will use: "++(BSChar.unpack aParentNS)

  parentNS_As <- dnslookupDefault aParentNS A

  debugline $ "parent NS A RRset is "++(show parentNS_As)

  let (RD_A a) = head $ maybeListToList parentNS_As

  -- note, this will look up the IP(s) of parent NS outside of my controlled
  -- environment - perhaps later I should be doing the resolution of this
  -- to an IP address myself?
  debugline $ "a parent NS A record is " ++(show a)

  maybeHereNSresult <- dnslookupRaw a (fromString domain) NS

  debugline $ "maybeHereNSresult = " ++ (show maybeHereNSresult)
  let (Just (DNSFormat h q ans auth add)) = maybeHereNSresult

  let rdatas = [rdata x | x <- ans]
  let authrdatas = [rdata x | x <- auth, rrtype x == NS,
                                         rrname x == fromString (domain++".")]
  debugline $ "rdatas = " ++ (show rdatas)
  debugline $ "authrdatas = " ++ (show authrdatas)
  let hereNS = rdatas ++ authrdatas :: [RDATA]

  debugline "Name servers for this domain, according to parent: "
-- BUG: if we don't pick up any nameservers here, then we won't look at any
-- child nameservers, and we'll pass this test because we'll end up with
-- a list [[]] where all the entries are equal to each other (and to []).
-- Whilst strictly this test would accept that, its pretty much not what
-- I expect this tool to do. So I should check there is at least one NS
-- (or maybe two and give a warning if not at least two?)
  debugline $ show hereNS

  -- in a non-linear version of this, this is the main point I want to loop:
  --   name servers can come from multiple places: the delegation glue (which
  --   the above gets) but also from all of these name servers.
  -- When I become aware of a new nameserver for a zone, I need to re-run
  --   any queries against that zone (that means anything below that, because
  --   a zone cut is a non-deterministic thing in this model so I need to
  --   discover those per-server)
  -- That includes the query for the nameserver records themselves off each
  --   nameserver (which should be taken into account by the above mechanism
  --   without further action)

  -- in the linear version, I need to explicitly map over all of the above
  -- nameservers in hereNS

  nsFromAllNS <- forM hereNS $ \ns -> do
    debugline $ "Checking parent-supplied name server "++(show ns)
    parentNS_As <- dnslookupDefault (fromString $ show ns) A
    -- ^^ ICK - don't go via String
    debugline $ "parent NS A RRset is "++(show parentNS_As)
    let (RD_A a) = head $ maybeListToList parentNS_As
    debugline $ "a parent NS A record is " ++(show a)
    res <- dnslookup a (fromString domain) NS
    let hereNS =
         case res of
          Just x -> x
          Nothing -> []
    debugline $ "Name servers for this domain, according to "++(show ns)++": "
    debugline $ show hereNS
    --  ask that resolver to find the NS records for our domain
    --    (which is the same query that we did before - this is where I want
    --      looping to happen eventually)
    --  and return that RRset
    return $ hereNS

  let allNS = hereNS : nsFromAllNS
  debugline $ "allNS = "++(show allNS)
  -- stringify and sort:
  let sortedAllNS = map sort (map (map show) allNS)
  debugline $ "sortedAllNS = "++(show sortedAllNS)
  -- now are they all the same?
  let compared = testAllEqual sortedAllNS

  debugline $ "All equal? " ++ (show compared)
  if compared then do
    reportSuccess "all NS RRsets match"
   else do
    reportWarning "NS RRsets are not all the same"


testAllEqual [] = True
testAllEqual [a] = True
testAllEqual (a:b:rest) = (a == b) && (testAllEqual (b:rest))

  -- get a parent zone server. assume that the local recursive resolver
  -- is going to give us truthful values for this - we assume there is no
  -- misconfiguration there. later on, an --abusive mode might :w

  -- MVP: get NS delegations from a single .com server
  --      using arbitrary IP address lookup (I should be more careful about this - there's a big product space) on the NS records, perform a non-recursive SOA lookup on each name server, and check that we get back an SOA.

  -- perform arbitrary DNS lookup to get a .COM nameserver
  -- pick an arbitrary nameserver from this list.
  -- query it non-RR for domain NS records.
  -- for each NS record:
  --   perform an arbitrary lookup on that NS hostname to give IP
  --   query that IP non-RR for 'domain' SOA

-- i wonder if there's a fairly generic model of how later results can
--    add possible answers to queries that were made ealier and so I need
--    to branch those again (because DNS queries i make later can return
--    cached information such that if I made those queries again, I'd end
--    up using that cached information, which adds another branch to the
--    tree)... this feels really mfixy to me?


-- this should check various things based around DNS.
-- most importantly, rather than checking that one path through the DNS
-- works (so not failing if there is one server timing out)
-- it should instead try all (or at least many of them) paths.

-- to begin with, do this in a list-like monad:

-- for example:
-- resolve NS from parent zone.

--parent = "com." -- that can be inferred by dropping one off target, usually?
                -- and can even happen inside getNSfromParent?
                -- maybe there's a notion of trusted/unmonitored zones, which
                --  we aren't trying to detect misconfigurations in (because
                --  that would result in a lot of tree walking)

--do
--  delegatedNS <- getNSfromParent target parent
  -- delegatedNS will be strings of NS names, optionally with glue.
  -- perhaps (String, Maybe [ARecord])
--  moreNS <- queryEachNsForMoreNS delegatedNS
  -- each of these queries may return glue (or the authoritative answer, even,
  -- if the NS is authoritative for that zone)
  -- TODO: but now we need to query those moreNs for NS records too
  --   there's something 'fix'-like here? becuase it should eventually
  --   stop growing (even if every IP address on the internet is used...)
  --   checkout recursive do notation here, which translates into mfix
  --   and use that to properly generate: allNS.
  --   maybe this is just simple recursion, not fixpoints? maybe they're the
  --   same thing?
--  let allNS = delegatedNS ++ moreNS

--   we now have some NS names.
--      they may have addresses as glue
--      or those NS names can be resolved directly from the nameservers of
--        their containing zone, which might include the name servers that
--        we're discovering now.
--  when we find a new name server, we can also query it for the name
--    servers that it thinks the zone has. This can be checked against the
--    delegation/glue name server records. It might also return new NS
--    records that aren't in glue. or it might not return all of the
--    nameservers that are in glue. both of these cases are probably a
--    misconfiguration and warning worthy.
--    we should then recurse (perhaps in mfix?) 
--  theres a kind of recursion here which is going to result in lots of
--    looping round. not sure the best way to express that? i need to trim
--    the search tree somehow - some kind of monad-like non-determinism but
--    with pruning.

-- so I want to do a kind of product space search: query every name server to
--  ask it for the IP address of every name server; and check that the
--  response coming back from all of them is the same.
--  some kind of notation for asserting that a particular query made against
--    all nameservers is equal? (multiple IP addresses for name servers
--    potentially so I'll be getting RRsets back, with RRset equivalence...)
--  and also compare against glue returned with each NS RR (giving a sort
--    of virtual other nameserver response set)
--  we need to check that all nameservers are returning the same NS RR set
--   and the same addresses off the end of those... (not just from the servers
--   that gave out the records...)

-- then, when I know all the name servers that I might get from everywhere,
--   I can check all of them for a "target" DNS query - eg an A record, an
--   MX record.
-- this will then generate a list of RRsets containing multiple records.
--   again, they might contain glue. so when recursing I need to keep track
--   of glue to see if its correct or not.
-- then against whatever the end results are, we want to run specific tests
--  (which might be a nagios-style plug in?) that might vary based on
--  protocol.
-- might there be a consistency check across plugins? perhaps: i can say "you nee to resturn same stdout" or something.
--  one such consistency check (but not for cpanel) is SOA equivalence.
--   (which needs to be modulo update time? or just accept that there will be
--   very occasional warnings for this? or say that it can only be
--   inconsistent for a certain time period?)
--  for initial use i will then want to probably do a specific Host: header
--   http get to check that that IP address is actually serving that
--   vhost.
--  for http I probably also want to assert that the serving IP addresses
--    are the same.
