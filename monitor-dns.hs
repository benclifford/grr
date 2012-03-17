-- (C)Copyright 2012 CQX Limited
-- Not licensed for distribution


-- ok forget the original plan for now.
-- what I want to do is query the 


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
