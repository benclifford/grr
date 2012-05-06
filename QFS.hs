-- (c) copyright 2012 cqx limited - distribution limited

module QFS where
  -- QFS = Query/Feedback/Storage - lack better name for now...

  -- I think I want to end up with a monad transformer where there
  -- is a read query operation where that read query must be of a type
  -- that has Eq and an operation to inject new results discovered
  -- later/earlier, and some way of launching a new actual concrete query,
  -- distinct from reading out the results of the query,
  -- and perhaps there is a higher level composite operation consisting of
  -- launching a query and reading its results.

  -- under this, perhaps i need a distinct structure (in some state monad?)
  -- that tracks queries, records results, and calls assinged callbacks.

  -- I think this can be built and tested separately, and the
  -- monad (transformer) can be put on top of that, perhaps?

  -- what environment does this QFS structure need to exist in? I think it
  -- necessarily needs to sit in a monad, so that it can make callbacks
  -- (does that need to be a monad?)

  -- do I need the idea of a "primary query" execution which will launch
  -- the query, which may happen both before and after "secondary results"
  -- have been received? In the DNS case, I can receive additional RR data
  -- before I've actually performed a query (and indeed, I might not even
  -- be able to perform that query with out the additional data)

  -- perhaps I need to provide an initial callback for "making the offical
  -- query", at construction time?

  -- | Describes the state of a QFS store at any particular instant in time.
  --   This is immutable. There will be operations which take a QFSState and
  --   yield a new QFSState; those operations will also have to invoke the
  --   callbacks as necessary, I think.

  data (Eq qType, Eq resType) => QFSState qType resType = QFSState [(qType, [resType])] deriving Show

  emptyQFS :: (Eq resType, Eq qType) => QFSState qType resType
  emptyQFS = QFSState []

  -- | Pushes a result for a particular query into the structure, whether
  --   that query has been "launched" or not (whatever that means).
  pushResult :: (Eq resType, Eq qType) => QFSState qType resType -> (qType, resType) -> QFSState qType resType
  pushResult (QFSState entries) (q,r) = let
    -- TODO: replace below with partition
    allExceptQ = filter (\(qe,_) -> qe /= q)  entries
    onlyQ = filter (\(qe,_) -> qe == q) entries
    -- onlyQ should have 0 or 1 entries. assert? or use code that merges all
    --   found results into one so we don't lose anything even in case of that
    --   incorrect structure.
    qWithNew = if onlyQ == [] then [(q, [r])]
                              else [(q, (snd $ head onlyQ) ++ [r] )]
   in QFSState (allExceptQ ++ qWithNew)


