{-# LANGUAGE GADTs, ScopedTypeVariables #-}

import System.IO.Unsafe -- repalce with statet
import Control.Monad.Cont
import Data.IORef -- replace with statet

-- this should become a state monad transformer layer between
-- ContT and IO, I think:

type Callback = Integer -> ContT [Integer] IO [Integer]

callback = unsafePerformIO $ newIORef ([] :: [Callback])
alreadyPushed = unsafePerformIO $ newIORef ([] :: [Integer])

{-
maybe implement using fixed Query Response types to reduce quantification
confusion in my mind


    commands:
      push q r     -- pushes a value in for a query
      pull q       -- pulls a value for a query

   push q r :: q -> r -> m ()
   pull q :: q -> m r
   io :: IO a -> m a    -- substitute for lift.

what are the meanings of push,pull,io, in the sense of >>= ?

so I need an 'interpret' function which interprets programs in this language
in the IO monad?

the way i've done `bind` in my other playing on this project seems ugly.

Any computation needs to have a 'final return type' because that's what
is finally returned (or at least a list of those is finally returned) that
is distinct from the return type of this particular 'intermediate computation'.

Thats like Cont r a, which has intermediate return value of a, and final
return value of r.

so what about if i implement push and pull in ContT IO? I still need to
thread state around, which might be possibel with a transformer, or might

-}

-- ContT r IO a
-- r is the final result, a is the intermediate value.

-- I'll use strings as cached values and integers as the final return value

main = do
  x <- runContT (ointerpret prog) return
  print x
  s <- readIORef callback
  t <- readIORef alreadyPushed
  putStrLn $ "callback state has " ++ (show $ length s) ++ " entries."
  putStrLn $ "alreadyPushed state has " ++ (show $ length t) ++ " entries."

data DM a where
   Push :: Integer -> DM ()
   Pull :: DM Integer
   Lift :: IO a -> DM a
   Return :: a -> DM a
   Bind :: DM a -> (a -> DM b) -> DM b

instance Monad DM where
  return a = Return a
  a >>= f = Bind a f


-- progrReturn is called 'abort' in some literature

ointerpret :: DM Integer -> ContT [Integer] IO [Integer]
ointerpret prog = do
  l <- callCC $ \progReturn -> do
    x <- interpret progReturn (prog :: DM Integer)
    return [x]
  return l

-- ^^^ progReturn needs to take a [Integer], but x<- is binding an Integer.

interpret :: ([Integer] -> ContT [Integer] IO [Integer]) -> DM a -> ContT [Integer] IO a

interpret progReturn Pull = callCC $ \k -> do
  lift $ putStrLn $ "pull"
  lift $ modifyIORef callback (\l -> l ++ [k])
  already <- lift $ readIORef alreadyPushed
  r <- lift $ mapM (\v -> runContT (k v) return) already
  progReturn $ join r
  return 5 -- for typing... we should never reach this.


interpret progReturn (Push v) = callCC $ \k -> do
  lift $ putStrLn $ "pushing " ++ (show v)
  already <- lift $ readIORef alreadyPushed
  if v `elem` already then do
    lift $ putStrLn "pruning because already seen"
    -- and now we'll flow off the end and carry on
   else do
-- TODO look if we've had this value before, and if so, don't cache/do the
-- callbacks - although if I want provenance later, I'll need to add a
-- provenance annotation here.
    (callbacks :: [Callback]) <- lift $ readIORef callback
    lift $ modifyIORef alreadyPushed (\l -> l ++ [v])
    (rl :: [[Integer]]) <- lift $ mapM (\(cb :: Callback) -> putStrLn "calling cb" >> runContT (cb v) return) (callbacks :: [Callback])
    (r :: [Integer]) <- lift $ runContT (k ()) id
    progReturn (r ++ join rl)
    return ()
  return () -- need this to makes type work... even though we should never hit this line


--interpret progReturn (Push v) = return ()


-- callCC $ \k -> do
  -- lift $ modifyIORef callback (\l -> l ++ [k])
--  return (5 :: Integer)

interpret progReturn (Return v) = return v

interpret progReturn (Lift action) = do
  v <- lift action -- double lift from the IO monad
  return v

-- in the absence of more specific action, use the continuation bind which
-- gives regular sequencing
interpret progReturn (Bind action rest :: DM b) = do
  v <- interpret progReturn action
  -- now v is a list, [a]
  -- and rest is function from a, not from [a]
  -- l <- mapM (\u -> interpret progReturn (rest u)) v -- type [[b]]
-- maybe this mapping should move into the push/pull code?
  u <- interpret progReturn (rest v)
  return u

prog :: DM Integer
prog = do
  Push 100
  x <- Pull
  Push 130
  return (x+1)


