{-# LANGUAGE LambdaCase #-}

-- | A basic implementation of the Actor Model in Haskell
--   Not a very idiomatic way to programme concurrently in Haskell.
--   Based on the Erlang-style of Actor Model
--
-- dan@danrh.co.uk wrote this, do what you want with it, but retain this notice.
-- Feel free to get in touch with improvements/suggestions/offersOfVastSumsOfMoney/lettersOfLoveAndHate
--
--
-- TODO:
-- - Make Id/Address a typeclass, so that string names can also be used as
--   identifiers.
-- - Make spawning a new process with the same id kill off the old one.
-- - Add ability to link Contexts
--      - Message nodes in other contexts
--      - Add remote contexts
-- - Consider how to make handle errors better
--      - Particularly when attempting to send a message to a non-existent node
--      - How should errors be handled?
--
module Erlangesque (initCtx, Action, Id) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import qualified Data.Map as Map

-- | Worker Identifier
type Id = Int

type Receive a = IO a

type Send a = Id -> a -> IO ()

-- | Worker action
type Action a = Send a -> Receive a -> IO ()

type Inbox a = TQueue a

data Worker a = Worker (Action a) (Inbox a)

newtype Context a = Context (TVar (Map.Map Id (Worker a)))

getInbox :: Worker a -> Inbox a
getInbox (Worker _ q) = q

newContext :: IO (Context a)
newContext = Context <$> newTVarIO Map.empty

-- | Initialise a fresh context, and return methods
--   for adding spawning new Actors and sending
--   messages in the context.
initCtx :: IO (Action a -> Id -> IO(), Id -> a -> IO())
initCtx = do
  ctx <- newContext
  return (spawn ctx, post ctx)

getWorker :: Context a -> Id -> IO (Maybe (Worker a))
getWorker (Context idMap) id =
  Map.lookup id <$> readTVarIO idMap

-- | Post a message to the worker in with the id in a
--   given context
post :: Context a -> Id -> a -> IO ()
post ctx id msg =
  getWorker ctx id >>= \case
    Just w -> pushMessageToWorker w msg
    Nothing -> putStrLn "No such worker exists"

-- | Create a new worker and add it to the context
spawn :: Context a -> Action a -> Id -> IO ()
spawn ctx a id =
  let outbox = post ctx
   in do
        w <- newWorker a
        threadId <- forkIO $ runWorker w outbox >> cleanUpWorker ctx id
        addWorkerToContext ctx w id

addWorkerToContext :: Context a -> Worker a -> Int -> IO ()
addWorkerToContext (Context idMap) w id =
  atomically $ modifyTVar idMap (Map.insert id w)

-- | Remove worker from context
cleanUpWorker :: Context a -> Id -> IO ()
cleanUpWorker (Context idMap) id =
  atomically $ modifyTVar idMap (Map.delete id)

newInbox :: STM (Inbox a)
newInbox = newTQueue

newWorker :: Action a -> IO (Worker a)
newWorker f = atomically $ Worker f <$> newInbox

runWorker :: Worker a -> Send a -> IO ()
runWorker (Worker action inbox) send = action send (popMessage inbox)

popMessage :: Inbox a -> IO a
popMessage = atomically . readTQueue

pushMessage :: Inbox a -> a -> IO ()
pushMessage inbox = atomically . writeTQueue inbox

pushMessageToWorker :: Worker a -> a -> IO ()
pushMessageToWorker = pushMessage . getInbox

