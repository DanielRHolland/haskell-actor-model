{-# LANGUAGE LambdaCase #-}

module Erlangesque where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import qualified Data.Map as Map

data Message = Kill | StrMsg String | Relay Id Message
  deriving (Show)

type Receive = IO Message

type Id = Int

type Name = String

type Send = Id -> Message -> IO ()

type Action = Send -> Receive -> IO ()

type Inbox = TQueue Message

data Worker = Worker Action Inbox

newtype Context = Context (TVar (Map.Map Id Worker))

getInbox :: Worker -> Inbox
getInbox (Worker _ q) = q

newContext :: IO Context
newContext = Context <$> newTVarIO Map.empty

getWorker :: Context -> Id -> IO (Maybe Worker)
getWorker (Context idMap) id =
  Map.lookup id <$> readTVarIO idMap

pushMessageToWorkerById :: Context -> Id -> Message -> IO ()
pushMessageToWorkerById ctx id msg =
  getWorker ctx id >>= \case
    Just w -> pushMessageToWorker w msg
    Nothing -> putStrLn "No such worker exists"

spawn :: Context -> Action -> Id -> IO ()
spawn ctx a id =
  let outbox = pushMessageToWorkerById ctx
   in do
        w <- newWorker a
        threadId <- forkIO $ runWorker w outbox >> cleanUpWorker ctx id
        addWorkerToContext ctx w id

addWorkerToContext :: Context -> Worker -> Int -> IO ()
addWorkerToContext (Context idMap) w id =
  atomically $ modifyTVar idMap (Map.insert id w)

cleanUpWorker :: Context -> Id -> IO ()
cleanUpWorker (Context idMap) id =
  atomically $ modifyTVar idMap (Map.delete id)

newInbox :: STM Inbox
newInbox = newTQueue

newWorker :: Action -> IO Worker
newWorker f = atomically $ Worker f <$> newInbox

runWorker :: Worker -> Send -> IO ()
runWorker (Worker action inbox) send = action send (popMessage inbox)

popMessage :: Inbox -> IO Message
popMessage = atomically . readTQueue

pushMessage :: Inbox -> Message -> IO ()
pushMessage inbox = atomically . writeTQueue inbox

pushMessageToWorker :: Worker -> Message -> IO ()
pushMessageToWorker = pushMessage . getInbox

-- Example `Actions`

printAction :: Action
printAction (!) rcv = go
  where
    go =
      rcv >>= \case
        Kill ->
          putStrLn "Worker Killed"
        StrMsg "Forward" ->
          2 ! StrMsg "Forwarded" >> go
        StrMsg s ->
          putStrLn s >> go

incrAction :: Action
incrAction (!) rcv = go 0
  where
    go n =
      rcv >>= \case
        Kill -> putStrLn "Worker Killed"
        Relay target msg ->
          putStrLn ("Forwarding message to " ++ show target)
            >> target ! msg
            >> go n
        StrMsg "Count" ->
          putStrLn ("Count: " ++ show n)
            >> go n
        StrMsg "Incr" ->
          go (n + 1)
        StrMsg s ->
          putStrLn s
            >> go n

-- Not a very idiomatic way to programme concurrently in Haskell.
--
-- Hopefully of interest as a comparision with Erlang.
--
