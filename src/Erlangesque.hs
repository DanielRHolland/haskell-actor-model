{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | A basic implementation of the Actor Model in Haskell
--  Not a very idiomatic way to programme concurrently in Haskell.
--
--  Hopefully of interest as a comparision with Erlang.
module Erlangesque where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import qualified Data.Map as Map

type Receive a = IO a

type Id = Int

type Name = String

type Send a = Id -> a -> IO ()

type NodeId = Id

data CompositeAddress = CompositeAddress Id NodeId
(@@) :: Id -> NodeId -> CompositeAddress
(@@) = CompositeAddress


instance Address CompositeAddress where
  getId (CompositeAddress id _) = id
  getNodeId (CompositeAddress _ nodeId) = Just nodeId 

class Address a where -- May be Id, or may be complex remote or local
  getId :: a -> Id
  getNodeId :: a -> Maybe NodeId
  getNodeId _ = Nothing

instance Address Int where
  getId = id

instance Address String where
  getId = read

type Action a = Send a -> Receive a -> IO ()

type Inbox a = TQueue a

data Worker a = Worker (Action a) (Inbox a)

newtype Context a = Context (TVar (Map.Map Id (Worker a)))

getInbox :: Worker a -> Inbox a
getInbox (Worker _ q) = q

newContext :: IO (Context a)
newContext = Context <$> newTVarIO Map.empty

getWorker :: Context a -> Id -> IO (Maybe (Worker a))
getWorker (Context idMap) id =
  Map.lookup id <$> readTVarIO idMap

pushMessageToWorkerById :: Context a -> Id -> a -> IO ()
pushMessageToWorkerById ctx id msg =
  getWorker ctx id >>= \case
    Just w -> pushMessageToWorker w msg
    Nothing -> putStrLn "No such worker exists"

post :: Address b => Context a -> b -> a -> IO ()
post ctx addr = pushMessageToWorkerById ctx (getId addr)

spawn :: Context a -> Action a -> Id -> IO ()
spawn ctx a id =
  let outbox = pushMessageToWorkerById ctx
   in do
        w <- newWorker a
        threadId <- forkIO $ runWorker w outbox >> cleanUpWorker ctx id
        addWorkerToContext ctx w id

addWorkerToContext :: Context a -> Worker a -> Int -> IO ()
addWorkerToContext (Context idMap) w id =
  atomically $ modifyTVar idMap (Map.insert id w)

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

-------------------
-- Example Usage --
-------------------

-- | 1. Create/Choose a type to use for messages
data ExampleMessage = Kill | StrMsg String | Relay Id ExampleMessage
  deriving (Show)

-- | 2. Implement an Action function for the message type.
--      The state of the process should be held in the parameter
--      of the inner function, which is passed back into itself,
--      with any changes.
--      (!) allows for Erlang-Style message sending
--      rcv provides access to the messages 'Inbox' of this process
exampleAction :: Action ExampleMessage
exampleAction (!) rcv = go 0
  where
    go n =
      rcv >>= \case
        Kill ->
          putStrLn "Process Terminated"
        Relay target msg -> do
          putStrLn ("Forwarding message to " ++ show target)
          target ! msg
          go (n + 1)
        StrMsg s -> do
          putStr s
          putStrLn ("\t#" ++ show n)
          go (n + 1)

-- | An example of usage
example :: IO ()
example = do
  -- Create a new context
  ctx <- newContext
  -- Allow usage of infix !, Erlang Style
  let (!) = post ctx :: Int -> ExampleMessage -> IO ()
  -- Spawn a new process with id 0
  spawn ctx exampleAction 0
  -- Send some messages to it
  0 ! StrMsg "Hi"
  0 ! StrMsg "1 2 3"
  0 ! StrMsg "Bye"

  -- Spawn another process
  spawn ctx exampleAction 1

  -- Send a message to process 0, via process 1
  1 ! Relay 0 (StrMsg "Message to relay")

  -- Spawn another
  spawn ctx exampleAction 2
  -- Can nest relays
  2 ! Relay 1 (Relay 0 (StrMsg "Message to relay twice"))

  --Need to wait for Workers to finish. The following (kinda) works as a hack:
  print =<< getLine
