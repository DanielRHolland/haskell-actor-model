{-# LANGUAGE LambdaCase #-}

-------------------
-- Example Usage --
-------------------

module Main where

import Erlangesque (initCtx, Action, Id)

-- | 1. Create/Choose a type to use for messages
data ExampleMessage = Kill | StrMsg String | Relay Id ExampleMessage

-- | 2. Implement an Action function for the message type.
--      The state of the process should be held in the parameter
--      of the inner function, which is passed back into itself,
--      with any changes.
--      (!) allows for Erlang-Style message sending
--      rcv provides the next message in the inbox
simpleAction :: Action ExampleMessage
simpleAction (!) rcv = go
  where
    go =
      rcv >>= \case
        StrMsg s -> do
          putStrLn s                            -- Do something
          go                                    -- Tail-call `go` to keep process alive 
        _ -> putStrLn "Process Terminated"      -- To kill it, simply don't call go again


-- | 2a A slightly more sophisticated action, with a counter to
--      demonstrate how to maintain a state within a process,
--      and an implementation of the `Relay` option, showing
--      how to message other processes.
relayingAction :: Action ExampleMessage
relayingAction (!) rcv = go 0
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
main :: IO ()
main = do
  -- Initialise a new context
  -- Allow usage of infix !, Erlang Style
  (spawn, (!)) <- initCtx  

  -- Let's create our first process
  spawn simpleAction 0
  -- Send a message to it
  0 ! StrMsg "Hello World!"
  -- Dispose of it
  0 ! Kill

  -- Spawn a new process with id 1
  spawn relayingAction 1
  -- Send some messages to it
  1 ! StrMsg "Hi"
  1 ! StrMsg "1 2 3"
  1 ! StrMsg "Bye"

  -- Spawn another process
  spawn relayingAction 2

  -- Send a message to process 1, via process 2
  2 ! Relay 1 (StrMsg "Message to relay")

  -- Spawn another
  spawn relayingAction 3
  -- Can nest relays
  3 ! Relay 2 (Relay 1 (StrMsg "Message to relay twice"))

  --Need to wait for Workers to finish. The following works as a hack:
  print =<< getLine

