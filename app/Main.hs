{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List
import Erlangesque (Action, Message (Kill, Relay, StrMsg), newContext, pushMessageToWorkerById, spawn)

myErlangStyleAction :: Action
myErlangStyleAction (!) rcv = go []
  where
    go l = do
      msg <- rcv
      case msg of
        Kill ->
          putStrLn "Process Terminated!"
        StrMsg "all" -> do
          putStr (intercalate ", " l)
          putStrLn ";"
          go []
        StrMsg s -> do
          putStrLn s
          go (s : l)

relayAction :: Action
relayAction (!) rcv = go
  where
    go =
      rcv >>= \case
        Kill ->
          putStrLn "Process Terminated!"
        Relay target msg -> do
          putStrLn ("Relaying message to " ++ show target)
          target ! msg
          go
        StrMsg s -> do
          putStrLn s
          go

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  ctx <- newContext
  let (!) = pushMessageToWorkerById ctx
  spawn ctx myErlangStyleAction 0
  0 ! StrMsg "Hi"
  0 ! StrMsg "1 2 3"
  0 ! StrMsg "Bye"
  0 ! StrMsg "all"

  spawn ctx relayAction 1
  1 ! Relay 0 (StrMsg "Message to relay")

  spawn ctx relayAction 2
  2 ! Relay 1 (Relay 0 (StrMsg "Message to relay twice"))

  putStrLn =<< readLn -- only here as a hack to force wait for Worker processes
