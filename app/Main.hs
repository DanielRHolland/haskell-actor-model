{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List
import Erlangesque (Action, newContext, post, spawn)

data Message = Kill | StrMsg String | Relay Int Message

data DifferentMessageSet = KillDiff | IntMsg Int

term :: IO ()
term = putStrLn "Process Terminated!"

diffAction :: Action DifferentMessageSet
diffAction (!) rcv = go 0
  where
    go n =
      rcv >>= \case
        KillDiff -> term
        IntMsg x -> print (n+x) >> go (n+x)

myErlangStyleAction :: Action Message
myErlangStyleAction (!) rcv = go []
  where
    go l = do
      msg <- rcv
      case msg of
        Kill -> do term
        StrMsg "all" -> do
          putStr (intercalate ", " l)
          putStrLn ";"
          go []
        StrMsg s -> do
          putStrLn s
          go (s : l)

relayAction :: Action Message
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
  let (!) = post ctx
  spawn ctx myErlangStyleAction 0
  0 ! StrMsg "Hi"
  0 ! StrMsg "1 2 3"
  0 ! StrMsg "Bye"
  0 ! StrMsg "all"

  spawn ctx relayAction 1
  1 ! Relay 0 (StrMsg "Message to relay")

  spawn ctx relayAction 2
  2 ! Relay 1 (Relay 0 (StrMsg "Message to relay twice"))

  diffCtx <- newContext
  let (!%) = post diffCtx
  spawn diffCtx diffAction 0
  0 !% IntMsg 2
  0 !% IntMsg 1
  0 !% IntMsg 3

  

  putStrLn =<< getLine -- only here as a hack to force wait for Worker processes
