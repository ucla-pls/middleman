module Run (run) where

import Import

import Worker
import Client
import Server

run :: RIO App ()
run = do
  mode <- asks appMode
  case mode of
    ModeServer sops ->
      server sops
    ModeWorker wops ->
      worker wops
    ModeClient cops ->
      client cops
