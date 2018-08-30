{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_middleman

main :: IO ()
main = do
  (options_, mode) <- simpleOptions
    $(simpleVersion Paths_middleman.version)
    "middleman"
    "A command line tool for running nix scripts in a distributed way."
    (Options
       <$> switch
       ( long "verbose"
         <> short 'v'
         <> help "Verbose output?"
       )
       <*> option auto
       ( long "port"
         <> short 'p'
         <> showDefault
         <> help "Port of the server"
         <> value 3000
       )
    ) $ do
      addCommand "start"
        "Start the server"
        (const ModeServer)
        (pure ())
      addCommand "work"
        "Try to get work on the server"
        (const $ ModeWorker (WorkerOptions "http://localhost"))
        (pure ())

  lo <- logOptionsHandle stderr (view optionsVerbose options_)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options_
          , appMode = mode
          }
     in runRIO app run
