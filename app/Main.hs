{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import hiding (argument)
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
        ModeServer
        (ServerOptions
         <$> switch
         (long "run-migration" <> short 'm' <> help "Run migration?")
         <*> option auto
         (long "postgresql" <> showDefault
          <> value "user=middleman password=middleman port=5432 connect_timeout=10"
          <> help "Postgresql connection string")
         <*> option str
         (long "gc-root"
          <> help "The directory to place the gc-roots")
        )
      addCommand "work"
        "Try to get work on the server"
        ModeWorker
        ( WorkerOptions
          <$> option str
            (long "server" <> value "localhost" <> showDefault
             <> help "The url of the server"
            )
          <*> option str
            (long "store" <> value "file:///nix/store" <> showDefault
             <> help "The connection string of the store"
            )
          <*> option auto
            (short 'j' <> long "job" <> value 1 <> showDefault
             <> help "The max number of job to use in parallel"
            )
          <*> option (Just <$> auto)
            (long "free" <> value Nothing
             <> help "Minimum required free memory"
            )
          <*> ( toRational <$> option (auto)
                ( short 'r' <> long "regulate-time"
                  <> value (5.0 :: Double) <> showDefault
                  <> help "Regulate the memory every time."
                )
              )
        )

      addCommand "push"
        "Try to push a derivation onto the server"
        ModeClient
        ( ClientOptions
          <$> option str
            (long "server" <> value "localhost" <> showDefault
             <> help "The url of the server"
            )
          <*> option str
            (long "store" <> value "file:///nix/store" <> showDefault
             <> help "The connection string of the store"
            )
          <*> option str
            (long "group" <> value "base" <> showDefault
             <> help "The connection string of the store"
            )
          <*> some ( argument str ( metavar "drv" <> help "The derivation to upload to the server")
            )
        )

  lo <- logOptionsHandle stderr (view optionsVerbose options_)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options_
          }
     in runRIO app $ run mode
