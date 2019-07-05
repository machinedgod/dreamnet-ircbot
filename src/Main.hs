{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where


import Control.Lens
import Control.Monad (void)
import System.Posix.Signals
import System.Environment
import System.Console.GetOpt
import Network.IRC.Client hiding (get)

import IrcState
import YoutubeTitleFetch
--import LinkStore
import ThatsWhatSheSaid

--------------------------------------------------------------------------------

newtype AppConfig
    = AppConfig { googleApiKey ∷ ApiKey
                }

--------------------------------------------------------------------------------

googleApiKeyOpt ∷ OptDescr (AppConfig → AppConfig)
googleApiKeyOpt = Option
    ['k']
    ["google-api-key"]
    (ReqArg (\v ac → ac { googleApiKey = ApiKey v }) "GOOGLE_KEY")
    "Google's API key, obtained via project management page."


main ∷ IO ()
main = do
    args ← getOpt RequireOrder [ googleApiKeyOpt ] <$> getArgs
    case args of
        ([ofun], _, _) → runWithApiKey (googleApiKey $ ofun $ AppConfig "")
        _              → putStrLn "Missing required parameter: --google-api-key"
        

runWithApiKey ∷ ApiKey → IO ()
runWithApiKey key = do
    s ← newIRCState (plainConnection "chat.freenode.net" 6667) cfg (IrcState key (Counter 5 0))
    void $ installHandler sigINT  (Catch (handleShutdown s)) Nothing
    void $ installHandler sigTERM (Catch (handleShutdown s)) Nothing
    putStrLn "Running client..."
    runClientWith s
    putStrLn "Disconnected, bye."
    where
        cfg  = defaultInstanceConfig "dreamnet-ircbot"
                & handlers <>~ [ youtubeTitleFetch
                               --, linkStore
                               , thatsWhatSheSaid
                               ]
                & channels .~ [ "#dreamnet" ]



handleShutdown ∷ IRCState s → IO ()
handleShutdown s = do
    putStrLn "SIGINT or SIGTERM caught, gracefully shutting down..."
    runIRCAction disconnect s

