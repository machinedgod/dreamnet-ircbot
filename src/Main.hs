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


import YoutubeTitleFetch

--------------------------------------------------------------------------------

data AppConfig
    = AppConfig { googleApiKey ∷ String
                }


googleApiKeyOpt ∷ OptDescr (AppConfig → AppConfig)
googleApiKeyOpt = Option
    ['k']
    ["google-api-key"]
    (ReqArg (\v ac → ac { googleApiKey = v }) "GOOGLE_KEY")
    "Google's API key, obtained via project management page."


main ∷ IO ()
main = do
    args ← getOpt RequireOrder [ googleApiKeyOpt ] <$> getArgs
    case args of
        ([ofun], _, _) → runWithApiKey (ApiKey (googleApiKey $ ofun $ AppConfig ""))
        _              → putStrLn "Missing required parameter: --google-api-key"
        

runWithApiKey ∷ ApiKey → IO ()
runWithApiKey key = do
    s ← newIRCState (plainConnection "chat.freenode.net" 6667) cfg key
    void $ installHandler sigTERM (Catch (handleShutdown s)) Nothing
    putStrLn "Running client..."
    runClientWith s
    putStrLn "Disconnected, bye."
    where
        cfg  = defaultInstanceConfig "dreamnet-ircbot"
                & handlers <>~ [ youtubeTitleFetch ]
                & channels .~ [ "#dreamnet" ]



handleShutdown ∷ IRCState s → IO ()
handleShutdown = runIRCAction disconnect


