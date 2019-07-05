{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module ThatsWhatSheSaid
( thatsWhatSheSaid
)
where


import Control.Lens
import Control.Monad.IO.Class    (liftIO)
import System.Random             (randomRIO)
import Network.IRC.Client hiding (get)

import qualified Data.Text as T

import Common
import IrcState

--------------------------------------------------------------------------------

range ∷ (Int, Int)
range = (10, 50)

--------------------------------------------------------------------------------

thatsWhatSheSaid ∷ EventHandler IrcState
thatsWhatSheSaid = EventHandler (matchWhen isForDreamnetChan) sourceF


sourceF ∷ Source T.Text → Message T.Text → IRC IrcState ()
sourceF (Channel "#dreamnet" _) (Privmsg "#dreamnet" (Right _)) = do
    increaseCounter
    maybeSayIt
sourceF _ _ = pure ()


increaseCounter ∷ IRC IrcState ()
increaseCounter = counter.count += 1


resetCounter ∷ IRC IrcState ()
resetCounter = counter.count .= 0


setNewLimit ∷ IRC IrcState ()
setNewLimit = do
    newLimit ← liftIO $ randomRIO range
    counter.threshold .= newLimit


maybeSayIt ∷ IRC IrcState ()
maybeSayIt = use counter >>= \(Counter l x) →
    if x >= l
        then do sayIt
                resetCounter
                setNewLimit
        else pure ()


sayIt ∷ IRC s ()
sayIt = send (Privmsg "#dreamnet" (Right "That's what she said!"))
