{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module LinkStore
( linkStore 
)
where


import Network.IRC.Client hiding (get)

import qualified Data.Text as T

import Common

--------------------------------------------------------------------------------

linkStore ∷ EventHandler a
linkStore = EventHandler (matchWhen isForDreamnetChan) sourceF


sourceF ∷ Source T.Text → Message T.Text → IRC a ()
sourceF (Channel "#dreamnet" _) (Privmsg "#dreamnet" (Right _)) = pure ()
sourceF _ _ = pure ()

