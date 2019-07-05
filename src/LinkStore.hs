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
linkStore = EventHandler matcher sourceF
    where
        matcher ∷ Event T.Text → Maybe (Message T.Text)
        matcher = matchWhen dreamnetMsgIsYoutube
    

sourceF ∷ Source T.Text → Message T.Text → IRC a ()
sourceF (Channel "#dreamnet" u) (Privmsg "#dreamnet" (Right x)) = pure ()
sourceF _ _ = pure ()

