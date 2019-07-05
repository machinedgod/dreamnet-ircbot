{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
( isForDreamnetChan
, isHttpUrl
)
where


import Network.IRC.Client   (Event(..), Source(..))

import qualified Data.Text as T

--------------------------------------------------------------------------------

isForDreamnetChan ∷ Event T.Text → Bool
isForDreamnetChan e =
    case _source e of
        (Channel "#dreamnet" _) → True
        _ → False


isHttpUrl ∷ T.Text → Bool
isHttpUrl = (||) <$> T.isPrefixOf "http"
                  <*> T.isPrefixOf "https"

