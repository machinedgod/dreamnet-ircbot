{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module IrcState
( ApiKey(..)
, Counter(..), threshold, count
, IrcState(..), apiKey, counter
)
where

import Control.Lens
import Data.String

--------------------------------------------------------------------------------

newtype ApiKey
    = ApiKey String
    deriving (IsString)

--------------------------------------------------------------------------------

data Counter
    = Counter {
      _threshold ∷ Int
    , _count     ∷ Int
    }
makeLenses ''Counter

--------------------------------------------------------------------------------

data IrcState
    = IrcState {
      _apiKey ∷ ApiKey
    , _counter ∷ Counter
    }
makeLenses ''IrcState
