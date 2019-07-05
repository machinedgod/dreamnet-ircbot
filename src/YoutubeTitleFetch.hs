{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module YoutubeTitleFetch
( youtubeTitleFetch
)
where


import Prelude            hiding (head)
import Safe                      (headMay)
import Control.Lens              (view, use)
import Control.Monad             ((>=>), forM_)
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson                (decode')
import Data.Aeson.TH             (deriveJSON, defaultOptions)
import Network.IRC.Client hiding (get)
import Network.URL               
import Network.Wreq              (get, responseBody)

import qualified Data.Text as T

import Common
import IrcState

--------------------------------------------------------------------------------

newtype Title
    = Title { title ∷ T.Text }
    deriving(Show)
$(deriveJSON defaultOptions ''Title)
newtype Snippet
    = Snippet { snippet ∷ Title }
    deriving(Show)
$(deriveJSON defaultOptions ''Snippet)
newtype GooglesResponse
    = GooglesResponse { items ∷ [Snippet] }
    deriving(Show)
$(deriveJSON defaultOptions ''GooglesResponse)

--------------------------------------------------------------------------------

newtype Param
    = Param [(String, String)]
newtype Path
    = Path String

--------------------------------------------------------------------------------

youtubeTitleFetch ∷ EventHandler IrcState
youtubeTitleFetch = EventHandler (matchWhen isForDreamnetChan) sourceF


sourceF ∷ Source T.Text → Message T.Text → IRC IrcState () 
sourceF (Channel "#dreamnet" u) (Privmsg "#dreamnet" (Right x)) = do
    let ws = zip (isHttpUrl <$> T.words x) (T.words x)
    forM_ ws $ processLinks (fetchVideoTitle >=> sendMessage u)
sourceF _ _ = pure ()


processLinks ∷ (String → IRC s ()) → (Bool, T.Text) → IRC s ()
processLinks f (True, w) = maybe (pure ())
                              (whenYouTube f)
                              (importURL (T.unpack w))
processLinks _ _ = pure ()


isYouTubeHost ∷ Host → URL → Maybe (Either Param Path)
isYouTubeHost h u = go . T.pack . host $ h
    where
        go (T.isPrefixOf "www.youtube.com" → True) =
            Just (Left $ Param $ url_params u)
        go (T.isPrefixOf "youtu.be"        → True) =
            Just (Right $ Path $ url_path u)
        go _ = 
            Nothing


whenYouTube ∷ (String → IRC s ()) → URL → IRC s ()
whenYouTube f u =
    case url_type u of
        Absolute h → case isYouTubeHost h u of
            Just e → f $ either
                            extractVideoIdFromParam
                            extractVideoIdFromPath
                            e
            _ → pure ()
        _ → pure ()


extractVideoIdFromPath ∷ Path → String
extractVideoIdFromPath (Path p) = p


extractVideoIdFromParam ∷ Param → String
extractVideoIdFromParam (Param ps) = foldr ff "" ps
    where
        ff ("v", v) _ = v
        ff _        a = a

--https://www.googleapis.com/youtube/v3/videos
--    ?part=snippet
--    &id=JH_Ou17_zyU
--    &key=l0l I originally forgot to remove it from the comment, fuck me, right!?

fetchVideoTitle ∷ String → IRC IrcState T.Text
fetchVideoTitle videoId = do
    (ApiKey key) ← use apiKey
    let h  = Host (HTTP True) "www.googleapis.com" Nothing
        p  = "/youtube/v3/videos"
        ps = [ ("part", "snippet")
             , ("key", key)
             , ("id", videoId)
             ]
        googleRequest = URL (Absolute h) p ps
    res ← liftIO $ view responseBody <$> get (exportURL googleRequest)
    pure $ maybe
            "Failed to fetch or decode the title!"
            (title . snippet)
            (decode' res >>= headMay . items)
        

sendMessage ∷ T.Text → T.Text → IRC s ()
sendMessage u t = let content = Right $ u <> "'s video title: " <> t
                  in  send (Privmsg "#dreamnet" content) 
