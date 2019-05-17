#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-13.21 runghc --package tagsoup --package yaml -}

import qualified Data.Yaml as Yaml
import Data.Yaml (object, (.=))
import Data.String (fromString)
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup ((~==), (~/=))

main :: IO ()
main = Yaml.encodeFile "audio.yaml" =<< extractAudioMessages <$> readFile "messages.html"

data AudioMessage = AudioMessage
  { audioSender :: String, audioDate :: String, audioUrl :: String }

instance Yaml.ToJSON AudioMessage where
  toJSON (AudioMessage sender date url) = object
    [fromString "by" .= sender, fromString "date" .= date, fromString "url" .= url]

extractAudioMessages :: String -> [AudioMessage]
extractAudioMessages html =
  let htmlMessages = TS.partitions (~== "<div class=from>") $ TS.parseTags html
  in [m | Just m <- extractAudio <$> htmlMessages]

extractAudio :: [TS.Tag String] -> Maybe AudioMessage
extractAudio message =
  let (sender, header) = extractSender message
      (date, messageBody) = extractDate header
  in case skipUntilAttachmentUrl messageBody of
    TS.TagOpen "a" [_, ("href", url)] : TS.TagText "Audio Message" : _ ->
      Just $ AudioMessage { audioSender = sender, audioDate = date, audioUrl = url }
    _ ->
      Nothing
  where
    extractSender (TS.TagOpen "b" [] : TS.TagText sender : rest) = (sender, rest)
    extractSender (_ : rest) = extractSender rest
    extractDate (TS.TagText date@('2' : _) : rest) = (date, rest)
    extractDate (TS.TagText (' ' : date@('2' : _)) : rest) = (date, rest)
    extractDate (_ : rest) = extractDate rest
    extractDate [] = error ("Unable to extract date for message " <> show message)
    skipUntilAttachmentUrl = dropWhile (~/= "<a>") . dropWhile (~/= "<div class=attacment>") -- attacment is not a typo (not mine at least <.<)
