#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-13.21 runghc --package tagsoup -}

import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup ((~==))
import Data.List (isPrefixOf)

data AudioMessage = AudioMessage
  { audioSender :: String, audioDate :: String, audioUrl :: String }
  deriving (Show)

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
    skipUntilAttachmentUrl = dropWhile (TS.~/= "<a>") . dropWhile (TS.~/= "<div class=attacment>") -- attacment is not a typo (not mine at least <.<)

main :: IO ()
main = do
  tags <- TS.parseTags <$> readFile "messages.html"
  let messages = TS.partitions (~== "<div class=from>") tags
  let audioMessages = [m | Just m <- extractAudio <$> messages]

  putStrLn $ show $ audioMessages
