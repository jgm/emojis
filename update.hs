#!/usr/bin/env stack
-- stack --resolver lts-18.10 script --package aeson --package bytestring --package pretty-show --package pretty
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (hang, render, text)

data Emoji = Emoji String [String]
  deriving Show

instance FromJSON Emoji where
    parseJSON = withObject "Emoji" $ \v -> Emoji
        <$> v .: "emoji"
        <*> v .: "aliases"

main :: IO ()
main = do
  bs <- B.readFile "emoji.json"
  case eitherDecode bs of
    Left e -> error e
    Right emojis -> putStrLn $ render $
                       hang (text "emojis = ") 2 (ppDoc pairs)
      where pairs = [ (alias, txt)
                    | Emoji txt aliases <- emojis
                    , alias <- aliases
                    ]

