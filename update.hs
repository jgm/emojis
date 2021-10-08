#!/usr/bin/env stack
-- stack --resolver lts-18.10 script --package aeson --package megaparsec --package bytestring --package pretty-show --package pretty
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (($+$), hang, render, text)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (hexadecimal)


data Emoji = Emoji String [String]
  deriving Show

instance FromJSON Emoji where
    parseJSON = withObject "Emoji" $ \v -> Emoji
        <$> v .: "emoji"
        <*> v .: "aliases"

type Parser = Parsec () TL.Text

-- | Parse a nonempty list of hexadecimal code points
emojiP :: Parser Text
emojiP = T.pack . map chr <$> sepEndBy1 hexadecimal (char ' ')

-- | Parse a range of hexadecimal code points, representing many single-code-point emoji
emojiRangeP :: Parser [Text]
emojiRangeP = do
    start <- chr <$> hexadecimal
    _ <- string ".."
    end <- chr <$> hexadecimal
    return . map T.singleton $ enumFromTo start end

-- | Parse anything until the end of the line
skipTillEol :: Parser ()
skipTillEol = () <$ skipManyTill anySingle eol

-- | Parse a Unicode spec file containing lists of valid emoji
unicodeSpecP :: Parser [Text]
unicodeSpecP = fmap concat . many $ choice
    [ [] <$ char '#' <* skipTillEol
    , [] <$ eol
    , try emojiRangeP <* skipTillEol
    , pure <$> emojiP <* skipTillEol
    ]

-- | Parse a Unicode spec file and error on failure
parseEmojiFile :: FilePath -> IO [Text]
parseEmojiFile fp = either (error . show) return . runParser unicodeSpecP fp . decodeUtf8 =<< B.readFile fp


main :: IO ()
main = do
  emojiAliases <- either error return . eitherDecode =<< B.readFile "emoji.json"
  baseEmojis   <- parseEmojiFile "emoji-sequences.txt"
  zwjEmojis    <- parseEmojiFile  "emoji-zwj-sequences.txt"

  let pairs = [ (alias, txt)
              | Emoji txt aliases <- emojiAliases
              , alias <- aliases
              ]

  putStrLn $ render $
      hang (text "emojis =") 2 (ppDoc pairs)
      $+$
      hang (text "baseEmojis =") 2 (ppDoc baseEmojis)
      $+$
      hang (text "zwjEmojis =") 2 (ppDoc zwjEmojis)
