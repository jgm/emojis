{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Emoji
( emojis
, emojiFromAlias
, aliasesFromEmoji
, replaceEmojis
, baseEmojis
, zwjEmojis
) where

import Prelude
import qualified Data.Map as M
import Data.Text (Text)
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import Data.Trie as Trie
import Data.Text.Encoding as TE

emojiMap :: M.Map Text Text
emojiMap = M.fromList emojis

emojiAliasMap :: M.Map Text [Text]
emojiAliasMap =
  foldr (\(alias, s) m -> M.alter (go alias) s m) mempty emojis
    where
     go alias Nothing   = Just [alias]
     go alias (Just as) = Just (alias:as)

-- | Lookup an emoji given its alias.
emojiFromAlias :: Text -> Maybe Text
emojiFromAlias name = M.lookup name emojiMap

-- | Lookup the aliases of an emoji.
aliasesFromEmoji :: Text -> Maybe [Text]
aliasesFromEmoji s = M.lookup s emojiAliasMap

-- | Replace emojis in a text with alternative text.
-- The replacement function maps an emoji and a list of emoji aliases
-- to a replacement text.
replaceEmojis :: (Text -> [Text] -> Text) -> Text -> Text
replaceEmojis getReplacement = TE.decodeUtf8 . go . TE.encodeUtf8
 where
   go bs
     | B.null bs = mempty
     | otherwise =
       case Trie.match emojiTrie bs of
         Just (emojiBs, aliases, rest) ->
           TE.encodeUtf8 (getReplacement (TE.decodeUtf8 emojiBs) aliases)
             <> go rest
         Nothing -> -- go forward one UTF8 code point
           let b = B.head bs
               numbytes
                 | b >= 0xF0 = 4
                 | b >= 0xE0 = 3
                 | b >= 0xC0 = 2
                 | otherwise = 1
            in B.take numbytes bs <> go (B.drop numbytes bs)

emojiTrie :: Trie [Text]
emojiTrie =
  Trie.fromList . map (first TE.encodeUtf8) . M.toList $ emojiAliasMap

-- | Association list of (alias, emoji) pairs.  Note that the
-- same emoji may have multiple aliases.
emojis :: [(Text, Text)]

-- | A list of all valid emoji (not containing zero-width joiners), taken from
-- the Unicode Emoji Specification.
baseEmojis :: [Text]

-- | A list of all valid emoji built from zero-width joiners, taken from the
-- Unicode Emoji Specification.
zwjEmojis :: [Text]
#include "emojis.inc"
