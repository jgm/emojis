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
import Data.Char (chr, ord)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bifunctor (first)
import Trie as Trie

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
replaceEmojis getReplacement = fromCodePoints . go . toCodePoints
 where
   go [] = []
   go (c:cs) =
       case Trie.matchLongestPrefix emojiTrie (c:cs) of
         Just (aliases, numcps, _subtrie) ->
           let (consumed, remaining) = splitAt numcps (c:cs)
            in toCodePoints (getReplacement (fromCodePoints consumed) aliases)
                 <> go remaining
         Nothing -> -- go forward one UTF8 code point
           c : go cs

toCodePoints :: Text -> [Int]
toCodePoints = map ord . T.unpack

fromCodePoints :: [Int] -> Text
fromCodePoints = T.pack . map chr

emojiTrie :: Trie.Trie [Text]
emojiTrie =
  Trie.fromList . map (first toCodePoints) . M.toList $ emojiAliasMap

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
