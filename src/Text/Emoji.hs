{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Emoji
( emojis
, emojiFromAlias
, aliasesFromEmoji
, baseEmojis
, zwjEmojis
) where

import Prelude
import qualified Data.Map as M
import Data.Text (Text)

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
