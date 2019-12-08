{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Emoji
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Emoji symbol lookup from alias, and vice versa.
-}
module Text.Emoji ( emojis, emojiFromAlias, aliasesFromEmoji ) where
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
#include "emojis.inc"
