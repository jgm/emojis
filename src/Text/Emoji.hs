{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Emoji
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Emoji symbol lookup from canonical name.
-}
module Text.Emoji ( emojis, getEmoji, getEmojiName ) where
import Prelude
import qualified Data.Map as M
import Data.Text (Text)
import Text.Emoji.TH (genEmojis)

emojiMap :: M.Map Text Text
emojiMap = M.fromList emojis

emojiNameMap :: M.Map Text Text
emojiNameMap = M.fromList [(s,name) | (name,s) <- emojis]

getEmoji :: Text -> Maybe Text
getEmoji name = M.lookup name emojiMap

getEmojiName :: Text -> Maybe Text
getEmojiName s = M.lookup s emojiNameMap

emojis :: [(Text, Text)]
emojis = $(genEmojis "emoji.json")


