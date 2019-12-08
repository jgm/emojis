{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{- |
   Module      : Text.Emoji.TH
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Code generation for emoji list in Text.Emoji.
-}
module Text.Emoji.TH ( genEmojis ) where
import Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)

genEmojis :: FilePath -> Q Exp
genEmojis fp = do
  addDependentFile fp
  bs <- runIO $ B.readFile fp
  case eitherDecode bs of
    Left e -> error e
    Right (emoji :: [Emoji]) -> [| emojis |]
      where emojis = [ (alias, txt)
                     | Emoji txt aliases <- emoji
                     , alias <- aliases
                     ]

data Emoji = Emoji String [String]
  deriving Show

instance FromJSON Emoji where
    parseJSON = withObject "Emoji" $ \v -> Emoji
        <$> v .: "emoji"
        <*> v .: "aliases"
