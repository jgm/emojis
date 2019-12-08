{-# LANGUAGE OverloadedStrings #-}

import Text.Emoji
import Test.HUnit

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList $ map TestCase
  [ assertEqual "emoji from '100'" (Just "💯") (emojiFromAlias "100")
  , assertEqual "aliases from 'rage'" (Just "😡") (emojiFromAlias "rage")
  , assertEqual "aliases from 'pout" (Just "😡") (emojiFromAlias "pout")
  , assertEqual "emoji name from nonexistent" Nothing (emojiFromAlias "saotehusatnoeus")
  , assertEqual "aliases from nonexistent emoji" Nothing (aliasesFromEmoji "oo")
  , assertEqual "aliases from emoji" (Just ["rage", "pout"]) (aliasesFromEmoji "😡")
  ]
