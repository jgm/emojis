# emojis

# 0.1.5

  * Update emoji data to Emoji 17.0. Note: recent emojis have no
    aliases, because we still derive aliases from gemoji's emoji.json,
    which has not been updated since Emoji 15.0.
  * Fix order of aliases (regression in 0.1.4.1, which reversed them).

# 0.1.4.1

  * Use foldl' instead of foldr to avoid stack overflows.

# 0.1.4

  * Update emoji data.
  * Export new function `replaceEmojis`, allowing replacement of
    emojis in a Text with alternate text.
  * Drop support for ghc < 8.8.

# 0.1.3

  * Update emoji data to Unicode Emoji 15.0 and aliases from gemoji.

# 0.1.2

  * Update emoji data (Unicode 14.0) and aliases from gemoji.
  * Export `baseEmojis` and `zwjEmojis`, generated directly from
    Unicode emoji data files, so that the library contains a
    complete list of emojis, including emojis for which gemoji
    provides no alias (Stephen Morgan).

# 0.1.1

  * Update emoji data.

## 0.1

  * Initial release
