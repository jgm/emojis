# emojis

[![CI
tests](https://github.com/jgm/emojis/workflows/CI%20tests/badge.svg)](https://github.com/jgm/emojis/actions)

This package provides functions for looking up an emoji by its name
(alias), and for returning the aliases of an emoji.

A full list of (alias, emoji) pairs is also exported. In addition, a list of
all emoji (including those without aliases) is exported, along with a list of
all emoji formed using zero-width joiners.

Emoji aliases (`emoji.json`) are taken from the
[`gemoji`](https://github.com/github/gemoji) gem, used by GitHub. The full list
of emoji are taken from the Unicode emoji specification files,
(`emoji-sequences.txt`, `emoji-zwj-sequences.txt`). To regenerate these
datafiles from the latest source, do `make datafiles`.  If they have changed,
do `make` to regenerate the `emojis.inc` file and rebuild the project.

This package has the following advantages over the `emoji` package on Hackage:

- It supports a fuller range of emojis, including all those supported by GitHub
- It supports lookup of emoji aliases from emoji
- It uses Text rather than String
- It has a lighter dependency footprint: in particular, it does not require aeson
- It does not require TemplateHaskell

