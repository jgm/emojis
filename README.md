# emojis

[![CI
tests](https://github.com/jgm/emojis/workflows/CI%20tests/badge.svg)](https://github.com/jgm/emojis/actions)

This package provides functions for looking up an emoji by its name
(alias), and for returning the alias of an emoji.

A full list of (alias, emoji) pairs is also exported, as is a
Template Haskell splice for splicing in such a list generated
from a JSON data file.

Emoji data (`emoji.json`) is taken from the
[`gemoji`](https://github.com/github/gemoji) gem, used by GitHub.  To
regenerate the `emoji.json` datafile from the latest gemoji source, do `make
emoji.json`, then recompile this project.

This package differs in three main ways from the `emoji` package on Hackage:

- It uses Text rather than String
- A fuller list of emojis is used, including all those supported by GitHub
- Bidirectional lookup is supported (emoji from alias or alias from emoji)

