all: src/Text/emojis.inc
	cabal test

src/Text/emojis.inc: datafiles
	./update.hs > $@

# 'latest' is 15.0 as of 2023-07-22.
unicodeVersion = latest

datafiles: emoji.json emoji-sequences.txt emoji-zwj-sequences.txt

emoji.json: .FORCE
	curl https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json | jq '[.[] | {emoji: .emoji, aliases: .aliases}]' > $@

emoji-sequences.txt emoji-zwj-sequences.txt: .FORCE
	curl https://unicode.org/Public/emoji/$(unicodeVersion)/$@ > $@

.PHONY: all datafiles .FORCE
