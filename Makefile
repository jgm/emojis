all: src/Text/emojis.inc
	stack test

src/Text/emojis.inc: datafiles
	./update.hs > $@

# Unicode 14.0 is not yet the latest
unicodeVersion = 14.0
#unicodeVersion = latest

datafiles: emoji.json emoji-sequences.txt emoji-zwj-sequences.txt

emoji.json: .FORCE
	curl https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json | jq '[.[] | {emoji: .emoji, aliases: .aliases}]' > $@

emoji-sequences.txt emoji-zwj-sequences.txt: .FORCE
	curl https://unicode.org/Public/emoji/$(unicodeVersion)/$@ > $@

.PHONY: all datafiles .FORCE
