all: src/Text/emojis.inc
	stack test

src/Text/emojis.inc: emoji.json
	./update.hs > $@

.PHONY: all

emoji.json:
	curl https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json | jq '[.[] | {emoji: .emoji, aliases: .aliases}]' > $@
