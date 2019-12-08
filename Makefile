all:
	stack build

.PHONY: all

emoji.json:
	curl https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json | jq '[.[] | {emoji: .emoji, aliases: .aliases}]' > $@
