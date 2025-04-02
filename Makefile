.PHONY: format

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
js-sources := $(shell fd --no-ignore-parent -ejs -ecjs)

format:
	@purs-tidy format-in-place ${ps-sources}
	nixpkgs-fmt ${nix-sources}
	prettier -w ${js-sources}
