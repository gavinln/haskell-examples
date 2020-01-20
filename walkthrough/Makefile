# Using Makefiles in Python
# https://www.client9.com/self-documenting-makefiles/

ifeq ($(OS),Windows_NT)
    SHELL='c:/Program Files/Git/usr/bin/sh.exe'
endif

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.DEFAULT_GOAL=help
.PHONY: help
help:  ## help for this Makefile
	@grep -E '^[a-zA-Z0-9_\-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

tmux:  ## start tmux
	tmuxp load tmux.yaml

ghcid:   ## check types
	ghcid --command="ghci -package safe align-equals.hs"

ghci:  ## interactive compiler
	ghci -package safe align-equals.hs

