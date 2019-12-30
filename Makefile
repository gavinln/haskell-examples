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

s0-helloworld:  ## step 0 - use step=show/run/help
	pipenv run python metaflow-tutorials/00-helloworld/helloworld.py $(step)

s1-playlist:  ## step 1 - use step=show/run/help
	pipenv run python metaflow-tutorials/01-playlist/playlist.py $(step)

s2-statistics:  ## step 2 - use step=show/run/help
	pipenv run python metaflow-tutorials/02-statistics/stats.py $(step)

s3-playlist-redux:  ## step 3 - use step=show/run/help
	pipenv run python metaflow-tutorials/03-playlist-redux/playlist.py $(step)

s4-playlist-plus:  ## step 4 - use step=show/run/help
	echo export CONDA_CHANNELS=conda-forge
	pipenv run python metaflow-tutorials/04-playlist-plus/playlist.py --environment=conda $(step)
