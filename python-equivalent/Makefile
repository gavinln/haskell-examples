# Using Makefiles in Python
# https://krzysztofzuraw.com/blog/2016/makefiles-in-python-projects.html

# Listing targets in the Makefile
# http://stackoverflow.com/questions/4219255/how-do-you-get-the-list-of-targets-in-a-makefile

SCRIPT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.DEFAULT_GOAL=help
.PHONY: help
help:  ## help for this Makefile
	@grep -E '^[a-zA-Z0-9_\-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

run:
	@pipenv run python $(SCRIPT_DIR)/functor-applicative-monad.py

test:
	@pytest $(SCRIPT_DIR)/test*.py

mypy:
	@mypy --follow-imports=skip --ignore-missing-imports --check-untyped-defs $(SCRIPT_DIR)/functor-applicative-monad.py

flake8:
	@flake8 $(SCRIPT_DIR)

clean:
	# find files (works on Linux/Windows)
	@grep -rl --include='*.pyc' '' . | xargs rm -f
