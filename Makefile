EMACS ?= emacs
CASK ?= cask

test:
	${CASK} exec ert-runner
