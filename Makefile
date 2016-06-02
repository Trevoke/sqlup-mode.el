EMACS ?= emacs
CASK ?= cask

test:
	${CASK} exec ert-runner

e2e:
	${CASK} exec ecukes -r magnars
