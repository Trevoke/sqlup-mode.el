EMACS ?= emacs
CASK ?= cask

unit:
	${CASK} exec ert-runner

e2e:
	${CASK} exec ecukes -r magnars
