include epkg.mk
epkg.mk:
	emacs --batch -l package -f package-initialize -l epkg -f epkg-copy-mk

SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v pipx 2>/dev/null),)
$(error pipx not found)
endif
BIN := venvs/subreddit/bin
PYTHON := $(BIN)/python
GIT_DIR ?= .

ELSRC := $(shell git ls-files lisp/*.el)
TESTSRC := $(shell git ls-files test/*.el)

EPKG_FILES := $(ELSRC) Makefile pyproject.toml src/subreddit/VERSION src/subreddit/*.py src/subreddit/templates/{mailcap,index.html,rtv.cfg} src/subreddit/jsonrpyc/*.py src/subreddit/rtv/*.py src/subreddit/rtv/packages/*.py src/subreddit/rtv/packages/praw/*{.ini,.py}
EPKG_MAIN := lisp/emacs-subreddit.el
EPKG_TEST_EL := $(TESTSRC)

.DEFAULT_GOAL := all

.PHONY: all
all: compile bin/app

bin/app: $(wildcard src/subreddit/*.py src/subreddit/jsonrpyc/*.py src/subreddit/rtv/*.py)
	PIPX_HOME=. PIPX_BIN_DIR=./bin PIPX_MAN_DIR=./man pipx install . --editable --quiet --force

$(BIN)/pytest: $(PYTHON)
	$(PYTHON) -m pip install --use-deprecated=legacy-resolver .[test]

$(BIN)/pylint: $(BIN)/pytest

.PHONY: pylint
pylint: $(BIN)/pylint
	$(PYTHON) -m pylint src/subreddit --rcfile=pylintrc

.PHONY: pytest
pytest: $(BIN)/pytest
	$(PYTHON) -m pytest tests

.PHONY: compile
compile: epkg-compile

.PHONY: test
test: bin/app compile epkg-test

README.rst: README.in.rst lisp/emacs-subreddit.el
	grep ';;' lisp/emacs-subreddit.el \
	  | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	  | sed -e 's/^\s*;;\s\?/   /g' \
	  | bash readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: install
install: epkg-install
	( \
	PKG_DIR=`$(EMACS) -batch -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'emacs-subreddit package-alist))))"`; \
	GIT_DIR=`git rev-parse --show-toplevel`/.git $(MAKE) -C $${PKG_DIR} bin/app; \
	)
