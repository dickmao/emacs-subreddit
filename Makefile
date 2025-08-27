SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v pipx 2>/dev/null),)
$(error pipx not found)
endif
BIN := venvs/subreddit/bin
PYTHON := $(BIN)/python
GIT_DIR ?= .
SRC := $(shell git ls-files lisp/*.el)
TESTSRC := $(shell git ls-files test/*.el)

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

deps/archives/gnu/archive-contents:
	$(EMACS) -batch -l package --visit lisp/emacs-subreddit.el --eval "(let ((package-user-dir \"$(CURDIR)/deps\")) (package-refresh-contents) (package-install-from-buffer))"

.PHONY: compile
compile: deps/archives/gnu/archive-contents
	$(EMACS) -batch -l gnus -L lisp -L test \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(put 'gnus-select-method 'byte-obsolete-variable nil)" \
	  --eval "(setq package-user-dir \"$(CURDIR)/deps\")" \
	  -f package-initialize \
	  -f batch-byte-compile $(SRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(SRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

.PHONY: test
test: bin/app compile
	$(EMACS) --batch -L lisp -L test $(patsubst %.el,-l %,$(notdir $(TESTSRC))) -f ert-run-tests-batch

README.rst: README.in.rst lisp/emacs-subreddit.el
	grep ';;' lisp/emacs-subreddit.el \
	  | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	  | sed -e 's/^\s*;;\s\?/   /g' \
	  | bash readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: dist-clean
dist-clean:
	( \
	PKG_NAME=`$(EMACS) -batch -L ./lisp -l emacs-subreddit-package --eval "(princ (emacs-subreddit-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L ./lisp -l emacs-subreddit-package -f emacs-subreddit-package-inception
	( \
	PKG_NAME=`$(EMACS) -batch -L ./lisp -l emacs-subreddit-package --eval "(princ (emacs-subreddit-package-name))"`; \
	rsync -R Makefile pyproject.toml src/subreddit/VERSION src/subreddit/*.py src/subreddit/templates/{mailcap,index.html,rtv.cfg} src/subreddit/jsonrpyc/*.py src/subreddit/rtv/*.py src/subreddit/rtv/packages/*.py src/subreddit/rtv/packages/praw/*{.ini,.py} $${PKG_NAME}; \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	( \
	PKG_NAME=`$(EMACS) -batch -L ./lisp -l emacs-subreddit-package --eval "(princ (emacs-subreddit-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir $(1))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote emacs-subreddit) package-alist)))" \
	  -l gnus \
	  --eval "(put 'gnus-select-method 'byte-obsolete-variable nil)" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -l package --eval "(setq package-user-dir $(1))" -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'emacs-subreddit package-alist))))"`; \
	GIT_DIR=`git rev-parse --show-toplevel`/.git $(MAKE) -C $${PKG_DIR} bin/app; \
	)
	$(MAKE) dist-clean
endef

.PHONY: install-dev
install-dev: dist
	$(call install-recipe,\"$(CURDIR)/deps\")

.PHONY: install
install: dist
	$(call install-recipe,package-user-dir)
