EMACS ?= emacs

# A space-separated list of required package names
DEPS = cl-lib magit dash s

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${DEPS})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
	(package-refresh-contents)) \
      (package-install pkg))) \
  (unless package-archive-contents (package-refresh-contents)) \
  )"

EMACS_BATCH=${EMACS} -Q -batch --eval ${INIT_PACKAGES}

all: clean compile test

clean:
	rm -f *.elc

compile: clean
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch -f batch-byte-compile *.el

TEST_SELECTOR ?= t
test-unit:
	@echo "---- Run unit tests"
	@${EMACS_BATCH} \
		 -l gerrit-rest.el \
		 -l gerrit.el \
		 -l test/gerrit.el-test.el \
		 --eval "(ert-run-tests-batch-and-exit '${TEST_SELECTOR})" \
		 && echo "OK"

test: test-unit

.PHONY:	all compile clean package-lint test test-unit
