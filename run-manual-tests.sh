#!/bin/sh -e
# For running the tests you need to have the following entries in your
# ~/.netrc
#
#machine review.opendev.org
#        login ...
#        password ...
#
#machine review.gerrithub.io
#        login ...
#        password ...


set -x
set -e

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)
  (package-initialize))"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         -l gerrit.el \
       	 -l gerrit-rest.el \
         -l gerrit-manual-tests.el \
         -f ert-run-tests-batch-and-exit
