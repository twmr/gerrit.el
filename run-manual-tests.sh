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

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         -l gerrit.el \
         -l gerrit-rest.el \
         -l gerrit-manual-tests.el \
         -f ert-run-tests-batch-and-exit
