#!/bin/sh -e

set -x
set -e

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (setq package-install-upgrade-built-in t)
  (package-install 'seq)
  (progn (unload-feature 'seq t) (require 'seq))
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)
  (package-initialize))"

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(unless package-archive-contents (package-refresh-contents))" \
         --eval "(unless (package-installed-p 'cl-lib) (package-install 'cl-lib))" \
         --eval "(unless (package-installed-p 'magit) (package-install 'magit))" \
         --eval "(unless (package-installed-p 'dash) (package-install 'dash)" \
         --eval "(unless (package-installed-p 's) (package-install 's))"

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
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         gerrit.el gerrit-rest.el test/gerrit.el-test.el

"$EMACS" -Q -batch \
         --eval '(progn (package-initialize) (byte-compile-file "gerrit-rest.el"))'
"$EMACS" -Q -batch \
         --eval '(package-initialize)' \
         -l gerrit-rest.el \
         --eval '(byte-compile-file "gerrit.el")'


"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         -l gerrit.el \
         -l gerrit-rest.el \
         -l test/gerrit.el-test.el \
         -f ert-run-tests-batch-and-exit
