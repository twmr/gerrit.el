#!/bin/sh -e

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)
  (package-initialize))"

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval '(package-refresh-contents)' \
         --eval "(unless (package-installed-p 'cl-lib) (package-install 'cl-lib))" \
         --eval "(unless (package-installed-p 'hydra) (package-install 'hydra))" \
         --eval "(unless (package-installed-p 'magit) (package-install 'magit))" \
         --eval "(unless (package-installed-p 'dash) (package-install 'dash))"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l gerrit.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         gerrit.el test/gerrit.el-test.el

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l gerrit.el \
         -l test/gerrit.el-test.el \
         -f ert-run-tests-batch-and-exit
