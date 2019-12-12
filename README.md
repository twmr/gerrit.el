[![Build
Status](https://travis-ci.org/thisch/gerrit.el.svg?branch=master)](https://travis-ci.org/thisch/gerrit.el)

gerrit.el
=========

TODO

This code is tested using git-review=0.27 and gerrit=2.x.

## Installation

Make sure that [git-review](https://opendev.org/opendev/git-review) is
installed and that every cloned gerrit repo has a gerrit-specific pre-commit
hook configured (`git review -s`).

This emacs package is NOT YET available on
[MELPA](http://melpa.org/#/gerrit).

Example `use-package` config

``` el
(use-package gerrit
  :load-path "~/path/to/repo/"
  :custom
  (gerrit-host "gerrit.my.domain")
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload)
    (global-set-key (kbd "C-x o") 'gerrit-download)))
```

## Similar elisp packages

* [magit-gerrit](https://github.com/darcylee/magit-gerrit) Fork of
unmaintained https://github.com/terranpro/magit-gerrit. Uses the
[`ssh`](https://gerrit-review.googlesource.com/Documentation/cmd-index.html)
interface for performing gerrit requests.


* [gerrit-download](https://github.com/chmouel/gerrit-download.el) Downloads
  gerrit change and shows the diff in a diff buffer. Uses `git-review` under
  the hood.

* [gerrit-el](https://github.com/iartarisi/gerrit-el) reimplementation of
  the gerrit code review Web UI in emacs.