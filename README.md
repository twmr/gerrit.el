[![Build
Status](https://travis-ci.org/thisch/gerrit.el.svg?branch=master)](https://travis-ci.org/thisch/gerrit.el)

gerrit.el
=========

TODO

## Installation

This package is NOT YET available on
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