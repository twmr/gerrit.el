[![MELPA](http://melpa.org/packages/gerrit-badge.svg)](http://melpa.org/#/gerrit)
[![Build Status](https://travis-ci.org/thisch/gerrit.el.svg?branch=master)](https://travis-ci.org/thisch/gerrit.el)

gerrit.el
=========

This package uses the gerrit REST interface and the `git-review` CLI
tool to add support for

* uploading changes (`gerrit-upload`)
* downloading changes (`gerrit-download`)
* creating a dashboard  (`gerrit-dashboard`).

The emacs interfaces for uploading and downloading changes require
`git-review` and are implemented using the great
[hydra](https://github.com/abo-abo/hydra) package. In the case of the
`gerrit-upload` hydra it is possible to

* specify reviewers
* set an assignee
* set a topic name
* set WIP flag
* specify custom parameters for `git-review`.

This package also contains a minimalistic open-reviews status-section
(`gerrit-magit-insert-status`) for magit status buffers.

This code is tested using git-review=0.27 and gerrit=2.16.

## Installation

Make sure that [git-review](https://opendev.org/opendev/git-review) is
installed and that every cloned gerrit repo has a gerrit-specific pre-commit
hook configured (`git review -s`).

This emacs package is available on
[MELPA](http://melpa.org/#/gerrit).

Example `use-package` config

``` el
(use-package gerrit
  :ensure t
  :custom
  (gerrit-host "gerrit.my.domain")  ;; is needed for REST API calls
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload)
    (global-set-key (kbd "C-x o") 'gerrit-download)))
```

In the case of the gerrit server [review.opendev.org](https://review.opendev.org), the following
variables have to be set:

``` el
(setq gerrit-host "review.opendev.org") ;; runs gerrit-2.13
(setq gerrit-rest-endpoint-prefix "") ;; needed for older(?) gerrit server versions
```

## Screenshots

![gerrit-dashboard](https://user-images.githubusercontent.com/206581/88588506-f8048780-d057-11ea-9c57-ac2a58aadd58.png)

![gerrit-download](https://user-images.githubusercontent.com/206581/88589693-d0162380-d059-11ea-8c96-028659450904.png)

![gerrit-upload](https://user-images.githubusercontent.com/206581/88589947-356a1480-d05a-11ea-8964-e7d0b4bc8a18.png)

## Debugging

TODO

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
