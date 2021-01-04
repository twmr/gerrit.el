[![MELPA](http://melpa.org/packages/gerrit-badge.svg)](http://melpa.org/#/gerrit)
[![Build Status](https://travis-ci.org/thisch/gerrit.el.svg?branch=master)](https://travis-ci.org/thisch/gerrit.el)

gerrit.el
=========

Gerrit is a great code review tool and a great git hosting serivce. This
package provides an emacs interface for

* uploading changes (`gerrit-upload`)
* downloading changes (`gerrit-download`)
* creating a dashboard  (`gerrit-dashboard`)
* creating buffers that contain details about gerrit topics an gerrit
  changes (`gerrit-section-topic-info` and `gerrit-section-change-info`).

The function `gerrit-upload` uses the `transient` package (if
`gerrit-use-gitreview-interface` is set to `nil`) and provides the following
features in addition to uploading new changes (and new patchsets)

* specify reviewers
* set an assignee
* set a topic name
* set WIP flag
* set a ready-for-review flag

Furthermore, a minimalistic open-reviews status-section
(`gerrit-magit-insert-status`) for magit status buffers is available.

This code is tested using gerrit=2.16 and the gerrit version used on
`review.gerrithub.org`.

## Legacy git-review functions and new style REST-based functions

Initially this package depended on the `git-review` command line tool, which
was used for downloading and uploading gerrit changes. The functions are
still provided but they are no longer actively updated. They can be used by
setting `gerrit-use-gitreview-interface` to `t`, which is the default value.

The recommended interface for downloading and uploading gerrit changes is
the new REST-API only interface that has to be activated by setting
`gerrit-use-gitreview-interface` to `nil`.

## Installation

If you want to use the git-review interface, make sure that
[git-review](https://opendev.org/opendev/git-review) is installed and that
every cloned gerrit repo has a gerrit-specific pre-commit hook configured
(`git review -s`).

This emacs package is available on
[MELPA](http://melpa.org/#/gerrit).

Example `use-package` config

``` el
(use-package gerrit
  :ensure t
  :custom
  (gerrit-host "gerrit.my.domain")  ;; is needed for REST API calls
  (gerrit-use-gitreview-interface nil)
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
    (global-set-key (kbd "C-x o") 'gerrit-download-new)))
```

In the case of the gerrit server [review.opendev.org](https://review.opendev.org), the following
variables have to be set:

``` el
(setq gerrit-host "review.opendev.org") ;; runs gerrit-2.13
(setq gerrit-rest-endpoint-prefix "") ;; needed for older(?) gerrit server versions
```

## Authentication

### netrc

TODO

### keyring

Make sure that emacs was compiled with dbus support (requires devel pacakges
of libdbus)

Load dbus library


Make sure that `(dbus-list-names :session)` returns a non-empty list,
otherwise dbus is not working

Load the `secrets` library, which depends on a working dbus setup.

`(load-library 'secrets)` and check that the `secrets-enabled` variable is
`t`.

Now you can list the secrets using `secrets-show-secrets`.

Make sure that you have a secret for gerrit

https://github.com/mrvdb/emacs-config
```
;; Use only password-store
(use-package auth-source-pass
  :ensure password-store
  :init
  (auth-source-pass-enable)
  :config
  ;; Make sure it's the only mechanism
  (setq auth-sources '(password-store))
```

If not you can add one using ....

## Screenshots

![gerrit-dashboard](https://user-images.githubusercontent.com/206581/88588506-f8048780-d057-11ea-9c57-ac2a58aadd58.png)

![gerrit-download](https://user-images.githubusercontent.com/206581/88589693-d0162380-d059-11ea-8c96-028659450904.png)

![gerrit-upload](https://user-images.githubusercontent.com/206581/88589947-356a1480-d05a-11ea-8964-e7d0b4bc8a18.png)

![gerrit-section-change-info](https://user-images.githubusercontent.com/206581/101976331-9dee1280-3c44-11eb-8d01-629d3634da43.png)

## Usage notes for the `gerrit-upload` transient

All settings entered in the `gerrit-upload` transient are saved to a file,
whose filename is in the `transient-history-file` variable. This file is
updated in the `kill-emacs-hook`, which is run when the emacs
process/daemon is stopped using `(kill-emacs)`.

If you are using `systemd` for starting emacs as a daemon, make sure that your
unit files contains

```
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
```

You can cycle through the history by using <kbd>M-p</kbd> and
<kbd>M-n</kbd>.

The reviewers have to be added as a comma-separated string. Completion of
the individual reviewres using the account information from the gerrit
servers should work with <kbd>TAB</kbd>.

## Dashboards

`gerrit-dashboard` displays a dashboard similar to the one in the gerrit
webinterface.  The currently supported keybindings in a dashboard buffer are

* <kbd>a</kbd> Assign the change under point
* <kbd>A</kbd> Assign the change under point to me
* <kbd>g</kbd> Refresh
* <kbd>o</kbd> Open change under point in browser using `browse-url`
* <kbd>t</kbd> Switch to a buffer containing a topic-overview of the change under point
* <kbd>RET</kbd> Download patch of change under point and display it in new
  buffer

If you want to create multiple dashboards you can create a dashboard using

```el
(defun gerrit-dashboard-standup ()
  (interactive)
  (let ((gerrit-dashboard-query-alist
         '(
           ("Waiting for +1" . "is:open assignee:groupX label:Code-Review=0")
           ("Waiting for +2" . "is:open assignee:groupX label:Code-Review=1")
           )
         )
        (gerrit-dashboard-buffer-name "*gerrit-groupX-standup*")
        )
    (gerrit-dashboard)))
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
