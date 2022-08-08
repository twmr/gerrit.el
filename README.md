[![Build Status](https://github.com/thisch/gerrit.el/workflows/CI/badge.svg)](https://github.com/thisch/gerrit.el/actions)
[![MELPA](http://melpa.org/packages/gerrit-badge.svg)](http://melpa.org/#/gerrit)

gerrit.el
=========

Gerrit is a great code review tool and a great git hosting service. This
package provides an emacs interface for

* uploading changes (`gerrit-upload`)
* downloading changes (`gerrit-download` and `gerrit-download-transient`)
* creating a dashboard  (`gerrit-dashboard`)
* creating buffers that contain details about gerrit topics and gerrit
  changes (`gerrit-section-topic-info` and `gerrit-section-change-info`).

The function `gerrit-upload` uses the `transient` package and provides the
following features in addition to uploading new changes (and new patchsets)

* specify reviewers
* set an assignee
* set a topic name
* set WIP flag
* set a ready-for-review flag

Furthermore, a minimalistic open-reviews status-section
(`gerrit-magit-insert-status`) for magit status buffers is available.

This code is tested using gerrit=3.5 and the gerrit version used on
`review.gerrithub.org`.

## News

August 2022:

* [internal] Introduce new `gerrit-rest-sync-v2`.
* Update `gerrit-dashboard-query-alist` (Include the "Your turn" in
  the query, which requires a new-ish gerrit server release >= 3.3).
* Update storage format of `gerrit--accounts-alist`. This allows
  displaying the real names of the users instead usernames in the
  dashboards and the query frontends.
* Display the reviewers instead of the assignee in the default
  dashboard. It is still possible to display the assignee by adding an
  assignee entry to `gerrit-dashboard-columns`.
* Add support for displaying the CC list in the dashboard. To add the CC
  list to the dashboard add "CC" to `gerrit-dashboard-columns`.

April 2022:

* Add a new interactive `gerrit-query` function, which display the results
  of a user defined gerrit query.
* Display clickable links in the gerrit-dashboard.
* Add support for sorting the columns in the dashboard buffers: To
  sort by column either click on the column that you want to sort or
  use `tabulated-list-sort` (<kbd>S</kbd>). [Requested by Gerald]

March 2022:

* Add support for voting to gerrit-dashboard.
  * <kbd>V</kbd> set a "Verified" vote for the entire topic under point.
  * <kbd>C</kbd> set a "Code-Review" vote for the entire topic under point.

Februrary 2022:

* Fix formatting issue when `gerrit-dashboard-columns` is customized.
* Allow toggling between HTTPS and HTTP by changing `gerrit-use-ssl`.

Jannuary 2022:

* Legacy git-review functions were dropped. The
  `gerrit-use-gitreview-interface` variable is no longer used.
* Support displaying the owner-name in the displayed changes in
  `gerrit-download`.

October 2021:

* Add a new transient called `gerrit-download-transient`, which will replace
  the `gerrit-download` function in the future.
* Add support for downloading changes in arbitrary git workspaces using
  `gerrit-download-transient`.
* Add new <kbd>d</kbd> keybinding to gerrit-dashboard.

## Installation

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
    (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
    (global-set-key (kbd "C-x o") 'gerrit-download)))
```

## Authentication

### `.authinfo`, `.authinfo.gpg` and `.netrc`

By default emacs searches in files called `~/.authinfo`, `~/.authinfo.gpg`
and `~/.netrc` in the specified order for credentials. Take a look at the
`auth-sources` variable and its documentation if you want to change this.

You can add an entry with the following format to any of above files

```
machine gerrithostname.org
    login my-gerrit-username
    password xxxx
```
Note: Depending on your auth configuration, Gerrit may expect a generated HTTP password (ex. if you have `git_basic_auth_policy="HTTP_LDAP"`). If there is an HTTP credentials section in your user's account settings page, then an HTTP password needs to be generated and supplied in your auth-source file.

## Pre-commit hook

As you know, there is a gerrit pre-commit hook that must be installed for
every gerrit repo s.t. the Change-ID is added to the bottom of your git
commit messages. This pre-commit hook can be installed using the
`gerrit--ensure-commit-msg-hook-exists` function or e.g. by calling `git
review -s` provided that the `git-review` CLI tool is installed.

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
the individual reviewers using the account information from the gerrit
servers should work with <kbd>TAB</kbd>.

## Dashboards

`gerrit-dashboard` displays a dashboard similar to the one in the gerrit
web-interface.  The currently supported keybindings in a dashboard buffer are

* <kbd>a</kbd> Assign the change under point
* <kbd>A</kbd> Assign the change under point to me
* <kbd>g</kbd> Refresh
* <kbd>o</kbd> Open change under point in browser using `browse-url`
* <kbd>d</kbd> Download change under point in a local clone of the repository
* <kbd>t</kbd> Switch to a buffer containing a topic-overview of the change
  under point
* <kbd>e</kbd> Edit the query and open a new buffer with query results
* <kbd>RET</kbd> Download patch of change under point and display it in new
  buffer

It is possible to click on the links in the dashboard buffer to open new
query buffers like in the gerrit webfrontend.

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
https://github.com/emacsorphanage/magit-gerrit. Uses the
[`ssh`](https://gerrit-review.googlesource.com/Documentation/cmd-index.html)
interface for performing gerrit requests.

* [gerrit-download](https://github.com/chmouel/gerrit-download.el) Downloads
  gerrit change and shows the diff in a diff buffer. Uses the `git-review`
  command line tool under the hood.

* [gerrit-el](https://github.com/iartarisi/gerrit-el) reimplementation of
  the gerrit code review Web UI in emacs.
