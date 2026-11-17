# my-consult-gh-enhancement

## Disclaimer: 

Made with grok.com


```
;; Fix for Emacs in a terminal.
;; Fix the `markdown-display-inline-images' signals "Cannot show images".
;; That abort drops the GitHub commit prompt.
(advice-add 'markdown-display-inline-images :around
            (lambda (orig &rest args)
              (when (display-images-p)
                (apply orig args))))

;; --------------------------------------------------------------------------

;; Mode-line chips.
;; M-x gh-mode-line-mode turns them off when the work is done.
;; A consult-gh push turns the mode on and watches that repository.
(require 'gh-mode-line)

;; Global minor mode `gh-mode-line-mode'.
;;
;; While the mode is on, the mode line shows one chip per watched
;; repository and a bell count:
;;
;;   [/my-fedora-bootc/workflows: 1] [/emacs-fedora-dynamic/workflows: 1] [bell: 0]
;;
;; A running Actions count uses the workflow face.  A finished run
;; uses the success or failed face.  The bell uses its own face.
;;
;; Polls GitHub every 2 minutes.  A consult-gh push turns the mode on,
;; watches that repository, and polls again after 20 seconds so the
;; new run can appear.  Turn the mode off when the work is done.
;; That stops the poll and clears the chips.

;; ---------------------------------------

;; Release asset list.  M-x my-consult-gh-release-download
(require 'my-consult-gh-release)

;; `my-consult-gh-release-download' lists the uploaded assets of a
;; GitHub release and downloads the one under point.
;;
;; From a consult-gh release buffer the repository and tag are taken
;; from `consult-gh--topic'.  Otherwise the command asks for them.
;;
;; The asset buffer keys are:
;;
;;   RET  download the asset under point
;;   c    copy its download link
;;   q    quit

;; ---------------------------------------

;; Star a repository, or react to the open release note.
;; M-x my-consult-gh-star-and-reaction
(require 'my-consult-gh-star-and-reaction)

;; `my-consult-gh-star-and-reaction' looks at the consult-gh buffer:
;;
;;   repository view   choose star or unstar
;;   release note      choose a reaction
;;
;; Move point to a line and press RET.  q quits the list.
;; The echo area reports the result after GitHub responds.

;; ---------------------------------------

;; From an elfeed entry, list that GitHub release's repository.
;; M-x my-consult-gh-release-list
(require 'my-consult-gh-release-list)

;; `my-consult-gh-release-list' reads the elfeed entry in the current
;; buffer.  The first GitHub release link becomes the repository for
;; `consult-gh-release-list'.
;;
;; A release link looks like:
;;
;;   https://github.com/OWNER/REPO/releases/tag/TAG
;;   https://github.com/OWNER/REPO/releases

