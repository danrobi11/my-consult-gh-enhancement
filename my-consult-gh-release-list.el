;;; my-consult-gh-release-list.el --- Release list from an elfeed entry -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: grok.com
;; Package-Requires: ((emacs "29.1") (consult-gh "3.1") (elfeed "3.4"))

;;; Commentary:

;; `my-consult-gh-release-list' reads the elfeed entry in the current
;; buffer.  The first GitHub release link becomes the repository for
;; `consult-gh-release-list'.
;;
;; A release link looks like:
;;
;;   https://github.com/OWNER/REPO/releases/tag/TAG
;;   https://github.com/OWNER/REPO/releases

;;; Code:

(require 'consult-gh)
(require 'elfeed-show)

(defconst my-consult-gh-release-list--url-regexp
  (concat "\\`https?://\\(?:www\\.\\)?github\\.com/"
          "\\([^/?#]+\\)/\\([^/?#]+\\)/releases"
          "\\(?:/tag/[^/?#]+\\|/latest\\)?"
          "/?\\(?:[?#].*\\)?\\'")
  "Match a GitHub release URL.  Groups 1 and 2 are owner and repo.")

(defun my-consult-gh-release-list--repo-from-url (url)
  "Return OWNER/REPO when URL is a GitHub release link, or nil."
  (when (and (stringp url)
             (string-match my-consult-gh-release-list--url-regexp url))
    (concat (match-string 1 url) "/" (match-string 2 url))))

(defun my-consult-gh-release-list--shr-urls ()
  "Return `shr-url' values in the current buffer, from top to bottom."
  (let ((urls nil)
        (pos (point-min)))
    (while (and pos (< pos (point-max)))
      (setq pos (text-property-not-all pos (point-max) 'shr-url nil))
      (when pos
        (let ((url (get-text-property pos 'shr-url)))
          (when (stringp url)
            (push url urls)))
        (setq pos (next-single-property-change pos 'shr-url))))
    (nreverse urls)))

(defun my-consult-gh-release-list--text-urls ()
  "Return GitHub URLs written as text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (urls)
      (while (re-search-forward
              "https?://\\(?:www\\.\\)?github\\.com/[^[:space:]<>\"]+"
              nil t)
        (push (match-string-no-properties 0) urls))
      (nreverse urls))))

(defun my-consult-gh-release-list--repo ()
  "Return OWNER/REPO for the first GitHub release link in this buffer."
  (or (seq-some #'my-consult-gh-release-list--repo-from-url
                (my-consult-gh-release-list--shr-urls))
      (seq-some #'my-consult-gh-release-list--repo-from-url
                (my-consult-gh-release-list--text-urls))))

;;;###autoload
(defun my-consult-gh-release-list ()
  "List releases for the GitHub release link in this elfeed entry.
The link is read from the current `elfeed-show-mode' buffer.
Signal an error when the buffer has no GitHub release link."
  (interactive)
  (unless (derived-mode-p 'elfeed-show-mode)
    (user-error "Open an elfeed entry first"))
  (let ((repo (my-consult-gh-release-list--repo)))
    (unless repo
      (user-error "No GitHub release link in this entry"))
    (consult-gh-release-list repo)))

(provide 'my-consult-gh-release-list)

;;; my-consult-gh-release-list.el ends here
