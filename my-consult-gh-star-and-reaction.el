;;; my-consult-gh-star-and-reaction.el --- Star a repo or react to a release -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: grok.com
;; Package-Requires: ((emacs "29.1") (consult-gh "3.1"))

;;; Commentary:

;; `my-consult-gh-star-and-reaction' looks at the consult-gh buffer:
;;
;;   repository view   choose star or unstar
;;   release note      choose a reaction
;;
;; Move point to a line and press RET.  q quits the list.
;; The echo area reports the result after GitHub responds.

;;; Code:

(require 'consult-gh)

(defvar my-consult-gh-star--last-status nil
  "Last star or reaction status string reported in the echo area.")

(defvar my-consult-gh-star--count 0
  "Counter so concurrent requests get distinct process names.")

(defvar-local my-consult-gh-star--host nil
  "GitHub host for requests started from this choice buffer.")

(defconst my-consult-gh-star--reactions
  '(("thumbs up" . "+1")
    ("thumbs down" . "-1")
    ("laugh" . "laugh")
    ("confused" . "confused")
    ("heart" . "heart")
    ("hooray" . "hooray")
    ("rocket" . "rocket")
    ("eyes" . "eyes"))
  "Reaction labels and the GitHub API content values.")

(defun my-consult-gh-star--plain (value)
  "Return VALUE as a string without text properties, or nil."
  (when (stringp value)
    (substring-no-properties value)))

(defun my-consult-gh-star--topic (property)
  "Return text property PROPERTY from `consult-gh--topic'."
  (and (stringp consult-gh--topic)
       (my-consult-gh-star--plain
        (get-text-property 0 property consult-gh--topic))))

(defun my-consult-gh-star--http-code (output)
  "Return the last HTTP status code in OUTPUT, or nil."
  (when (stringp output)
    (let ((start 0)
          (code nil))
      (while (string-match "HTTP[[:space:]]+\\([0-9]\\{3\\}\\)" output start)
        (setq code (match-string 1 output)
              start (match-end 0)))
      code)))

(defun my-consult-gh-star--failure-message (noun exit-code output)
  "Status text when NOUN fails with EXIT-CODE and OUTPUT."
  (let ((http (my-consult-gh-star--http-code output))
        (text (replace-regexp-in-string
               "[\n\r]+" " "
               (string-trim (or output "")))))
    (cond
     (http (format "%s failed: HTTP %s" noun http))
     ((string-empty-p text) (format "%s failed: exit %s" noun exit-code))
     (t (format "%s failed: exit %s: %s" noun exit-code text)))))

(defun my-consult-gh-star--report (status)
  "Show STATUS in the echo area and remember it."
  (setq my-consult-gh-star--last-status status)
  (message "%s" status))

(defun my-consult-gh-star--sentinel (proc _event)
  "Report PROC after that process has exited."
  (when (memq (process-status proc) '(exit signal))
    (let* ((buf (process-buffer proc))
           (output (and (buffer-live-p buf)
                        (with-current-buffer buf (buffer-string))))
           (code (process-exit-status proc)))
      (if (and (eq (process-status proc) 'exit) (zerop code))
          (my-consult-gh-star--report (process-get proc :success-message))
        (my-consult-gh-star--report
         (my-consult-gh-star--failure-message
          (process-get proc :failure-noun) code output)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun my-consult-gh-star--start (command success-message failure-noun host)
  "Run gh COMMAND in the background.
SUCCESS-MESSAGE is reported on exit 0.  FAILURE-NOUN names the
action in a failure report.  HOST is a GitHub host, or nil.
The echo area is updated from the process sentinel only after
the process exits."
  (unless (executable-find "gh")
    (user-error "gh not found"))
  (setq my-consult-gh-star--count (1+ my-consult-gh-star--count))
  (let* ((process-environment
          (if (and host (not (string-empty-p host)))
              (cons (concat "GH_HOST=" host) process-environment)
            process-environment))
         (buf (generate-new-buffer " *my-consult-gh-star*"))
         (proc (make-process
                :name (format "my-consult-gh-star-%s" my-consult-gh-star--count)
                :buffer buf
                :command command
                :connection-type 'pipe
                :noquery t
                :sentinel #'ignore)))
    (process-put proc :success-message success-message)
    (process-put proc :failure-noun failure-noun)
    (set-process-sentinel proc #'my-consult-gh-star--sentinel)
    proc))

(defun my-consult-gh-star--release-id (repo tag)
  "Return the numeric release id of TAG in REPO."
  (unless (executable-find "gh")
    (user-error "gh not found"))
  (with-temp-buffer
    (let ((exit-code (call-process "gh" nil t nil
                                   "release" "view" tag "-R" repo
                                   "--json" "databaseId" "--jq" ".databaseId")))
      (unless (eq exit-code 0)
        (user-error "Could not get release id: %s" (string-trim (buffer-string))))
      (let ((id (string-trim (buffer-string))))
        (if (string-match-p "\\`[0-9]+\\'" id)
            id
          (user-error "Could not get release id: %s" id))))))

(defun my-consult-gh-star-and-reaction-at-point ()
  "Star, unstar, or react using the line under point."
  (interactive)
  (let ((kind (get-text-property (point) :kind))
        (repo (get-text-property (point) :repo))
        (host my-consult-gh-star--host))
    (cond
     ((eq kind 'star)
      (my-consult-gh-star--start
       (list "gh" "api" "--method" "PUT" (format "/user/starred/%s" repo))
       "project starred" "star" host))
     ((eq kind 'unstar)
      (my-consult-gh-star--start
       (list "gh" "api" "--method" "DELETE" (format "/user/starred/%s" repo))
       "project unstarred" "unstar" host))
     ((eq kind 'reaction)
      (my-consult-gh-star--start
       (list "gh" "api" "--method" "POST"
             "-H" "Accept:application/vnd.github+json"
             "-f" (format "content=%s" (get-text-property (point) :content))
             (format "/repos/%s/releases/%s/reactions"
                     repo (get-text-property (point) :release-id)))
       (format "reacted with %s" (get-text-property (point) :label))
       "reaction" host))
     (t (user-error "No choice on this line")))))

(defvar my-consult-gh-star-and-reaction-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'my-consult-gh-star-and-reaction-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my-consult-gh-star-and-reaction-mode'.")

(define-derived-mode my-consult-gh-star-and-reaction-mode special-mode "GH Mark"
  "Choice list for starring a repository or reacting to a release."
  :interactive nil)

(defun my-consult-gh-star--show (name title lines host)
  "Show choice buffer NAME with TITLE and LINES.
LINES are strings with text properties.  HOST is stored for RET."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (my-consult-gh-star-and-reaction-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local my-consult-gh-star--host host)
        (insert title "\n\n")
        (dolist (line lines)
          (insert line "\n")))
      (goto-char (or (text-property-not-all (point-min) (point-max) :kind nil)
                     (point-min))))
    (pop-to-buffer buf)))

(defun my-consult-gh-star--show-star (repo host)
  "Show the star and unstar lines for REPO."
  (my-consult-gh-star--show
   (format "*my-consult-gh-star: %s*" repo)
   repo
   (list (propertize "star" :kind 'star :repo repo)
         (propertize "unstar" :kind 'unstar :repo repo))
   host))

(defun my-consult-gh-star--show-reactions (repo tag host)
  "Show the reaction lines for TAG in REPO."
  (let ((id (my-consult-gh-star--release-id repo tag)))
    (my-consult-gh-star--show
     (format "*my-consult-gh-reaction: %s %s*" repo tag)
     (format "%s %s" repo tag)
     (mapcar (lambda (reaction)
               (propertize (car reaction)
                           :kind 'reaction
                           :label (car reaction)
                           :content (cdr reaction)
                           :repo repo
                           :tag tag
                           :release-id id))
             my-consult-gh-star--reactions)
     host)))

;;;###autoload
(defun my-consult-gh-star-and-reaction ()
  "Star the repository, or react to the release note.
In a consult-gh repository buffer, offer star and unstar.
In a consult-gh release buffer, offer the reaction list."
  (interactive)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let ((repo (my-consult-gh-star--topic :repo))
         (tag (my-consult-gh-star--topic :tagname))
         (host (consult-gh--auth-account-host)))
     (cond
      ((and (bound-and-true-p consult-gh-release-view-mode) repo tag)
       (my-consult-gh-star--show-reactions repo tag host))
      ((and (bound-and-true-p consult-gh-repo-view-mode) repo)
       (my-consult-gh-star--show-star repo host))
      (t (user-error "Open a repository or a release note first"))))))

(provide 'my-consult-gh-star-and-reaction)

;;; my-consult-gh-star-and-reaction.el ends here
