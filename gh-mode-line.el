;;; gh-mode-line.el --- GitHub workflow and bell chips on the mode line -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: grok.com
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Global minor mode `gh-mode-line-mode'.
;;
;; While the mode is on, the mode line shows one chip per watched
;; repository and a bell count:
;;
;;   [/my-fedora-bootc/workflows: 1] [/emacs-fedora-dynamic/workflows: 1] [bell: 0]
;;
;; A running Actions count uses the workflow face.  A finished run
;; uses the success or failed face.  The bell uses its own face.
;; These colors are temporary.
;;
;; Polls GitHub every 2 minutes.  A consult-gh push turns the mode on,
;; watches that repository, and polls again after 20 seconds so the
;; new run can appear.  Turn the mode off when the work is done.
;; That stops the poll and clears the chips.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup gh-mode-line nil
  "Mode-line chips for GitHub Actions and the notification inbox."
  :group 'consult-gh)

(defcustom gh-mode-line-interval 120
  "Seconds between GitHub polls while `gh-mode-line-mode' is on."
  :type 'integer
  :group 'gh-mode-line)

;; Temporary colors.  Replace these once the real ones are chosen.
(defface gh-mode-line-workflow
  '((t :foreground "#e6c200" :weight bold))
  "Face for a watched repo whose workflow is running or not started."
  :group 'gh-mode-line)

(defface gh-mode-line-workflow-success
  '((t :foreground "#3ddc97" :weight bold))
  "Face for a watched repo whose latest workflows succeeded."
  :group 'gh-mode-line)

(defface gh-mode-line-workflow-failed
  '((t :foreground "#ff5c57" :weight bold))
  "Face for a watched repo whose latest workflows failed."
  :group 'gh-mode-line)

(defface gh-mode-line-bell
  '((t :foreground "#6cb6ff" :weight bold))
  "Face for the unread GitHub notification count."
  :group 'gh-mode-line)

(defvar gh-mode-line--repos nil
  "Watched repositories, in the order they were pushed.
Each element is a plist: :full :name :since :phase :value :token.")

(defvar gh-mode-line--bell "0"
  "Unread notification count shown in the bell chip.")

(defvar gh-mode-line--bell-token 0
  "Increments so a late bell process cannot overwrite a newer poll.")

(defvar gh-mode-line--timer nil)
(defvar gh-mode-line--kick-timer nil)

(defun gh-mode-line--gh ()
  "Return the gh executable, or nil."
  (executable-find "gh"))

(defun gh-mode-line--short-name (full)
  "Repository name from an owner/name string FULL."
  (car (last (split-string full "/" t))))

(defun gh-mode-line--running-p (status)
  "Return non-nil when a GitHub Actions STATUS is still in progress."
  (member status '("queued" "in_progress" "waiting" "pending" "requested")))

(defun gh-mode-line--failed-p (conclusion)
  "Return non-nil when a finished run CONCLUSION should show as failed."
  (member conclusion '("failure" "cancelled" "timed_out" "startup_failure"
                       "action_required" "stale")))

(defun gh-mode-line--workflow-face (value)
  "Face for a workflow chip whose displayed VALUE is a count or a result."
  (cond
   ((equal value "failed") 'gh-mode-line-workflow-failed)
   ((equal value "success") 'gh-mode-line-workflow-success)
   (t 'gh-mode-line-workflow)))

(defun gh-mode-line--workflow-chip (repo)
  "Propertized mode-line chip for REPO."
  (propertize (format "[/%s/workflows: %s]"
                      (plist-get repo :name)
                      (plist-get repo :value))
              'face (gh-mode-line--workflow-face (plist-get repo :value))))

(defun gh-mode-line--bell-chip ()
  "Propertized mode-line chip for the notification inbox."
  (propertize (format "[bell: %s]" gh-mode-line--bell)
              'face 'gh-mode-line-bell))

(defun gh-mode-line-string ()
  "Mode-line text, or nil when `gh-mode-line-mode' is off."
  (when gh-mode-line-mode
    (concat " "
            (mapconcat #'identity
                       (append (mapcar #'gh-mode-line--workflow-chip
                                       gh-mode-line--repos)
                               (list (gh-mode-line--bell-chip)))
                       " "))))

(defun gh-mode-line--find (full)
  "Return the watched plist for FULL, or nil."
  (cl-find full gh-mode-line--repos
           :key (lambda (repo) (plist-get repo :full))
           :test #'equal))

(defun gh-mode-line-watch (full)
  "Watch FULL and turn `gh-mode-line-mode' on.
FULL is an owner/name string.  A new push while a run is still
going keeps the earlier start time, so both runs stay in the count.
A push after the previous run finished starts a new watch window."
  (when (and (stringp full) (not (string-empty-p full)))
    (let ((repo (gh-mode-line--find full))
          (since (time-subtract (current-time) 15)))
      (if (null repo)
          (setq gh-mode-line--repos
                (append gh-mode-line--repos
                        (list (list :full full
                                    :name (gh-mode-line--short-name full)
                                    :since since
                                    :phase 'waiting
                                    :value "0"
                                    :token 0))))
        (unless (eq (plist-get repo :phase) 'running)
          (plist-put repo :since since)
          (plist-put repo :phase 'waiting)
          (plist-put repo :value "0"))))
    (if gh-mode-line-mode
        (gh-mode-line--kick)
      (gh-mode-line-mode 1))))

(defun gh-mode-line--after-push (orig file repo &rest args)
  "Call ORIG to push FILE in REPO, then watch REPO when the push lands.
ARGS are the remaining arguments of `consult-gh--create-commit-single-file'."
  (let ((result (apply orig file repo args)))
    (when result
      (gh-mode-line-watch repo))
    result))

(advice-add 'consult-gh--create-commit-single-file :around
            #'gh-mode-line--after-push)

(defun gh-mode-line--parse-time (value)
  "Decode a GitHub timestamp VALUE, or return nil."
  (and (stringp value)
       (condition-case nil
           (date-to-time value)
         (error nil))))

(defun gh-mode-line--apply-runs (repo runs)
  "Set REPO's phase and value from GitHub Actions RUNS."
  (let* ((since (plist-get repo :since))
         (relevant
          (cl-remove-if-not
           (lambda (run)
             (let ((created (gh-mode-line--parse-time
                             (alist-get 'createdAt run))))
               (and created (not (time-less-p created since)))))
           runs))
         (running (cl-count-if
                   (lambda (run)
                     (gh-mode-line--running-p (alist-get 'status run)))
                   relevant)))
    (cond
     ((null relevant)
      (plist-put repo :phase 'waiting)
      (plist-put repo :value "0"))
     ((> running 0)
      (plist-put repo :phase 'running)
      (plist-put repo :value (number-to-string running)))
     (t
      (plist-put repo :phase 'done)
      (plist-put repo :value
                 (if (cl-some (lambda (run)
                                (gh-mode-line--failed-p
                                 (alist-get 'conclusion run)))
                              relevant)
                     "failed"
                   "success"))))))

(defun gh-mode-line--read-runs (text)
  "Parse a gh run-list JSON array TEXT into an alist list."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (condition-case nil
           (json-parse-string text
                              :object-type 'alist
                              :array-type 'list
                              :null-object nil
                              :false-object :false)
         (error nil))))

(defun gh-mode-line--finish-process (proc buffer token current apply)
  "Read PROC output from BUFFER and call APPLY when TOKEN is still CURRENT."
  (unwind-protect
      (when (and (eq (process-exit-status proc) 0)
                 (eq token current))
        (funcall apply (with-current-buffer buffer
                         (buffer-string))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when gh-mode-line-mode
      (force-mode-line-update t))))

(defun gh-mode-line--spawn (name args token current apply)
  "Run gh ARGS asynchronously.  APPLY receives the output string.
TOKEN is compared with CURRENT when the process exits.  CURRENT is
a function of no arguments so a newer poll can invalidate this one."
  (let* ((gh (gh-mode-line--gh))
         (buffer (generate-new-buffer (format " *%s*" name))))
    (if (null gh)
        (progn
          (kill-buffer buffer)
          nil)
      (make-process
       :name name
       :buffer buffer
       :command (append (list gh) args)
       :noquery t
       :connection-type 'pipe
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (gh-mode-line--finish-process
            proc buffer token (funcall current) apply)))))))

(defun gh-mode-line--poll-bell ()
  "Refresh the unread notification count."
  (let ((token (setq gh-mode-line--bell-token
                     (1+ gh-mode-line--bell-token))))
    (gh-mode-line--spawn
     "gh-mode-line-bell"
     '("api" "--paginate" "notifications" "--jq" ".[].id")
     token
     (lambda () gh-mode-line--bell-token)
     (lambda (text)
       (when gh-mode-line-mode
         (setq gh-mode-line--bell
               (number-to-string
                (length (split-string text "\n" t)))))))))

(defun gh-mode-line--poll-repo (repo)
  "Refresh the Actions status of REPO."
  (let ((token (1+ (or (plist-get repo :token) 0))))
    (plist-put repo :token token)
    (gh-mode-line--spawn
     "gh-mode-line-run"
     (list "run" "list"
           "--repo" (plist-get repo :full)
           "--limit" "15"
           "--json" "status,conclusion,createdAt")
     token
     (lambda () (plist-get repo :token))
     (lambda (text)
       (when (and gh-mode-line-mode
                  (gh-mode-line--find (plist-get repo :full)))
         (when-let* ((runs (gh-mode-line--read-runs text)))
           (gh-mode-line--apply-runs repo runs)))))))

(defun gh-mode-line--poll ()
  "Poll the bell and every watched repository."
  (when gh-mode-line-mode
    (gh-mode-line--poll-bell)
    (dolist (repo gh-mode-line--repos)
      (gh-mode-line--poll-repo repo))))

(defun gh-mode-line--kick ()
  "Poll now, then once more so a run that has not started yet can show up."
  (gh-mode-line--poll)
  (when (timerp gh-mode-line--kick-timer)
    (cancel-timer gh-mode-line--kick-timer))
  (setq gh-mode-line--kick-timer
        (run-with-timer 20 nil #'gh-mode-line--poll)))

(defun gh-mode-line--start-timer ()
  "Start the 2 minute poll, replacing any previous timer."
  (when (timerp gh-mode-line--timer)
    (cancel-timer gh-mode-line--timer))
  (setq gh-mode-line--timer
        (run-with-timer gh-mode-line-interval
                        gh-mode-line-interval
                        #'gh-mode-line--poll)))

(defun gh-mode-line--stop-timer ()
  "Cancel poll timers."
  (when (timerp gh-mode-line--timer)
    (cancel-timer gh-mode-line--timer))
  (when (timerp gh-mode-line--kick-timer)
    (cancel-timer gh-mode-line--kick-timer))
  (setq gh-mode-line--timer nil
        gh-mode-line--kick-timer nil))

;;;###autoload
(define-minor-mode gh-mode-line-mode
  "Show GitHub workflow chips and the bell count on the mode line.
A consult-gh push turns this on for the repository that was pushed.
Turn it off when that work is finished."
  :global t
  :lighter ""
  (if gh-mode-line-mode
      (progn
        (gh-mode-line--start-timer)
        (gh-mode-line--kick))
    (gh-mode-line--stop-timer)
    (setq gh-mode-line--repos nil
          gh-mode-line--bell "0"))
  (force-mode-line-update t))

(provide 'gh-mode-line)

;;; gh-mode-line.el ends here
