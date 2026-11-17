;;; my-consult-gh-release.el --- Pick and download one GitHub release asset -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: grok.com
;; Package-Requires: ((emacs "29.1") (consult-gh "3.1"))

;;; Commentary:

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

;;; Code:

(require 'consult-gh)

(defvar my-consult-gh-release--last-status nil
  "Last download status string reported in the echo area.")

(defvar my-consult-gh-release--download-count 0
  "Counter so concurrent downloads get distinct process names.")

(defvar-local my-consult-gh-release--host nil
  "GitHub host used for downloads started from this asset buffer.")

(defun my-consult-gh-release--plain (value)
  "Return VALUE as a string without text properties, or nil."
  (when (stringp value)
    (substring-no-properties value)))

(defun my-consult-gh-release--field (object key)
  "Return KEY from alist OBJECT.
JSON parsing may use either a symbol or a string as the key."
  (or (alist-get key object)
      (alist-get (symbol-name key) object nil nil #'equal)))

(defun my-consult-gh-release--glob-escape (name)
  "Escape glob metacharacters in NAME.
`gh release download' treats the pattern as a glob.  A literal
asset name must not match a different file."
  (replace-regexp-in-string "\\([*?[\\\\]\\)" "\\\\\\1" name t))

(defun my-consult-gh-release--http-code (output)
  "Return the last HTTP status code in OUTPUT, or nil."
  (when (stringp output)
    (let ((start 0)
          (code nil))
      (while (string-match "HTTP[[:space:]]+\\([0-9]\\{3\\}\\)" output start)
        (setq code (match-string 1 output)
              start (match-end 0)))
      code)))

(defun my-consult-gh-release--failure-message (exit-code output)
  "Status text for a download that ended with EXIT-CODE and OUTPUT."
  (let ((http (my-consult-gh-release--http-code output))
        (text (replace-regexp-in-string
               "[\n\r]+" " "
               (string-trim (or output "")))))
    (cond
     (http (format "Download failed: HTTP %s" http))
     ((string-empty-p text) (format "Download failed: exit %s" exit-code))
     (t (format "Download failed: exit %s: %s" exit-code text)))))

(defun my-consult-gh-release--size (bytes)
  "Return BYTES as a short size string."
  (let ((n (or bytes 0)))
    (cond
     ((>= n 1048576) (format "%.1f MB" (/ n 1048576.0)))
     ((>= n 1024) (format "%.1f KB" (/ n 1024.0)))
     (t (format "%d B" n)))))

(defun my-consult-gh-release--pad (text width)
  "Pad TEXT on the right to WIDTH characters."
  (concat text (make-string (max 0 (- width (string-width text))) ?\s)))

(defun my-consult-gh-release--report (status)
  "Show STATUS in the echo area and remember it."
  (setq my-consult-gh-release--last-status status)
  (message "%s" status))

(defun my-consult-gh-release--download-sentinel (proc _event)
  "Report the download in PROC after that process has exited."
  (when (memq (process-status proc) '(exit signal))
    (let* ((buf (process-buffer proc))
           (output (and (buffer-live-p buf)
                        (with-current-buffer buf (buffer-string))))
           (name (process-get proc :asset-name))
           (dir (process-get proc :directory))
           (code (process-exit-status proc)))
      (if (and (eq (process-status proc) 'exit) (zerop code))
          (my-consult-gh-release--report
           (format "Downloaded %s to %s"
                   name
                   (abbreviate-file-name (directory-file-name dir))))
        (my-consult-gh-release--report
         (my-consult-gh-release--failure-message code output)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun my-consult-gh-release--start-download (repo tag name dir &optional host clobber)
  "Download asset NAME from TAG in REPO into DIR.
HOST is a GitHub host, or nil.  CLOBBER replaces an existing file.
The process runs in the background.  The echo area says that the
download has started, and the sentinel reports success or failure
only after the process exits."
  (setq my-consult-gh-release--download-count
        (1+ my-consult-gh-release--download-count))
  ;; Say that the download started before the process exists.  The
  ;; sentinel is the only place that reports success or failure, and
  ;; it runs after the process exits.  A fast process must not have
  ;; its finished status overwritten by this line.
  (my-consult-gh-release--report (format "Downloading %s..." name))
  (let* ((process-environment
          (if (and host (not (string-empty-p host)))
              (cons (concat "GH_HOST=" host) process-environment)
            process-environment))
         (buf (generate-new-buffer " *my-consult-gh-release-download*"))
         (cmd (append (list "gh" "release" "download" tag
                            "-R" repo
                            "-p" (my-consult-gh-release--glob-escape name)
                            "--dir" dir)
                      (when clobber (list "--clobber"))))
         (proc (make-process
                :name (format "my-consult-gh-release-download-%s"
                              my-consult-gh-release--download-count)
                :buffer buf
                :command cmd
                :connection-type 'pipe
                :noquery t
                :sentinel #'ignore)))
    ;; Attach the name before the real sentinel can run.  Setting the
    ;; sentinel after the process has already exited runs it at once.
    (process-put proc :asset-name name)
    (process-put proc :directory dir)
    (set-process-sentinel proc #'my-consult-gh-release--download-sentinel)
    proc))

(defun my-consult-gh-release--assets (repo tag)
  "Return the uploaded assets of TAG in REPO."
  (unless (executable-find "gh")
    (user-error "gh not found"))
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "gh" nil t nil
                            (list "release" "view" tag "-R" repo "--json" "assets"))))
      (unless (eq exit-code 0)
        (user-error "Could not list assets: %s" (string-trim (buffer-string))))
      (let ((assets (my-consult-gh-release--field
                     (json-parse-string (buffer-string)
                                        :object-type 'alist
                                        :array-type 'list)
                     'assets)))
        (seq-filter
         (lambda (asset)
           (equal (my-consult-gh-release--field asset 'state) "uploaded"))
         assets)))))

(defun my-consult-gh-release--asset-at-point ()
  "Plist of the asset at point, or nil."
  (let ((name (get-text-property (point) :name)))
    (when name
      (list :name name
            :url (get-text-property (point) :url)
            :repo (get-text-property (point) :repo)
            :tag (get-text-property (point) :tag)))))

(defun my-consult-gh-release-copy-link ()
  "Copy the download link of the asset under point."
  (interactive)
  (let ((url (get-text-property (point) :url)))
    (if (not (and url (not (string-empty-p url))))
        (user-error "No asset on this line")
      (kill-new url)
      (message "Copied download link: %s" url))))

(defun my-consult-gh-release-download-at-point ()
  "Download the asset under point into a chosen directory."
  (interactive)
  (let ((asset (my-consult-gh-release--asset-at-point)))
    (unless asset
      (user-error "No asset on this line"))
    (let* ((name (plist-get asset :name))
           (dir (read-directory-name
                 (format "Download %s to directory: " name)
                 nil nil t))
           (dest (expand-file-name name dir))
           (clobber nil))
      (when (file-exists-p dest)
        (unless (y-or-n-p (format "Replace %s? " dest))
          (user-error "Download canceled"))
        (setq clobber t))
      (my-consult-gh-release--start-download
       (plist-get asset :repo)
       (plist-get asset :tag)
       name
       (file-name-as-directory dir)
       my-consult-gh-release--host
       clobber))))

(defvar my-consult-gh-release-assets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my-consult-gh-release-download-at-point)
    (define-key map (kbd "c") #'my-consult-gh-release-copy-link)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my-consult-gh-release-assets-mode'.")

(define-derived-mode my-consult-gh-release-assets-mode special-mode "GH Assets"
  "Buffer of downloadable assets for one GitHub release."
  :interactive nil)

(defun my-consult-gh-release--show-assets (repo tag assets host)
  "Show ASSETS for TAG in REPO.  HOST is stored for later downloads."
  (let ((buf (get-buffer-create
              (format "*my-consult-gh-release-assets: %s %s*" repo tag)))
        (width (apply #'max (mapcar
                             (lambda (asset)
                               (string-width (or (my-consult-gh-release--field asset 'name) "")))
                             assets))))
    (with-current-buffer buf
      (my-consult-gh-release-assets-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local my-consult-gh-release--host host)
        (insert (format "Assets for %s %s\n\n" repo tag))
        (insert (format "%s  %10s  %s\n"
                        (my-consult-gh-release--pad "NAME" width)
                        "SIZE"
                        "DOWNLOADS"))
        (dolist (asset assets)
          (let* ((name (or (my-consult-gh-release--field asset 'name) ""))
                 (url (or (my-consult-gh-release--field asset 'url) ""))
                 (line (format "%s  %10s  %s\n"
                               (my-consult-gh-release--pad name width)
                               (my-consult-gh-release--size
                                (my-consult-gh-release--field asset 'size))
                               (or (my-consult-gh-release--field asset 'downloadCount) 0))))
            (insert (propertize line
                                :name name
                                :url url
                                :repo repo
                                :tag tag)))))
      (goto-char (or (text-property-not-all (point-min) (point-max) :name nil)
                     (point-min))))
    (pop-to-buffer buf)))

(defun my-consult-gh-release--choose ()
  "Ask for the asset picker or cancel.  Return `picker' or `cancel'."
  (consult--read
   '(("asset picker" . picker)
     ("cancel" . cancel))
   :prompt "Release assets: "
   :lookup #'consult--lookup-cdr
   :require-match t
   :sort nil))

;;;###autoload
(defun my-consult-gh-release-download (&optional repo tagname)
  "List the uploaded assets of a release and download one.
REPO is OWNER/REPO.  TAGNAME is the release tag.  With no
arguments, use the consult-gh release buffer, or ask."
  (interactive)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((repo (my-consult-gh-release--plain
                 (or repo
                     (and (stringp consult-gh--topic)
                          (get-text-property 0 :repo consult-gh--topic))
                     (get-text-property 0 :repo (consult-gh-search-repos nil t)))))
          (tagname (my-consult-gh-release--plain
                    (or tagname
                        (and (stringp consult-gh--topic)
                             (get-text-property 0 :tagname consult-gh--topic))
                        (get-text-property 0 :tagname (consult-gh-release-list repo t))))))
     (unless (and repo tagname)
       (user-error "No release selected"))
     (when (eq (my-consult-gh-release--choose) 'picker)
       (let ((assets (my-consult-gh-release--assets repo tagname)))
         (if assets
             (my-consult-gh-release--show-assets
              repo tagname assets (consult-gh--auth-account-host))
           (message "No downloadable assets")))))))

(provide 'my-consult-gh-release)

;;; my-consult-gh-release.el ends here
