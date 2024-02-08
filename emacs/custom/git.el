;; A very basic interface to git
;; Magit is nice but too slow on Windows since it calls git too often.
;; This mode is intentionally kept simple and close to git's CLI interface to
;; minimize the spawned `git` processes.


;; TODO move this into init.el
;; Disable evil-mode when entering git-status
(evil-set-initial-state 'git-status-mode 'emacs)


(setq git-status--current-file 0
	  git-status--data nil)

(defun git-status ()
  "Open git status"
  (interactive)
  (switch-to-buffer "*git-status*")
  (git--cd-to-current-file)
  (git-status-mode)
  (git-status--refresh-buffer))

(define-derived-mode git-status-mode special-mode "git status"
  (define-key git-status-mode-map (kbd "q") 'quit-window)
  (define-key git-status-mode-map (kbd "<f5>") 'git-status--refresh-buffer)
  (define-key git-status-mode-map (kbd "<down>") 'git-status-next-line)
  (define-key git-status-mode-map (kbd "<up>") 'git-status-prev-line)
  (define-key git-status-mode-map (kbd "<left>") 'git-status-left)
  (define-key git-status-mode-map (kbd "<right>") 'git-status-right)
  (define-key git-status-mode-map (kbd "a") 'git-status-add-file-at-point)
  (define-key git-status-mode-map (kbd "r") 'git-status-unstage-file-at-point))

(defun git-status-left ()
  "Prevent moving point to the previous line by going left"
  (interactive)
  (unless (bolp)
	(left-char)))

(defun git-status-right ()
  "Prevent moving point to the next line by going right"
  (interactive)
  (unless (eolp)
	(right-char)))

(defun git-status--sync-point ()
  "Moves point to the file that the index points to"
  (let ((old-index git-status--current-file))
	(goto-char (point-min))
	(re-search-forward "^  .  ")
	(setq git-status--current-file 0)
	(dotimes (number old-index)
	  (git-status-next-line))))

(defun git-status-next-line ()
  "Moves point to the next line that contains a selectable file"
  (interactive)
  (let ((max-index (- (length git-status--data) 1)))
    (when (< git-status--current-file max-index)
      (next-line)
	  (beginning-of-line)
	  (while (not (looking-at "  .  "))
		(next-line)
		(beginning-of-line))
      (move-to-column 5)
      (setq git-status--current-file (+ git-status--current-file 1)))))

(defun git-status-prev-line ()
  "Moves point to the previous line that contains a selectable file"
  (interactive)
  (when (> git-status--current-file 0)
    (previous-line)
	(beginning-of-line)
	(while (not (looking-at "  .  "))
	  (previous-line)
	  (beginning-of-line))
    (move-to-column 5)
    (setq git-status--current-file (- git-status--current-file 1))))

(defun git-status-add-file-at-point ()
  "Stages the file at point"
  (interactive)
  (let ((file-info-at-point (nth git-status--current-file git-status--data)))
	(let ((filename (nth 2 file-info-at-point))
		 (area (nth 0 file-info-at-point)))
	 (when (string= area "unstaged")
       (shell-command (format "git add \"%s\"" filename))
	   (git-status--refresh-buffer)
	   (git-status-next-line)))))

(defun git-status-unstage-file-at-point ()
  "Removes the file at point from the index"
  (interactive)
  (let* ((file-info-at-point (nth git-status--current-file git-status--data))
		 (filename (nth 2 file-info-at-point))
		 (area (nth 0 file-info-at-point)))
	(message (format "%s" git-status--data))
	(when (string= area "staged")
      (shell-command (format "git restore --staged \"%s\"" filename))
	  (git-status--refresh-buffer))))

(defun git-status-revert-file-at-point ()
  "TODO"
  (interactive)
  (let* ((file-info (nth git-status--current-file git-status--data))
		 (area (nth 0 file-info))
		 (path (nth 2 file-info)))
	(if (not(yes-or-no-p (format "Revert \"%s\"? " path)))
		(message "Revert aborted")
	  (when (string= area "unstaged")
		(shell-command (format "git restore \"%s\"" path))
		(git-status--refresh-buffer)))))

(defun git-status--refresh-buffer ()
  (interactive)
  (let ((inhibit-read-only t)
		(file-infos (git-status--parse (git-status--exec))))
	(setq git-status--data (sort file-infos 'git-status--compare-fileinfos))
	(message (format "%s" git-status--data))
	(erase-buffer)
	(insert "STAGED\n\n")
	(dolist (file-info git-status--data)
	  (pcase file-info
		(`("staged" ,X ,filename) (git-status--insert-file filename X))))
	(insert "\n\nCHANGES\n\n")
	(dolist (file-info git-status--data)
	  (pcase file-info
		(`("unstaged" ,Y ,filename) (git-status--insert-file filename Y)))))
  (git-status--sync-point))

(defun git-status--parse (status)
  (let ((lines (split-string (string-trim status) "\n"))
		(output nil))
	(dolist (line lines)
	  (let ((line-type (substring line 0 1)))
		(cond
		 ((string= line-type "?")
		  (push (git-status--parse-untracked line) output))
		 ((string= line-type "1")
		  (setq output (append (git-status--parse-modified line) output)))
		 ((string= line-type "2")
		  (setq output (append (git-status--parse-renamed line) output)))
		 ((string= line-type "u")
		  (push (git-status--parse-unmerged line) output)))))
	(reverse output)))

(defun git-status--parse-untracked (line)
  (list "unstaged" "?" (substring line 2)))

(defun git-status--parse-modified (line)
  (let ((X (substring line 2 3))
		(Y (substring line 3 4))
		(filename (string-join (nthcdr 8 (split-string line " ")) " "))
		(out nil))
	(when (not (string= X "."))
	  (push (list "staged" X filename) out))
	(when (not (string= Y "."))
	  (push (list "unstaged" Y filename) out))
	out))

(defun git-status--parse-renamed (line)
  "Parses a line indicating a file rename or copy (lines that start with a '2')"
  (let ((X (substring line 2 3))
		(Y (substring line 3 4))
		(filename (string-join (nthcdr 9 (split-string (nth 0 (string-split line "\t")) " ")) " "))
		(out nil))
	(when (not (string= X "."))
	  (push (list "staged" X filename) out))
	(when (not (string= Y "."))
	  (push (list "unstaged" Y filename) out))
	;; TODO separate "from" and "to" filenames in the returned list
	out))

(defun git-status--parse-unmerged (line)
  "Parses a line indicating an unmerged file (lines that start wihth a 'u')"
  (let ((filename (string-join (nthcdr 10 (string-split line " ")))))
	(list "unmerged" filename)))

(defun git-status--sort-fileinfos (file-infos)
	(sort file-infos (lambda (a b) (string< (nth 0 a) (nth 0 b)))))

(defun git-status--compare-fileinfos (a b)
  "Compares two file infos. Can be used to sort file infos."
  (let ((a-area (nth 0 a))
		(a-path (nth 2 a))
		(b-area (nth 0 b))
		(b-path (nth 2 b)))
	(if (string= a-area b-area)
		(string< a-path b-path)
	  (string< a-area b-area))))

(defun git-status--exec ()
  "Executes `git status` and returns the results as a string."
  (shell-command-to-string "git status --porcelain=v2 --branch --show-stash"))

(defun git--cd-to-current-file ()
  "Changes the working dir to the currently openend file"
  (cd "/home/kurt/code/python-sandbox")) ; TODO remove this
  ;(cd (file-name-directory buffer-file-name))

(defun git-status--insert-file (filename status)
  (let ((filename (file-name-nondirectory filename))
		(dir (file-name-directory filename)))
	(insert "  ")
	(git-status--insert-status-code status)
	(insert (format "  %s" filename))
	(when dir
	  (insert " ")
	  (git--insert-colored-string dir 'git-status-directory-face))
	(insert "\n")))

(defun git-status--insert-status-code (status)
  (let ((face (pcase status
				("A" 'git-status-added-face)
				("M" 'git-status-modified-face)
				("D" 'git-status-deleted-face)
				("R" 'git-status-renamed-face)
				("?" 'git-status-untracked-face)
				(_ 'default))))
	(git--insert-colored-string status face)))

(defun git--insert-colored-string (str target-face)
  (put-text-property 0 (length str) 'face target-face str)
  (insert str))

(defface git-status-added-face
  '((t :inherit
	   ansi-color-green
	   :background "unspecified"
	   :weight bold))
  "Face for added files"
  :group 'git-status-mode)

(defface git-status-modified-face
  '((t :inherit
	   ansi-color-cyan
	   :background "unspecified"
	   :weight bold))
  "Face for modified files"
  :group 'git-status-mode)

(defface git-status-deleted-face
  '((t :inherit
	   ansi-color-red
	   :background "unspecified"
	   :weight bold))
  "Face for deleted files"
  :group 'git-status-mode)

(defface git-status-renamed-face
  '((t :inherit
	   ansi-color-yellow
	   :background "unspecified"
	   :weight bold))
  "Face for renamed files"
  :group 'git-status-mode)

(defface git-status-untracked-face
  '((t :inherit
	   font-lock-comment-face
	   :background "unspecified"
	   :weight bold))
  "Face for untracked files"
  :group 'git-status-mode)

(defface git-status-directory-face
  '((t :inherit
	   font-lock-comment-face
	   :background "unspecified"
	   :weight bold))
  "Face for a file's directory"
  :group 'git-status-mode)
