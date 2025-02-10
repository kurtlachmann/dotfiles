;; My custom Emacs config

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(defvar kl/default-font-size 120)
(defvar kl/default-variable-font-size 120)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; Turn off some unneeded UI elements
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(menu-bar-mode -1)     ; Disable the menu bar
;(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)   ; Give some room to breathe

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Display the current column
(column-number-mode)

;; Display line numbers in every buffer
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use at least 3 columns for line numbers
(setq-default display-line-numbers-width 3)

;; Default width of tab characters
(setq-default tab-width 4)

;; Dired: don't show available space in the header
(setq dired-free-space nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; USABILITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remember recently opened files (M-x recentf-open-files)
(recentf-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable line-wrap by default
;; Can be turned on via `toggle-truncate-lines
(set-default 'truncate-lines t)

;; Place all auto-saves and backup files in the OS's temp dir (e.g. C:/Temp on Windows)
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; When searching, immediately move to another match on direction change
(setq isearch-repeat-on-direction-change t)

;; Immediately wrap around the end of the file without extra keypress and without warning ("ding")
(setq isearch-wrap-pause 'no-ding)

;; Auto close parentheses
(electric-pair-mode t)

;; Dired: which flags should be passed to the `ls` command
(setq dired-listing-switches "-Ahl --group-directories-first")

;; Dired: don't show hidden files by default
(require 'dired-x)
(setq dired-omit-files "^\\.")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Start scrolling before hitting the last visible line
(setq scroll-margin 5)

;; "Smooth" scrolling: scroll only 1 line instead of a full page
(setq scroll-step 1)
(setq scroll-conservatively 10000)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up package.el to work with MELPA
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-v") 'yank)
  (define-key evil-normal-state-map (kbd ",") 'evil-repeat-find-char)
  (define-key evil-normal-state-map (kbd ";") 'evil-repeat-find-char-reverse)
  (define-key evil-normal-state-map (kbd "<return>") '(lambda () (interactive) (insert-newline-below) (next-line)))
  (define-key evil-normal-state-map (kbd "S-<return>") 'insert-newline-above)
  (define-key evil-normal-state-map (kbd "<home>") 'evil-first-non-blank)

  ;; TODO configure key bindings
  ;; ??? 'lsp-ui-doc-show
  ;; ??? 'flymake-diagnostic-at-point-popup

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'image-dired-display-image-mode 'emacs)
  (setq evil-normal-state-cursor '(box "white"))
  (setq evil-insert-state-cursor '(bar "white"))
  (setq evil-visual-state-cursor '(box "orange"))
  (setq evil-replace-state-cursor '(hbar "red"))
)

;; Only let the cursor blink in insert mode
(blink-cursor-mode 0)
(add-hook 'evil-insert-state-entry-hook
                        (lambda () (blink-cursor-mode 1)))
(add-hook 'evil-insert-state-exit-hook
                        (lambda () (blink-cursor-mode 0)))

;; which-key: show available shortcuts in the minibuffer
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Completion engine
(use-package vertico
  :init
  (vertico-mode t))

;; Add annotations next to search results
(use-package marginalia
  :init
  (marginalia-mode t))

;; Improve dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Autocompletion
(use-package company)

;; Add language server client
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook (
    (python-mode . lsp)
    (rust-mode . lsp))
  :config
  (lsp-enable-which-key-integration t)
  :commands lsp)

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'at-point)
  :commands lsp-ui-mode)

;; Python
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package flymake-ruff
  :hook (python-mode . flymake-ruff-load))

;; Rust
(use-package rust-mode)

;; Webdev
;; tsx-mode
(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))
;(use-package coverlay)
;(use-package css-in-js-mode :straight '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))
;(use-package origami)

;; Git
(use-package magit)

(use-package popup)

(use-package dap-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; THEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default font
(set-face-attribute 'default nil :font "Fira Code" :height kl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height kl/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height kl/default-variable-font-size :weight 'regular)

;; Color theme
(use-package dracula-theme
  :init)
;;  (load-theme 'dracula))

(use-package ayu-theme
  :init
  (load-theme 'ayu-dark))

;; Fix some font colors
(setq face-remapping-alist
  '(
    (diredfl-dir-heading . font-lock-comment-face)
    (diredfl-no-priv . font-lock-comment-face)
    (diredfl-dir-priv . font-lock-keyword-face)
    (diredfl-read-priv . font-lock-builtin-face)
    (diredfl-write-priv . font-lock-builtin-face)
    (diredfl-exec-priv . font-lock-function-name-face)
    (diredfl-number . font-lock-number-face)
    (diredfl-date-time . font-lock-comment-face)
    (diredfl-dir-name . diredfl-dir-priv)
    (diredfl-file-name . default)
    (diredfl-file-suffix . default)
	(diredfl-link-priv bold font-lock-string-face)
	(diredfl-symlink . diredfl-link-priv)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUSTOM FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymake-diagnostic-at-point-popup ()
  "Display the flymake diagnostic inside a popup."
  (interactive)
  (popup-tip (concat "" (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))))

(defun scroll-down-chunk ()
  "Scroll down by a couple of lines and move the cursor the same distance."
  (interactive)
  (next-line 8)
  (scroll-up 8))

(defun scroll-up-chunk ()
  "Scroll up a couple of lines and move the cursor the same distance."
  (interactive)
  (previous-line 8)
  (scroll-down 8))

(defun insert-newline-above ()
  "Insert a newline above the current cursor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)))

(defun insert-newline-below ()
  "Insert a newline below the current cursor."
  (interactive)
  (save-excursion
	(end-of-line)
	(newline)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save buffer with Ctrl-s
(global-set-key (kbd "C-s") 'save-buffer)

;; Ctrl-f search buffer
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
;; (Remove evil's default C-f keybinding)
(eval-after-load "evil-maps"
  (define-key evil-motion-state-map "\C-f" nil))

;; Dired
;; Ctrl-up    Go to parent directory
;; Ctrl-left  Go to previous directory
(define-key dired-mode-map (kbd "C-<up>") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-<left>") 'quit-window)
(define-key dired-mode-map (kbd "M-RET") 'dired-display-file)

(global-set-key (kbd "C-<up>") 'scroll-up-chunk)
(global-set-key (kbd "C-<down>") 'scroll-down-chunk)
