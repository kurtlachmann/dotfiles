;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(defvar kl/default-font-size 120)
(defvar kl/default-variable-font-size 120)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Don't show the splash screen
(setq inhibit-startup-message t)


;; Turn off some unneeded UI elements
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(menu-bar-mode -1)     ; Disable the menu bar
;(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)   ; Give some room to breathe

;; Display the current column
(global-display-line-numbers-mode t)

;; Display line numbers in every buffer
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use at least 3 columns for line numbers
(setq display-line-number-width 3)

;; Save what you enter into minibuffer promps
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Color theme
(use-package dracula-theme)
(load-theme 'dracula)

;; Default font
(set-face-attribute 'default nil :font "Fira Code" :height kl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height kl/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height kl/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Keybindings
(global-set-key (kbd "C-s") 'save-buffer)

;; Vim keybindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; TODO configure key bindings
  ;; ??? 'lsp-ui-doc-show
  ;; ??? 'flymake-diagnostic-at-point-popup

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
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

(use-package ivy
  :diminish
  :bind (("C-f" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; Improve dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired
  :ensure nil
  :commands dired-jump
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        ;image-dired-dir (concat doom-cache-dir "image-dired/")
        ;image-dired-db-file (concat image-dired-dir "db.el")
        ;image-dired-gallery-dir (concat image-dired-dir "gallery/")
        ;image-dired-temp-image-file (concat image-dired-dir "temp-image")
        ;image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " "))

    (add-hook 'dired-mode-hook
      (defun +dired-disable-gnu-ls-flags-maybe-h ()
        "Remove extraneous switches from `dired-actual-switches' when it's
uncertain that they are supported (e.g. over TRAMP or on Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
        (when (or (file-remote-p default-directory)
                  (and (boundp 'ls-lisp-use-insert-directory-program)
                       (not ls-lisp-use-insert-directory-program)))
          (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))

;; Improve sorting for ivy results
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

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

;; Git
(use-package magit)

(use-package popup)
(defun flymake-diagnostic-at-point-popup ()
  "Display the flymake diagnostic inside a popup."
  (interactive)
  (popup-tip (concat "" (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))))
