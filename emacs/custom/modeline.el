;; My custom modeline

(custom-set-faces
 '(mode-line ((t (:background "unspecified" :overline "#6272a4" :box (:line-width 12 :color "#282a36")))))
 '(mode-line-inactive ((t (:background "unspecified" :overline "#6272a4" :box (:line-width 12 :color "#282a36"))))))

;; TODO change this to setq-default
(setq mode-line-format
	  '((:eval
		 (my-mode-line-render
		  ;; Left
		  '("%e"
			my-modeline-buffer-name
     		my-modeline-buffer-modified
	    	"  "
		    my-modeline-major-mode
   			"  "
			my-modeline-cursor-pos
			"  "
			my-modeline-region-length)
		  ;; Right
		  '(my-modeline-git-branch)))))

(defvar-local my-modeline-buffer-modified
	'(:eval
	  (if (buffer-modified-p)
		  (propertize "*" 'face 'my-modeline-buffer-modified-face)
		" ")))

(defvar-local my-modeline-buffer-name
	'(:eval
	  (propertize (buffer-name) 'face 'bold))
  "Mode line construct for displaying the buffer name.")

(defvar-local my-modeline-major-mode
	'(:eval
	  (propertize (symbol-name major-mode) 'face 'my-modeline-major-mode-face))
  "Mode line construct for displaying the major mode.")

(defvar-local my-modeline-cursor-pos
	'(:eval
	  (format "%s:%s" (line-number-at-pos) (current-column))))

(defvar-local my-modeline-git-branch
	'(:eval
	  ;; TODO
	  ""))
	  ;;(shell-command-to-string "git rev-parse --abbrev-ref HEAD")))

(defvar-local my-modeline-region-length
	'(:eval
	  ;; TODO
	  ;; Point and mark seem to update only when exiting the region
	  (format "%ss" (window-point))))
	  ;;(format "%s" (length (buffer-substring-no-properties (region-beginning) (region-end))))))
	  ;;(format "%s" (abs (- (mark) (point))))))

(dolist (construct '(my-modeline-buffer-modified
					 my-modeline-buffer-name
					 my-modeline-major-mode
					 my-modeline-cursor-pos
					 my-modeline-region-length
					 my-modeline-git-branch))
  (put construct 'risky-local-variable t))

(defface my-modeline-major-mode-face
  '((t :inherit
	   font-lock-keyword-face
	   :background "unspecified"
	   :weight bold))
  "Face for the major mode.")

(defface my-modeline-buffer-modified-face
  '((t :inherit
	   warning
	   :weight bold))
  "Face to indicate that the buffer is modified.")
  
(defun my-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))
