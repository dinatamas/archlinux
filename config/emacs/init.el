;; Package manager setup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Packages to install automatically.
(setq package-list '(
  centered-cursor-mode
  dash-functional
  direx
  multiple-cursors
  nord-theme
  xclip))
;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))
;; Install missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; External include file for generated config.
(setq custom-file "/archlinux/config/emacs/custom-file.el")
(load-file custom-file)

;; Nicer modeline and minibuffer.
(load "/archlinux/config/emacs/mini-modeline.el")
(mini-modeline-mode t)
(setq mini-modeline-r-format '(:eval (concat
  "%0l : %0c : "
  (format "%d" (/ (- (line-number-at-pos) 1) 0.01
                  (max (count-lines (point-min) (point-max)) 1)))
  "%%")))
(defun mini-modeline--set-buffer-face () ())

;; Custom hi-lock related configuration (interactive highlighting).
(load "/archlinux/config/emacs/highlight.el")

;; Load repetition error finder.
(load "/archlinux/config/emacs/highlight-repeats.el")

;; No startup/title screen.
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)

;; No Messages buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Load Nord theme.
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.config/emacs.d/themes/"))
(setq nord-region-highlight "snowstorm")
(load-theme 'nord t)
;; All the nice colors (256 Xterm):
; https://jonasjacek.github.io/colors/

;; Transform temporary buffers to temporary windows.
;(require 'popwin)
;(popwin-mode 1)
;(window-divider-mode)
;; TODO: Popwin should be configured properly.
;; TODO: Window dividers should be configured properly.

;; Load directory tree viewer.
(require 'direx)
(global-set-key (kbd "C-x j") 'direx:jump-to-directory)
(global-set-key (kbd "C-x p") 'direx-project:jump-to-project-root)
(global-set-key (kbd "C-x f") 'direx:find-directory)

;; Additional file creation utility for direx.
(defun direx:create-file ()
  (interactive)
  (let* ((item (direx:item-at-point!))
         (file (direx:item-tree item))
         (parent-dir
          (if (cl-typep file 'direx:directory)
              (direx:file-full-name file)
            (direx:directory-dirname
             (direx:file-full-name file))))
         (newfile (read-directory-name "Create file: " parent-dir)))
    (when (file-exists-p newfile)
      (error "Can't create %s: file exists" newfile))
    (if (not (file-exists-p (file-name-directory newfile)))
        (make-directory (file-name-directory newfile) t))
    (write-region "" nil newfile)
    (direx:refresh-whole-tree)))
(define-key direx:direx-mode-map (kbd "F") 'direx:create-file)

;; Keep cursor centered.
(require 'centered-cursor-mode)
(global-centered-cursor-mode 1)

;; Simplify the user interface.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set buffer width to 80 and center the text.
;; Calculate window width.
;(defun nice-window-width (window)
;  (let ((margins (window-margins window)))
;    (+ (window-width window) (or (car margins) 0) (or (cdr margins) 0))))
;(defun nice-window-change ()
;  (dolist (window (window-list))
;    (unless (window-minibuffer-p window)
;      (let* ((target (with-selected-window window 84))
;             (margin (and target (max (floor (/ (- (nice-window-width window)
;                                            target) 2)) 0))))
;        (set-window-margins window margin margin)))))
;(defun clear-nice-window-margins ()
;  (dolist (window (window-list))
;    (set-window-margins window 0 0)))
;(define-advice split-window (:before (&rest _))
;  (set-window-margins (selected-window) 0 0))
;(add-hook 'window-configuration-change-hook 'nice-window-change)
;(add-hook 'window-size-change-functions 'nice-window-change)
;(add-hook 'window-buffer-change-functions 'nice-window-change)
;(add-hook 'window-setup-hook 'nice-window-change)

;(defun my-resize-margins ()
;  (if (> (window-width (window-main-window)) 85)
;    (let ((margin-size (floor (/ (- (window-width (window-main-window)) 85) 2))))
;      (set-window-margins (selected-window) margin-size margin-size))
;    (set-window-margins (selected-window) 0 0)))

(defun ideal-margin-size ()
  (max (floor (/ (- (frame-width) 82) 2)) 0))
(defun my-resize-margins ()
  (interactive)
  (set-window-margins (frame-root-window)
                      (ideal-margin-size) (ideal-margin-size)))
(add-hook 'window-configuration-change-hook 'my-resize-margins)
(add-hook 'window-size-change-functions 'my-resize-margins)
(add-hook 'window-buffer-change-functions 'my-resize-margins)
(add-hook 'window-setup-hook 'my-resize-margins)
(my-resize-margins)

;; Move all backups to separate folder.
(setq backup-dir "~/.local/share/emacs-backups/")
(when (not (file-directory-p backup-dir))
  (make-directory backup-dir t))
(setq backup-directory-alist `(("." . ,backup-dir))
      auto-save-file-name-transforms `((".*" ,backup-dir t))
      auto-save-list-file-prefix (concat backup-dir ".saves-"))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)

;; Kill buffers with simple key combination.
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Always ask when exiting emacs. Prefer to kill individual buffers.
(setq confirm-kill-emacs 'y-or-n-p)
;; Enable using 'y' or 'n' for prompts in general.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight current line.
;; Disabled because of personal preference.
;(global-hl-line-mode t)

;; Disable the Insert button (overwrite mode).
(global-set-key (kbd "<insertchar>") nil)

;; Use system clipboard.
(require 'xclip)
(xclip-mode 1)
(delete-selection-mode t)

;; Enable basic in-terminal mouse support.
(xterm-mouse-mode 1)

;; Multiple cursors.
(require 'multiple-cursors)
;; Set multiple fake cursors then enter multicursor mode.
(defun mc/toggle-cursor-at-point ()
  (interactive)
  (if multiple-cursors-mode
      (message (concat "Cannot toggle cursor at point "
                       "while `multiple-cursors-mode' is active."))
    (let ((existing (mc/fake-cursor-at-point)))
      (if existing
          (mc/remove-fake-cursor existing)
        (mc/create-fake-cursor-at-point)))))
(add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
(add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)
(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m c") 'mc/toggle-cursor-at-point)
(global-set-key (kbd "C-x m RET") 'multiple-cursors-mode)
(global-set-key (kbd "C-x m r") 'mc/edit-lines)
;; So that hitting enter doesn't quit multicursor mode,
;; just inserts a newline character.
(define-key mc/keymap (kbd "RET") nil)

;; Ignore case when searching by default.
(setq case-fold-search t)
(setq case-replace t)

;; Highlight mismatching parentheses.
;; Disabled due to performance reasons.
;(require 'paren)
;(setq show-paren-style 'parenthesis)
;(show-paren-mode +1)

;; Quick utility to get current line length.
(defun curr-line-len ()
  (- (line-end-position)
     (line-beginning-position)))

;; Disable auto here mode completion.
(add-hook 'sh-mode-hook
  (lambda ()
    (sh-electric-here-document-mode -1)))

;; Disable smart quotes.
(defun never-smart-quote ()
  (local-unset-key "\""))
(add-hook 'latex-mode-hook 'never-smart-quote)

;; Scroll half a page (screen).
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))
(defun scroll-up-half () (interactive)
  (forward-line (window-half-height)))
(global-set-key (kbd "C-U") 'scroll-up-half)
(defun scroll-down-half () (interactive)
  (forward-line (* (window-half-height) -1)))
(global-set-key (kbd "C-D") 'scroll-down-half)

;; Disable TAB indentation.
(setq-default indent-tabs-mode nil)

;; Use a better word skip function.
;; This skips words but stops around symbols.
(defun improved-forward (&optional arg)
  (interactive "^p")
  (if (> arg 0)
      (dotimes (_ arg)
        (when (looking-at-p "[ \t]")
          (skip-chars-forward " \t"))
        (unless (= (point) (point-max))
          (forward-same-syntax)))
    (dotimes (_ (- arg))
      (when (looking-back "[ \t]")
        (skip-chars-backward " \t"))
      (unless (= (point) (point-min))
        (forward-same-syntax -1)))))
(defun improved-backward (&optional arg)
  (interactive "^p")
  (improved-forward (- arg)))
(global-set-key (kbd "M-f") 'improved-forward)
(global-set-key (kbd "M-b") 'improved-backward)

;; Auto-compile using xelatex.
(defun call-xelatex ()
  (interactive)
  (save-window-excursion
    (async-shell-command (concat "xelatex " (buffer-file-name)))))
(add-hook 'latex-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'call-xelatex)))
