;; Package manager setup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; External include file for generated config.
(setq custom-file "/archlinux/config/emacs/custom-file.el")
(load-file custom-file)

;; Powerline as modeline with custom modifications.
(load "/archlinux/config/emacs/powerline.el")

;; Custom hi-lock related configuration (interactive highlighting).
(require 'hi-lock)
;(load "/archlinux/config/emacs/my-hi-lock.el")
(load "/archlinux/config/emacs/highlight.el")

;; Set window width to 80 and center the buffer.
(defun my-resize-margins ()
  (if (> (frame-width) 81)
    (let ((margin-size (/ (- (frame-width) 81) 2)))
      (set-window-margins nil margin-size margin-size))))
(add-hook 'window-configuration-change-hook #'my-resize-margins)
(my-resize-margins)

;; Load Nord theme.
(add-to-list 'custom-theme-load-path (expand-file-name "~/.config/emacs.d/themes/"))
(setq nord-region-highlight "snowstorm")
(load-theme 'nord t)

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

;; Disable menu bar.
(menu-bar-mode -1)

;; Highlight current line.
;; Disabled because of personal preference.
;(global-hl-line-mode t)

;; Use system clipboard.
(require 'xclip)
(xclip-mode 1)
(delete-selection-mode t)

;; Keep cursor centered.
(require 'centered-cursor-mode)
(global-centered-cursor-mode 1)

;; Enable basic in-terminal mouse support.
(xterm-mouse-mode 1)

;; Show matching parentheses.
;; Disabled because it is too slow.
;(show-paren-mode t)

;; Enable using 'y' or 'n' for prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Define hook for line changes.
;(defvar current-line-number (line-number-at-pos))
;(defvar changed-line-hook nil)
;(defun update-line-number ()
;  (let ((new-line-number (line-number-at-pos)))
;    (when (not (equal new-line-number current-line-number))
;      (setq current-line-number new-line-number)
;      (run-hooks 'changed-line-hook))))
;(add-hook 'post-command-hook #'update-line-number)

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
(defun scroll-up-half ()
  (scroll-up (window-half-height)))
(defun scroll-down-half ()
  (scroll-down (window-half-height)))

;; Disable TAB indentation.
(setq-default indent-tabs-mode nil)
