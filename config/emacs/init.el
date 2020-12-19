;; Package manager setup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; External include file for generated config.
(setq custom-file "/archlinux/config/emacs/custom-file.el")
(load-file custom-file)

;; Powerline as modeline with custom modifications.
(load "/archlinux/config/emacs/powerline.el")

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
(global-hl-line-mode t)

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
(show-paren-mode t)

;; Enable using 'y' or 'n' for prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable permanent highlights.
(global-hi-lock-mode t)

;; Define new highlight faces.
(defface nordhl0
  '((t :foreground "white"
       :background "cyan")) "")
(defface nordhl1
  '((t :foreground "white"
       :background "blue")) "")
(defface nordhl2
  '((t :foreground "white"
       :background "green")) "")
(defface nordhl3
  '((t :foreground "white"
       :background "magenta")) "")
(defface nordhl4
  '((t :foreground "white"
       :background "red")) "")

; Set faces for highlighting.
(setq hi-lock-face-defaults '(
  "nordhl0" "nordhl1" "nordhl2"
  "nordhl3" "nordhl4"))
; Do not prompt for selecting faces.
(setq hi-lock-auto-select-face t)

;; Highlight FIXME and TODO with red.
(defface redhl
  '((t :foreground "red")) "")
(add-hook 'hi-lock-mode-hook
  (lambda nil
    (highlight-regexp "FIXME" 'redhl)
    (highlight-regexp "TODO" 'redhl)
    ; Skip the redhl face when highlighting.
    (hi-lock-read-face-name)
t))

;; Make line hl-line work nicely with hi-lock.
; Set low priority for the hl-line overlay.
(defadvice hl-line-highlight (after set-priority activate)
  (unless (window-minibuffer-p)
    (overlay-put hl-line-overlay 'priority -50)))
; Configure hl-lock to use overlays (by disabling font lock).
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-mode nil))
    ad-do-it))

;; Disable auto here mode completion.
(add-hook 'sh-mode-hook
  (lambda ()
    (sh-electric-here-document-mode -1)))
