;; Package maanger setup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; External include file for generated config.
(setq custom-file "/archlinux/config/emacs.d/custom-file.el")
(load-file custom-file)

;; Load custom modeline.
(require 'telephone-line)
(telephone-line-mode 1)

;; Load Nord theme.
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)

;; Disable menu bar.
(menu-bar-mode -1)

;; Highlight current line.
; (global-hl-line-mode t)

;; Use system clipboard.
(require 'xclip)
(xclip-mode 1)
(delete-selection-mode t)
