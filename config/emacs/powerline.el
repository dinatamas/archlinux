;; Base powerline plugin.
(require 'powerline)

;; Define faces for nicer colors.
(defface pl-nord0
  '((t :background "blue"
       :foreground "black")) "")
(defface pl-nord1
  '((t :background "cyan"
       :foreground "black")) "")
(defface pl-nord2
  '((t :background "black"
       :foreground "white")) "")
(defface pl-nord3
  '((t :background "yellow"
       :foreground "black")) "")
(defface pl-nord4
  '((t :background "brightblack"
       :foreground "white")) "")

;; Buffer id shouldn't be padded to at least 12 chars.
(defun custom-buffer-id (&optional face pad)
  (powerline-raw
   (format-mode-line
    (concat " " (propertize
                 (format-mode-line "%0b")
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
                 'local-map (let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
                              (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
                              map))))
   face pad))

;; Remove the version control symbol.
(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if (and window-system (not powerline-gui-use-vcs-glyph))
        (format-mode-line '(vc-mode vc-mode))
      (format " %s"
        (format-mode-line '(vc-mode vc-mode))))))

;; Use black font instead of yellow for version control.
(eval-after-load "vc-hooks"
  '(defadvice vc-mode-line (after custom-git-advice () activate) ""
    (when (stringp vc-mode)
      (setq vc-mode
        (propertize
          (replace-regexp-in-string "^." "" vc-mode)
          'face 'pl-nord3)))))

;; Custom Powerline theme.
;; Don't differentiate active and inactive bars.
;; Use custom segments and faces.
(let* ((separator-left (intern (format "powerline-%s-%s"
                       (powerline-current-separator)
                       (car powerline-default-separator-dir))))
       (separator-right (intern (format "powerline-%s-%s"
                       (powerline-current-separator)
                       (cdr powerline-default-separator-dir))))
       (lhs (list (custom-buffer-id 'pl-nord0)
                  (powerline-raw " " 'pl-nord0)
                  (funcall separator-left 'pl-nord0 'pl-nord2)
                  (funcall separator-left 'pl-nord2 'pl-nord1)
                  (powerline-major-mode 'pl-nord1 'l)
                  (powerline-raw " " 'pl-nord1)
                  (funcall separator-left 'pl-nord1 'pl-nord2)
                  (if vc-mode (funcall separator-left 'pl-nord2 'pl-nord3))
                  (if vc-mode (powerline-vc 'pl-nord3))
                  (if vc-mode (powerline-raw " " 'pl-nord3))
                  (if vc-mode (funcall separator-left 'pl-nord3 'pl-nord2))))
       (rhs (list (funcall separator-right 'pl-nord2 'pl-nord4)
                  (powerline-raw (char-to-string #xe0a1) 'pl-nord4 'l)
                  (powerline-raw " %0l" 'pl-nord4)
                  (powerline-raw ":" 'pl-nord4)
                  (powerline-raw "%0c " 'pl-nord4)
                  (funcall separator-right 'pl-nord4 'pl-nord2)
                  (funcall separator-right 'pl-nord2 'pl-nord1)
                  (powerline-raw " " 'pl-nord1)
                  (powerline-raw
                   '(:eval (format "%d"
                     (/ (- (line-number-at-pos) 1) 0.01
                        (count-lines (point-min) (point-max)))))
                   'pl-nord1)
                  (powerline-raw "%%" 'pl-nord1)
                  (powerline-raw " " 'pl-nord1))))
       (setq modeline-powerline-lhs (powerline-render lhs))
       (setq modeline-powerline-rhs (powerline-render rhs)))
