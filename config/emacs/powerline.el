;; Base powerline plugin.
(require 'powerline)

;; Define faces for nicer colors.
(defface powerline-nord0
  '((t :background "blue"
       :foreground "black"
       )) "")
(defface powerline-nord1
  '((t :background "cyan"
       :foreground "black"
       )) "")
(defface powerline-nord2
  '((t :background "black"
       :foreground "white"
       )) "")
(defface powerline-nord3
  '((t :background "yellow"
       :foreground "black"
       )) "")
(defface powerline-nord4
  '((t :background "brightblack"
       :foreground "white"
       )) "")

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
          'face 'powerline-nord3)))))

;; Custom Powerline theme.
;; Don't differentiate active and inactive bars.
;; Use custom segments and faces.
(defun powerline-custom-theme () ""
  (interactive)
  (setq-default mode-line-format
    '("%e"
      (:eval
        (let* ((face0 'powerline-nord0)
               (face1 'powerline-nord1)
               (face2 'powerline-nord2)
               (face3 'powerline-nord3)
               (face4 'powerline-nord4)
               (separator-left (intern (format "powerline-%s-%s"
                               (powerline-current-separator)
                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                               (powerline-current-separator)
                               (cdr powerline-default-separator-dir))))
               (lhs (list (custom-buffer-id face0)
                          (powerline-raw " " face0)
                          (funcall separator-left face0 face2)
                          (funcall separator-left face2 face1)
                          (powerline-major-mode face1 'l)
                          (powerline-raw " " face1)
                          (funcall separator-left face1 face2)
                          (if vc-mode (funcall separator-left face2 face3))
                          (if vc-mode (powerline-vc face3))
                          (if vc-mode (powerline-raw " " face3))
                          (if vc-mode (funcall separator-left face3 face2))))
               (rhs (list (funcall separator-right face2 face4)
                          (powerline-raw (char-to-string #xe0a1) face4 'l)
                          (powerline-raw " %0l" face4)
                          (powerline-raw ":" face4)
                          (powerline-raw "%0c " face4)
                          (funcall separator-right face4 face2)
                          (funcall separator-right face2 face1)
                          (powerline-raw " " face1)
                          (powerline-raw
                           '(:eval (format "%d"
                             (/ (- (line-number-at-pos) 1) 0.01
                                (count-lines (point-min) (point-max)))))
                           face1)
                          (powerline-raw "%%" face1)
                          (powerline-raw " " face1))))
               (concat (powerline-render lhs)
                       (powerline-fill face2 (powerline-width rhs))
                       (powerline-render rhs)))))))

;; Apply theme.
(powerline-custom-theme)
