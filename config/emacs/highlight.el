(require 'hi-lock)

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

;; Make line hl-line work nicely with hi-lock.
; Set low priority for the hl-line overlay.
(defadvice hl-line-highlight (after set-priority activate)
  (unless (window-minibuffer-p)
    (overlay-put hl-line-overlay 'priority -50)))
; Configure hl-lock to use overlays (by disabling font lock).
; This also means that highlights won't auto-update by default.
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-mode nil))
    ad-do-it))

;; My own highlighter.
(defun my-highlight (regexp face &optional skip_curr)
  (unless (window-minibuffer-p)
  (let* ((no-matches t)
         (range-min (- (point) (/ 2000000 2)))
         (range-max (+ (point) (/ 2000000 2)))
         (current-line (line-number-at-pos))
         (current-col  (current-column))
         (search-start
           (max (point-min)
             (- range-min (max 0 (- range-max (point-max))))))
         (search-end
           (min (point-max)
             (+ range-max (max 0 (- (point-min) range-min))))))
         (save-excursion
           (goto-char search-start)
           (while (re-search-forward regexp search-end t)
             (when no-matches (setq no-matches nil))
               (unless (and skip_curr (and
               (eq current-line (line-number-at-pos))
               (eq current-col  (curr-line-len))))
               (let ((overlay (make-overlay (match-beginning 0)
                                            (match-end 0))))
               (overlay-put overlay 'mine t)
               (overlay-put overlay 'face face))
               )
             (goto-char (match-end 0)))))))

;; Custom permanent highlights.
(defface redfg
  '((t :foreground "red")) "")
(defface redbg
  '((t :background "red")) "")
(defun custom-highlights ()
  (remove-overlays nil nil 'mine t)
  (my-highlight "FIXME" 'redfg)
  (my-highlight "TODO" 'redfg)
  (my-highlight "\t" 'redbg)
  (my-highlight "[ ]+$" 'redbg t)
  t)
(add-hook 'find-file-hook 'custom-highlights)
(add-hook 'post-command-hook 'custom-highlights)
