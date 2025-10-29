;;; logview-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'evil)

(defvar-local logview-mode-map
  (make-sparse-keymap))

;; Fontlock:
;; List of '(regex (groupnum "face")+)
(rx-let (

         )

  (defconst logview-font-lock-keywords
    '(
      ("^\\([A-Z]+\\)\s+:\s+\\(.+?\\)$" (2 'font-lock-type-face))
      ("^\\(DEBUG\\)\s+:\s+\\(.+?\\)$"   (1 'font-lock-comment-face t))
      ("^\\(INFO\\)\s+:\s+\\(.+?\\)$"    (1 'font-lock-type-face t))
      ("^\\(WARNING\\)\s+:\s+\\(.+?\\)$" (0 'font-lock-warning-face t))
      ("^\\(ERROR\\)\s+:\s+\\(.+?\\)$"   (0 'error t))
      ("---+.+"   (0 'header-line-highlight t))
      (" :|: .+?$" (0 'shadow t))
      )
    "Highlighting for logview-mode"
    )
  )

(defvar logview-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?- "." st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: ".:2" st)
    st)
  "Syntax table for the logview-mode")

(defun logview-mode-filter ()
  " choose a log level, create a new buffer with just that level int it "
  (interactive)
  (let* ((level (ivy-read "Log Level: " '(debug info warning error)))
         (regex (format "^%s\s+:" (upcase level)))
         (current (buffer-file-name))
         (filtered (get-buffer-create (format "*Log: %s(filtered)*" (f-filename current))))
         (display-buffer-alist nil)
         (kill-whole-line t)
        )
    (with-current-buffer filtered (erase-buffer))
    (call-process "ggrep" nil filtered nil "-E" regex current)
    (with-current-buffer filtered
      (logview-mode)
      (goto-char (point-min))
      )
    (display-buffer filtered '(display-buffer-same-window))
    )
  )

(evil-define-motion logview-forward-section (count)
  "Move to the next block of statements that are a different level"
  :jump t
  :type inclusive
  (beginning-of-line)
  (let ((current-level (word-at-point)))
    (while (and (not (eobp))
                (s-equals? current-level (word-at-point)))
      (forward-line 1)
      )
    )
  )

(evil-define-motion logview-backward-section (count)
  :jump t
  :type inclusive
  (beginning-of-line)
  (let ((current-level (word-at-point)))
    (while (and (not (bobp))
                (s-equals? current-level (word-at-point))
                )
      (forward-line -1)
      )
    )
  )

(define-derived-mode logview-mode fundamental-mode
  "logview"
  (interactive)
  (kill-all-local-variables)
  (use-local-map logview-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list logview-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'logview-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'logview-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table logview-mode-syntax-table)
  ;;
  (setq major-mode 'logview-mode)
  (setq mode-name "logview")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  (align-regexp (point-min) (point-max) "\\(\s-*\\):")
  (set-buffer-modified-p nil)

  )
(add-to-list 'auto-mode-alist '("/log\\..+\\'" . logview-mode))

(provide 'logview-mode)
;;; logview-mode.el ends here
