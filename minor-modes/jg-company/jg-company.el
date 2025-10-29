 ;;; jg-company.el -*- lexical-binding: t; no-byte-compile: t;-*-
(require 'company)

;;-- vars
(defvar-local jg-company-activation-re nil)
(defvar-local jg-company-kws (make-hash-table :test 'equal))
;;-- end vars

(defun jg-company/backend (cmd &rest args)
  (interactive (list 'interactive))
  (cl-case cmd
    (init            nil)
    ;; Get the value to complete
    (prefix  (when (and jg-company-activation-re
                        (s-matches? jg-company-activation-re (or (current-word) "")))
               (current-word)))
    ;; Get candidates of completion
    (candidates (gethash (current-word) jg-company-kws))
    ;; Defaults
    (sorted          t)
    (duplicates      nil)
    (ignore-case     t)
    (no-cache        t)
    ;; Documentation location:
    (doc-buffer      nil)
    ;; Location of candidate definition
    (location        nil)
    ;; Add data in completion window
    (annotation      nil)
    ;; Add data in echo
    (meta            nil)
    ;; For Late expansion of snippets etc
    (post-completion nil)
    ;; For easy use of backend:
    (interactive     (company-begin-backend 'jg-company/backend))
    ;; Difference between usage / creation:
    (require-match   nil)

    (t               nil)
    )
  )

(define-minor-mode jg-company-mode
  "A minor mode for adding the jg-company backend"
  :global t

  )

(provide 'jg-company)
;;; jg-company.el ends here
