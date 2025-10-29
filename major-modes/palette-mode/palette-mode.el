;;; palette-mode.el -*- lexical-binding: t; -*-

(defvar-local palette-mode-map
  (make-sparse-keymap))

;; https://gka.github.io/palettes/
;; https://www.whocanuse.com/


;; List of '(regex (groupnum "face")+)
(defconst palette-font-lock-keywords
  (list)
  "Highlighting for palette-mode"
  )

(defconst palette-mode-syntax-table (copy-syntax-table emacs-lisp-mode-syntax-table))

(define-derived-mode palette-mode fundamental-mode
  "palette"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map palette-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list palette-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'palette-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'palette-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table palette-mode-syntax-table)
  ;;
  (setq major-mode 'palette-mode)
  (setq mode-name "palette")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  (rainbow-mode)

  )
(add-to-list 'auto-mode-alist '("\\.palette" . palette-mode))


(provide 'palette-mode)
;;; palette-mode.el ends here
