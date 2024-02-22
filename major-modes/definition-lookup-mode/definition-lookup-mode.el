;;; definition-lookup-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;; File Commentary:
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end header

;;-- imports

;;-- end imports

;;-- keymap

(defvar-local definition-lookup-mode-map
    (make-sparse-keymap))

;;-- end keymap

;;-- font lock

(defconst definition-lookup-font-lock-keywords
    (rx-let ((w (x) (: x (0+ blank)))
                (g (x) (group x))
                (ln (: punctuation line-end))
                (word+ (group word-start (+ (| word punct)) (0+ blank)))
                (basic-syms (| "@" "+" "!" "<-" "?" "-" "&"))
                (basic-kws  (| "percept" "self" "include" "register_function"))
                (agent-ids (| "beliefs" "goals" "debug" "verbose" "ag-class" "ag-arch" "ag-bb-class" "myparameter" "instances" "join" "focus" "roles"))
                (org-ids   (| "responsible-for" "debug" "group" "players" "owner"))
                )
        (list

            )
        )
    "Highlighting for definition-lookup-mode"
    )

;;-- end font lock

;;-- syntax

(defvar definition-lookup-mode-syntax-table
    (let ((st (make-syntax-table)))
        ;; Symbols
        (modify-syntax-entry ?. "_" st)
        (modify-syntax-entry ?! "_" st)
        (modify-syntax-entry ?$ "_" st)
        (modify-syntax-entry ?+ "_" st)
        (modify-syntax-entry ?- "_" st)
        (modify-syntax-entry ?? "_" st)
        (modify-syntax-entry ?@ "_" st)
        (modify-syntax-entry ?\; "_" st)
        ;;underscores are valid parts of words:
        (modify-syntax-entry ?_ "w" st)
        ;; Comments start with // and end on newlines
        (modify-syntax-entry ?/ ". 124" st)
        (modify-syntax-entry ?* "_ 23b" st)
        (modify-syntax-entry ?\n ">" st)
        ;; Strings
        (modify-syntax-entry ?" """ st)
        ;; Pair parens, brackets, braces
        (modify-syntax-entry ?( "()" st)
        (modify-syntax-entry ?\[ "(]" st)
        (modify-syntax-entry ?{ "(}" st)
        (modify-syntax-entry ?: ".:2" st)
        st)
    "Syntax table for the definition-lookup-mode"
    )

;;-- end syntax

;;-- mode definition

(define-derived-mode definition-lookup-mode fundamental-mode
    "definition-lookup"
    (interactive)
    (kill-all-local-variables)
    (use-local-map definition-lookup-mode-map)

    ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
    (set (make-local-variable 'font-lock-defaults) (list definition-lookup-font-lock-keywords nil))
    ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'definition-lookup-syntactic-face-function)
    ;; (set (make-local-variable 'indent-line-function) 'definition-lookup-indent-line)
    (set (make-local-variable 'comment-style) '(plain))
    (set (make-local-variable 'comment-start) "//")
    (set (make-local-variable 'comment-use-syntax) t)
    (set-syntax-table definition-lookup-mode-syntax-table)
    ;;
    (setq major-mode 'definition-lookup-mode)
    (setq mode-name "definition-lookup")
    (outline-minor-mode)
    (yas-minor-mode)
    (run-mode-hooks)
    )
(add-to-list 'auto-mode-alist '("\.definition-lookup\'" . definition-lookup-mode))

;;-- end mode definition

(provide 'definition-lookup-mode)

;;-- footer
;; Copyright (C) 2024 john
;;
;; Author: john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created: February 08, 2024
;; Modified: February 08, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end footer
;;; definition-lookup-mode.el ends here
