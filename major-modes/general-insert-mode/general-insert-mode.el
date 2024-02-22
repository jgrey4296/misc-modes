;;; general-insert-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;; File Commentary:
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end header

;;-- imports

;;-- end imports

;;-- keymap

(defvar-local general-insert-mode-map
    (make-sparse-keymap))

;;-- end keymap

;;-- font lock

(defconst general-insert-font-lock-keywords
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
    "Highlighting for general-insert-mode"
    )

;;-- end font lock

;;-- syntax

(defvar general-insert-mode-syntax-table
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
    "Syntax table for the general-insert-mode"
    )

;;-- end syntax

;;-- mode definition

(define-derived-mode general-insert-mode fundamental-mode
    "general-insert"
    (interactive)
    (kill-all-local-variables)
    (use-local-map general-insert-mode-map)

    ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
    (set (make-local-variable 'font-lock-defaults) (list general-insert-font-lock-keywords nil))
    ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'general-insert-syntactic-face-function)
    ;; (set (make-local-variable 'indent-line-function) 'general-insert-indent-line)
    (set (make-local-variable 'comment-style) '(plain))
    (set (make-local-variable 'comment-start) "//")
    (set (make-local-variable 'comment-use-syntax) t)
    (set-syntax-table general-insert-mode-syntax-table)
    ;;
    (setq major-mode 'general-insert-mode)
    (setq mode-name "general-insert")
    (outline-minor-mode)
    (yas-minor-mode)
    (run-mode-hooks)
    )
(add-to-list 'auto-mode-alist '("\.general-insert\'" . general-insert-mode))

;;-- end mode definition

(provide 'general-insert-mode)

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
;;; general-insert-mode.el ends here
