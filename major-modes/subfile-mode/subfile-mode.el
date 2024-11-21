;;; subfile-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;; File Commentary:
;;
;;
;;-- end header

;;-- imports
(require 'rx)
;;-- end imports

;;-- keymap

(defvar-local subfile-mode-map
    (make-sparse-keymap))

;;-- end keymap

;;-- font lock

(defconst subfile-font-lock-keywords
    (rx-let ((w (x) (: x (0+ space)))
                (g (x) (group x))
                (ln line-end)
                (subword (+ (| word (any "-,.0-9_"))))
                (sep (w "::"))
                (wordgroup (+ (w (+ subword))))
                (rest (: (*? not-newline) ln))
                )
        (list
         `(,(rx (g sep) (g wordgroup))
           (1 'font-lock-regexp-face t t)
           (2 'org-checkbox t t))
         `(,(rx line-start (g wordgroup) (g sep) (g (w (+ digit))))
           (0 'error)
           (1 'org-priority t t)
           (2 'font-lock-regexp-face t t)
           (3 'font-lock-type-face t t))
         `(,(rx line-start "%% " (*? anychar) ln)
           (0 'font-lock-comment-face t))
         )
        )
    "Highlighting for subfile-mode"
    )

;;-- end font lock

;;-- syntax

(defvar subfile-mode-syntax-table
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
        (modify-syntax-entry ?/ ". 125" st)
        (modify-syntax-entry ?* "_ 23b" st)
        (modify-syntax-entry ?\n ">" st)
        ;; Strings
        (modify-syntax-entry ?\" "\"" st)
        ;; Pair parens, brackets, braces
        (modify-syntax-entry ?\( "()" st)
        (modify-syntax-entry ?\[ "(]" st)
        (modify-syntax-entry ?{ "(}" st)
        (modify-syntax-entry ?: ".:2" st)
        st)
    "Syntax table for the subfile-mode"
    )
;;-- end syntax

;;-- mode definition

(define-derived-mode subfile-mode text-mode
    "subfile"
    (interactive)
    (kill-all-local-variables)
    (use-local-map subfile-mode-map)

    ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
    (set (make-local-variable 'font-lock-defaults) (list subfile-font-lock-keywords nil))
    ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'subfile-syntactic-face-function)
    ;; (set (make-local-variable 'indent-line-function) 'subfile-indent-line)
    (set (make-local-variable 'comment-style) '(plain))
    (set (make-local-variable 'comment-start) "%%")
    (set (make-local-variable 'comment-use-syntax) t)
    (set-syntax-table subfile-mode-syntax-table)
    ;;
    (setq major-mode 'subfile-mode)
    (setq mode-name "subfile")
    (run-mode-hooks)
    )
(add-to-list 'auto-mode-alist '("\.sub\'" . subfile-mode))

;;-- end mode definition

(provide 'subfile-mode)

;;-- footer
;; Copyright (C) 2024 john
;;
;; Author: john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created: November 21, 2024
;; Modified: November 21, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end footer
;;; subfile-mode.el ends here
