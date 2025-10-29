;;; basic-org-babel-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)

(describe "sanity"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
)
