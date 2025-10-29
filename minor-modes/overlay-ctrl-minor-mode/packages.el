;; packages.el -*- mode: elisp; lexical-binding: t; -*-

(package! helm)
(package! overlay-ctrl-minor-mode :recipe (:local-repo (expand-file-name "packages/minor-modes/overlay-ctrl-minor-mode" doom-user-dir)))
(package! font-lock+ :recipe (:host github :repo "emacsmirror/font-lock-plus"))

;;; packages.el ends here
