;; overlay-ctrl-minor-mode.el -*- mode: elisp; lexical-binding: t; -*-

(define-minor-mode overlay-ctrl-minor-mode
  "Minor Mode to handle overlay controls"
  :init-value nil
  :lighter "OvCtrl"
  (defvar-local overlay_control/overlays '() "Buffer Local Overlays")
  )

(defun overlay-ctrl-on ()
  (unless (minibufferp)
    (overlay-ctrl-minor-mode 1)
    )
  )

(define-globalized-minor-mode global-overlay-ctrl-mode overlay-ctrl-minor-mode overlay-ctrl-on)

(provide 'overlay-ctrl-minor-mode)
;;; overlay-ctrl-minor-mode.el ends here
