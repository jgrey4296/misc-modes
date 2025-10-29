;;; timeline-insert-state.el -*- lexical-binding: t; -*-

(evil-define-state timeline-insert
  "Insert State with spaces remapped"
  :tag "<TI>"
  :message "-- Timeline Insert --"
  :suppress-keymap nil
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-stop-track-last-insertion)
  :input-method t
  )


(map! :map evil-timeline-insert-state-map
      "." ".BCE"
      "," " -> "
      ;; TODO map a control to choose the SPC replacement
      )


(map! :map timeline-mode-map
      ;; TODO activate the state somewhere better
      "i" #'evil-timeline-insert-state
      ;; tag
      ;; verify
      ;; amend
      ;; add person
      )

(provide 'timeline-insert-state)
;;; timeline-insert-state.el ends here
