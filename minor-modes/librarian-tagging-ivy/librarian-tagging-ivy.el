;;; tagging-ivy.el -*- lexical-binding: t; -*-
;; Adds actions to ivy for easy search + tagging of files from dired

(eval-when-compile
  (require 'ivy)
  (require 'librarian--tag)
  (require 'librarian--tag-chart)
  )

(defvar librarian-tagging-selected-tag nil)

(defun librarian-tagging-ivy-tag-set (x)
  " Register a tag to reuse "
  (message "Registering")
  (setq librarian-tagging-selected-tag (read-string "Store Tag: "))
  )

(defun librarian-tagging-ivy-tag-clear (x)
  " Clear the registered tag "
  (message "Clearing")
  (setq librarian-tagging-selected-tag nil)
  )

(defun librarian-tagging-ivy-set-tags (tag)
  (goto-char (line-beginning-position))
  (if (re-search-forward "^\*\* Thread:.+?\s+\\(:.+?:\\)$" (line-end-position) t)
        (let* ((match (match-data))
               (the-match (match-string-no-properties 1))
               (tag-set (make-hash-table :test 'equal))
               (tags (if the-match (split-string the-match ":" t "\s+") nil))
               replacement)

          (puthash tag 1 tag-set)
          (mapc #'(lambda (x) (puthash x 1 tag-set)) tags)
          (setq replacement (format ":%s:" (string-join (hash-table-keys tag-set) ":")))
          (set-match-data match)
          (replace-match replacement nil nil nil 1)
          )
    (progn
      (goto-char (line-end-position))
      (insert "          " ":" tag ":"))))

(defun librarian-tagging-ivy-tag (x)
  "Opens the current candidate in another window."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let* ((file-name   (match-string-no-properties 1 x))
           (line-number (string-to-number (match-string-no-properties 2 x)))
           (full-file (expand-file-name file-name (ivy-state-directory ivy-last)))
           (input (plist-get (plist-get (ivy-state-extra-props ivy-last) :ivy-data) :text))
           (the-tag (if (not librarian-tagging-selected-tag) (read-string "Tag as: ") librarian-tagging-selected-tag))
          )
      (message "Using Tag: %s" the-tag)
      (with-temp-buffer
        ;; open the file indirectly
        (insert-file-contents full-file t)
        ;; go to the match
        (goto-char (point-min))
        (forward-line (1- line-number))
        ;; go up to its thread header
        (if (re-search-backward "^\*\* Thread:" nil t)
            (progn
              ;; Set Tags
              (librarian-tagging-ivy-set-tags the-tag)
              ;; Save the file
              (write-file full-file))
          )
        )
      )
    )
  )

(defun librarian-tagging-ivy-replace (x)
  "Opens the current candidate in another window."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let* ((file-name   (match-string-no-properties 1 x))
           (line-number (string-to-number (match-string-no-properties 2 x)))
           (full-file (expand-file-name file-name (ivy-state-directory ivy-last)))
           (the-line (s-trim (match-string-no-properties 3 x)))
           (input ivy-text)
           the-tag
          )
      (setq the-tag (if (not librarian-tagging-selected-tag)
                        (read-string (format "Replace %s with: " the-line))
                      librarian-tagging-selected-tag))
      ;;(message "Swapping %s for: %s in %s:%s" input the-tag full-file line-number)
      (with-temp-buffer
        ;; open the file indirectly
        (insert-file-contents full-file t)
        ;; go to the match
        (goto-char (point-min))
        (forward-line (1- line-number))
        (beginning-of-line)
        ;; Replace
        (if (re-search-forward input (line-end-position) t)
            (progn (replace-match the-tag t)
                   ;; Save the file
                   (write-file full-file))
          (message "Match Not Found")))
      )
    )
  )

(ivy-set-actions 'counsel-rg
                 '(("t" librarian-tagging-ivy-tag "Tag")
                   ("T" librarian-tagging-ivy-tag-set "Set Tag")
                   ("C" librarian-tagging-ivy-tag-clear "Clear Tag")
                   ("r" librarian-tagging-ivy-replace "Replace Tag")
                   ))

(provide 'librarian-tagging-ivy)
