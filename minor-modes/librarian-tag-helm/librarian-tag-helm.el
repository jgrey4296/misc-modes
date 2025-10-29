;;; tagging-helm.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'evil)
  (require 'dash)
  (require 'helm-source)
  (require 'helm-grep)
  (require 'helm-utils)
  (require 'helm-files)
  (require 'librarian--tag)
  (require 'librarian--tag-chart)
  (require 'librarian--tag-mode)
  )

(defvar litm--helm-source
  (helm-make-source "Helm Tagging" 'helm-source
    :action (helm-make-actions "Re-entrant-set" #'litm-set-tags-re-entrant
                               "Set"            #'litm-set-tags-oneshot)
    :pattern-transformer #'(lambda (x) (car (librarian-normalize-tags major-mode (list x))))
    :candidates #'(lambda () litm--candidate-names)
    :allow-dups nil
    )
  )

(defvar litm--fallback-source
  (helm-build-dummy-source "Helm Tags Fallback Source"
    :action (helm-make-actions "Re-entrant-Create" #'litm-set-tags-re-entrant
                               "Create"            #'litm-set-tags-oneshot)
    :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
  )

(defvar litm--helm-buffer-name  "*Helm Tags*")

(defvar litm-re-entrant-quit-char "|")

(defvar litm--candidate-names '() "Current candidate names")

(defvar litm--global-max-length nil "cached global length of the largest tag")

(defvar litm--global-max-count nil "cached global largest count of a tag")

(defvar litm--global-bar-chart nil "The cached bar char of tags")

(define-advice helm-grep--prepare-cmd-line (:override (only-files &optional include zgrep)
                                                      librarian-tag-mode-grep-helm-override)
  " "
  (let* ((default-directory (or helm-ff-default-directory
                                (helm-default-directory)
                                default-directory))
         (fnargs            (helm-grep-prepare-candidates
                             only-files default-directory))
         (ignored-files     (unless (helm-grep-use-ack-p)
                              (mapconcat (lambda (x) (concat "--exclude=" (shell-quote-argument x)))
                                         helm-grep-ignored-files " ")))
         (ignored-dirs      (unless (helm-grep-use-ack-p)
                              (mapconcat (lambda (x) (concat "--exclude-dir=" (shell-quote-argument x)))
                                         helm-grep-ignored-directories " ")))
         (exclude           (unless (helm-grep-use-ack-p)
                              (let ((inc     (and include (concat include " ")))
                                    (igfiles (and ignored-files (concat ignored-files " ")))
                                    (igdirs  (and helm-grep-in-recurse ignored-dirs)))
                                (concat inc igfiles igdirs))))
         (types             (and (helm-grep-use-ack-p) (or include "")))
         (smartcase         (if (helm-grep-use-ack-p) ""
                              (unless (let ((case-fold-search nil))
                                        (string-match-p "[[:upper:]]" helm-pattern))
                                "i")))
         (helm-grep-default-command (concat helm-grep-default-command " %m"))
         (pipe-switches (mapconcat 'identity helm-grep-pipe-cmd-switches " "))
         (patterns (helm-mm-split-pattern helm-pattern t))
         (pipcom (concat " | " (helm-grep--pipe-command-for-grep-command smartcase pipe-switches "grep")))
         (pipes (if (cdr patterns)
                    (concat pipcom (s-join pipcom (mapcar #'shell-quote-argument (cdr patterns))))
                  ""))
         (cmd (format-spec helm-grep-default-command
                           (delq nil
                                 (list (unless zgrep
                                         (if types
                                             (cons ?e types)
                                           (cons ?e exclude)))
                                       (cons ?c (or smartcase ""))
                                       (cons ?p (shell-quote-argument (car patterns)))
                                       (cons ?f fnargs)
                                       (cons ?m pipes)
                                       ))))
         )
    cmd
    )
  )

(define-advice helm-grep--pipe-command-for-grep-command (:override (smartcase pipe-switches &optional grep-cmd)
                                                                   librarian-tag-mode-helm-ggrep-fix)
  (pcase (or grep-cmd (helm-grep-command))
    ;; Use grep for GNU regexp based tools.
    ((or "grep" "zgrep" "git-grep")
     (format "grep --color=always%s %s"
             (if smartcase " -i" "") pipe-switches))
    ("ggrep"
     (format "grep --color=always%s %s"
             (if smartcase " -i" "") pipe-switches))
    ((and (pred (string-match-p "ack")) ack)
     (format "%s --smart-case --color %s" ack pipe-switches)))
  )

(defun litm-insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (car (helm-marked-candidates))))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n"))))
  )

(defun litm-sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) librarian--tag-global-tags 1))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )
    )
  )

(defun litm-propertize-entry (candidate)
  (let ((propstring (concat candidate)))
    (progn (put-text-property 0
                              (length propstring)
                              'font-lock-face
                              'rainbow-delimiters-depth-1-face
                              propstring))
    (list propstring candidate))
  )

(defun litm-format-candidates ()
  " Given Candidates, colour them if they are assigned, then sort them,
formatted as a bar chart
  "
  (let* ((global-tags librarian--tag-global-tags)
         (current-tags librarian--tag--current-entry-tags)
         )
    (cond ((hash-table-empty-p global-tags) ;; there are no tags
             nil)
          ((null litm--global-max-length) ;; the max tag hasn't been calculated
           (let* ((cand-keys (hash-table-keys global-tags))
                  (cand-vals (hash-table-values global-tags))
                  (cand-pairs (-zip-pair cand-keys cand-vals))
                  (maxTagLength (apply 'max (or litm--global-max-length 0)  (mapcar 'length cand-keys)))
                  (maxTagAmount (apply 'max (or litm--global-max-count 0) cand-vals))
                  (display-pairs (or litm--global-bar-chart
                                     (-zip-pair
                                      (librarian--tag-chart--make-bar-chart cand-pairs maxTagLength maxTagAmount)
                                      cand-keys)))
                  (propertied-tags (mapcar #'litm-propertize-entry current-tags))
                  )
             (setq litm--global-max-length maxTagLength
                   litm--global-max-count  maxTagAmount
                   litm--global-bar-chart  display-pairs
                   litm--candidate-names (append (sort propertied-tags 'litm-sort-candidates)
                                                 display-pairs)
                   )
             ))
          (t ;; Otherwise combine entry current with totals
           (let ((propertied-tags (mapcar #'litm-propertize-entry current-tags)))
             (setq litm--candidate-names (append (sort propertied-tags 'litm-sort-candidates)
                                                 litm--global-bar-chart)
                   )
             )
           )
          )
    )
  )

(defun litm-set-tags-oneshot (x)
  (with-current-buffer helm-current-buffer
    ;; Add new tags to the global record
    (let* ((flat (-flatten (helm-marked-candidates)))
           (newtags (librarian-tag-mode-set-tags flat))
           )
      (message "Flat Tags: %s" flat)
      (message "NewTags: %s" newtags)
      (cl-loop for new in newtags
               do
               (message "Adding new tag: %s" new)
               (push (cons (format "%s : 1+" new) new)
                     litm--global-bar-chart)
               )
      )
    )
  )

(defun litm-set-tags-re-entrant (x)
  ;; Add the tags
  (litm-set-tags-oneshot x)
  ;; Maybe resume the helm
  (cond ((-contains? (-flatten (helm-marked-candidates)) litm-re-entrant-quit-char) ;; quit signalled
         nil)
        (t ;; else add and resume
         (with-helm-buffer
           (setq-local helm-input-local " ")
           )
         (helm-resume litm--helm-buffer-name)
         )
        )
  )

(defun litm-clear-candidates ()
  (interactive)
  (setq litm--global-max-length nil
        litm--global-max-count  nil
        litm--global-bar-chart  nil
        litm--candidate-names   nil
        )
  )

(evil-define-command librarian-tag-helm (&optional beg end type)
  " Opens the Tagging Helm "
  (interactive "<R>")
  (unless librarian-tag-mode (user-error "Tagging Minor Mode not active"))
  (set-marker librarian--tag-marker end)
  (get-buffer-create litm--helm-buffer-name)

  (save-excursion
    (goto-char beg)
    (librarian-tag-mode-get-tags)
    (litm-format-candidates)
    (helm :sources (list litm--helm-source litm--fallback-source)
          :input ""
          :buffer litm--helm-buffer-name
          )
    )
  )

(provide 'librarian-tag-helm)
;;; librarian-tag-helm.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("litm-" . "librarian--tag-helm-")
;; )
;; End:
