;;; tagging-helm.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'evil)
  (require 'helm-source)
  (require 'helm-grep)
  (require 'helm-utils)
  (require 'helm-files)
  (require 'librarian--tag)
  (require 'librarian--tag-chart)
  )

(defvar librarian-tagging-helm--helm-source
  (helm-make-source "Helm Tagging" 'helm-source
    :action (helm-make-actions "Re-entrant-set" #'librarian-tagging-helm-set-tags-re-entrant
                               "Set"            #'librarian-tagging-helm-set-tags-oneshot)
    :pattern-transformer #'(lambda (x) (car (librarian-normalize-tags major-mode (list x))))
    :candidates #'(lambda () librarian-tagging-helm--candidate-names)
    ;; :update #'(lambda () librarian-tagging-helm--candidate-names)
    :allow-dups nil
    )
  )

(defvar librarian-tagging-helm--fallback-source
  (helm-build-dummy-source "Helm Tags Fallback Source"
    :action (helm-make-actions "Re-entrant-Create" #'librarian-tagging-helm-set-tags-re-entrant
                               "Create"            #'librarian-tagging-helm-set-tags-oneshot)
    :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
  )

(defvar librarian-tagging-helm--helm-buffer-name  "*Helm Tags*")

(defvar librarian-tagging-helm-re-entrant-quit-char "|")

(defvar librarian-tagging-helm--candidate-names '())

(defvar librarian-tagging-helm--global-max-length nil)

(defvar librarian-tagging-helm--global-max-count nil)

(defvar librarian-tagging-helm--global-bar-chart nil)

(define-advice helm-grep--prepare-cmd-line (:override (only-files &optional include zgrep)
                                                      librarian-tag-mode-grep-helm-override)
  ""
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
         ;; (patterns-alt (s-join " -e " (mapcar #'shell-quote-argument patterns)))
         (cmd (format-spec helm-grep-default-command
                           (delq nil
                                 (list (unless zgrep
                                         (if types
                                             (cons ?e types)
                                           (cons ?e exclude)))
                                       (cons ?c (or smartcase ""))
                                       (cons ?p (shell-quote-argument (car patterns)))
                                       ;; (cons ?p patterns-alt)
                                       (cons ?f fnargs)
                                       (cons ?m pipes)
                                       ;; (cons ?m "")
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

(defun librarian-tagging-helm-insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (car (helm-marked-candidates))))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n"))))
  )

(defun librarian-tagging-helm--sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) librarian-tag-mode-global-tags 1))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )

    (defun librarian-tagging-helm--propertize-entry (candidate)
      (let ((propstring (concat candidate)))
        (progn (put-text-property 0
                                  (length propstring)
                                  'font-lock-face
                                  'rainbow-delimiters-depth-1-face
                                  propstring))
        (list propstring candidate))
      )
    )
  )

(defun librarian-tagging-helm-format-candidates ()
  " Given Candidates, colour them if they are assigned, then sort them,
formatted as a bar chart
  "
  (let* ((global-tags librarian-tag-mode-global-tags)
         (current-tags librarian-tagging--current-entry-tags)
         )
    (cond ((hash-table-empty-p global-tags)
           nil)
          ((null librarian-tagging-helm--global-max-length)
           (let* ((cand-keys (hash-table-keys global-tags))
                  (cand-vals (hash-table-values global-tags))
                  (cand-pairs (-zip-pair cand-keys cand-vals))
                  (maxTagLength (or librarian-tagging-helm--global-max-length
                                    (apply 'max (mapcar 'length cand-keys))))
                  (maxTagAmount (or librarian-tagging-helm--global-max-count
                                    (apply 'max cand-vals)))
                  (display-pairs (or librarian-tagging-helm--global-bar-chart
                                     (-zip-pair
                                      (librarian-tagging-chart--make-bar-chart cand-pairs maxTagLength maxTagAmount)
                                      cand-keys)))
                  (propertied-tags (mapcar #'librarian-tagging-helm--propertize-entry current-tags))
                  )
             (setq librarian-tagging-helm--global-max-length maxTagLength
                   librarian-tagging-helm--global-max-counti maxTagAmount
                   librarian-tagging-helm--global-bar-chart  display-pairs
                   librarian-tagging-helm--candidate-names (append (sort propertied-tags 'librarian-tagging-helm--sort-candidates)
                                                                   display-pairs)
                   )
             ))
          (t (let ((propertied-tags (mapcar #'librarian-tagging-helm--propertize-entry current-tags))
                   )
               (setq librarian-tagging-helm--candidate-names (append (sort propertied-tags 'librarian-tagging-helm--sort-candidates)
                                                                     librarian-tagging-helm--global-bar-chart)
                     )
               )
             )
          )
    )
  )

(defun librarian-tagging-helm-set-tags-oneshot (x)
  (with-current-buffer helm-current-buffer
    (librarian-tag-mode-set-tags (-flatten (helm-marked-candidates)))
    )
  )

(defun librarian-tagging-helm-set-tags-re-entrant (x)
  (librarian-tagging-helm-set-tags-oneshot x)
  (cond ((-contains? (-flatten (helm-marked-candidates)) librarian-tagging-helm-re-entrant-quit-char)
         nil
         )
        (t (with-helm-buffer
             (setq-local helm-input-local " ")
             )
           (helm-resume librarian-tagging-helm--helm-buffer-name)
           )
        )
  )

(evil-define-command librarian-tagging-helm (&optional beg end type)
  " Opens the Tagging Helm "
  (interactive "<R>")
  (unless librarian-tag-mode (user-error "Tagging Minor Mode not active"))
  (set-marker librarian-tag-mode-marker end)
  (get-buffer-create librarian-tagging-helm--helm-buffer-name)

  (save-excursion
    (goto-char beg)
    (librarian-tag-mode-get-tags)
    (librarian-tagging-helm-format-candidates)
    (helm :sources (list librarian-tagging-helm--helm-source librarian-tagging-helm--fallback-source)
          :input ""
          :buffer librarian-tagging-helm--helm-buffer-name
          )
    )
  )

(provide 'librarian-tagging-helm)
