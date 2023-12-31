;; Test Execution

(defun org-unit-test-forward-to (locator bound)
  (cond
   ((eq :document locator) nil)
   ((eq :section locator) nil)
   (t (re-search-forward (format org-unit-test-heading-regexp locator) bound))
   )
  )

(defun org-unit-test-forward-past-tests (locator bound)
  (org-unit-test-forward-to locator bound)
  (let ((orig (point)))
    (re-search-forward org-drawer-regexp bound)
    (if (s-equals? "__org-unit-test-layer__" (match-string 1))
        (re-search-forward org-drawer-regexp bound)
      (goto-char orig))
    )
  )

(defun org-unit-test-run-tests (testgroups)
  (mapcar 'org-unit-test-run-test-group testgroups)
  )

(defun org-unit-test-run-test-group (testgroup)
  (cl-assert (org-unit-test-group-p testgroup) t)
  (goto-char (org-unit-test-group-start testgroup))
  (let* ((name (org-unit-test-group-name testgroup))
         (bound (org-unit-test-group-bound testgroup))
         (results (mapcar (lambda (x) `(,(org-unit-test-to-string x) . ,(org-unit-test-run-test x bound))) (org-unit-test-group-tests testgroup)))
         )
    (make-jg-org-test-results :name name :results results :link (org-unit-test-group-link testgroup)))
  )

(defun org-unit-test-run-test (test bound)
  (cl-assert (org-unit-test-p test))
  (save-excursion
    (condition-case e
        (let ((type (org-unit-test-type test)))
          (pcase
           (:section-check    (org-unit-test-run-test-section test bound))
           (:length-check     (org-unit-test-run-test-length test bound))
           (:order-check      (org-unit-test-run-test-order test bound))
           (:citation-check   (org-unit-test-run-test-citation test bound))
           (:mention-check     (org-unit-test-run-test-mention test bound))
           (:codeblock-check  (org-unit-test-run-test-codeblock test bound))
           (:tag-check        (org-unit-test-run-test-tag test bound))
           (_ (error "Unrecognized test type"))
           )
          )
      (search-failed nil)
      )
    )
  )

(defun org-unit-test-run-test-section (test bound)
  ;;go to the locator
  (org-unit-test-forward-past-tests (org-unit-test-locator test) bound)
  ;; get subheadings
  (let ((subheadings (org-map-entries
                      (lambda () (downcase (substring-no-properties (org-get-heading)))) nil 'tree)))
    ;; is the section value in the subheadings?
    (-contains? subheadings (org-unit-test-value test))
    )
  )

(defun org-unit-test-run-test-length (test bound)
  (org-unit-test-forward-past-tests (org-unit-test-locator test) bound)
  (let* ((vals (org-unit-test-value test))
         (dir (car vals))
         (length (cadr vals))
         (counter (caddr vals))
         (test (if (eq :larger dir) '> '<))
         )
    (cond
     ((eq counter :words) (funcall test (count-words (point)
                                                     (plist-get (cadr (org-element-at-point)) :contents-end))
                                   length))
     ;; ((eq counter :paras) (funcall test (count-paragraphs (point)
     ;;                                                      (plist-get (cadr (org-element-at-point)) :contents-end))
     ;;                               length)
     ((eq counter :sects)  (funcall test (- (reduce '+ (org-map-entries (lambda () 1) nil 'tree)) 1) length))
     (t (error "Unrecognized test-length counter"))
     )
    )
  )

(defun org-unit-test-run-test-order (test bound)
  (let* ((tree (org-map-entries (lambda () (substring-no-properties (org-get-heading))) nil 'tree))
         (locator (-elem-index (org-unit-test-locator test) tree))
         (section (-elem-index (org-unit-test-value test) tree))
         )
    (if (and locator section)
        (< locator section)
      nil)
    )
  )

(defun org-unit-test-run-test-citation (test bound)
  (org-unit-test-forward-past-tests (org-unit-test-locator test) bound)
  (let ((citations (org-unit-test-value test)) curr-line)
    (while citations
      (re-search-forward "cite:" bound)
      (setq curr-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (setq citations (remove-if (lambda (x) (s-contains? x curr-line)) citations))
      )
    )
  )

(defun org-unit-test-run-test-mention (test bound)
  (org-unit-test-forward-past-tests (org-unit-test-locator test) bound)
  (search-forward (org-unit-test-value test) bound)
  )

(defun org-unit-test-run-test-codeblock (test bound)
  (org-unit-test-forward-past-tests (org-unit-test-locator test) bound)
  (re-search-forward (format "%s %s" org-unit-test-src-block-regexp
                             (if (org-unit-test-value test) (org-unit-test-value test) "")) bound)
  )

(defun org-unit-test-run-test-tag (test bound)
  (org-unit-test-forward-to (org-unit-test-locator test) bound)
  (let ((tags (plist-get (cadr (org-element-at-point)) :tags)))
    (-contains? tags (org-unit-test-value test))
    )
  )
