(defun ha/insert-two-spaces (N)
  "Inserts two spaces at the end of sentences."
  (interactive "p")
  (when (and (looking-back "[!?.] " 2)
             (not (looking-back "[0-9]\. ")))
    (insert " ")))

(advice-add 'org-self-insert-command :after #'ha/insert-two-spaces)

(defun ha/org-special-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond
     ;; Open links like usual
     ((eq 'link (car (org-element-context)))
      (org-return))

     ;; lists end with two blank lines, so we need to make sure
     ;; we are also not at the beginning of a line to avoid a
     ;; loop where a new entry gets created with only one blank
     ;; line.
     ((and (org-in-item-p) (not (bolp)))
      (if (org-element-property :contents-begin (org-element-context))
          (evil-org-eol-call 'clever-insert-item)
        (delete-region (line-beginning-position) (line-end-position))))

     ((org-at-heading-p)
      (if (string= "" (org-element-property :title (org-element-context)))
          (delete-region (line-beginning-position) (line-end-position))
        (org-insert-heading-after-current)))

     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     (t
      (org-insert-heading-after-current)))))

(defun ha/refile-org-section-to-technical ()
  "Archive the org-mode section and create an entry in my
Technical directory. The formatting, since it is an archive,
isn't quite what I want, but it gets it going."
  (interactive)
  (let* ((header (substring-no-properties (org-get-heading)))
         (title (if (string-match ": \\(.*\\)" header)
                    (match-string 1 header)
                  header))
         (filename (replace-regexp-in-string "\s+" "-" (downcase title)))
         (filepath (format "~/technical/%s.org" filename))
         (org-archive-location (format "%s::" filepath)))
    (org-archive-subtree)
    (find-file-other-window filepath)))
