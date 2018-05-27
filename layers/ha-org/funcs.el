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
;; ------------------------------------------------------------
;;   Functions that help with capturing
;; ------------------------------------------------------------
(require 'which-func)

(defun ha/org-capture-get-src-block-string (major-mode)
  "Given a major mode symbol, return the associated org-src block
string that will enable syntax highlighting for that language
e.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."
  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (let ((org-src-mode (ha/org-capture-get-src-block-string major-mode))
        (func-name (which-function)))
    (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name)))

(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil))

(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (with-current-buffer (find-buffer-visiting f)
    (let* ((code-snippet
            (buffer-substring-no-properties (mark) (- (point) 1)))
           (file-name   (buffer-file-name))
           (file-base   (file-name-nondirectory file-name))
           (line-number (line-number-at-pos (region-beginning)))
           (initial (if (null func-name)
                        (format "From [[file:%s::%s][%s]]:"
                                file-name line-number file-base)
                      (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                              func-name file-name line-number
                              file-base))))
      (format "
   %s

   #+BEGIN_%s %s
%s
   #+END_%s" initial type headers code-snippet type))))
