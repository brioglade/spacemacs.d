;;; packages.el --- my-org layer packages file for Spacemacs.
;;
;; Author: Howard Abrams <howard@google.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on
;; how to implement a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this
;; layer should be added to `ha-org-packages'. Then, for each
;; package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer,
;;   define a function `ha-org/init-PACKAGE' to load and
;;   initialize the package.

;; - Otherwise, PACKAGE is already referenced by another
;;   Spacemacs layer, so define the functions
;;   `ha-org/pre-init-PACKAGE' and/or `ha-org/post-init-PACKAGE'
;;   to customize the package as it is loaded.

;;; Code:

(defconst ha-org-packages
  '(
    ;; We don't have to specify `org' because it is listed in layers.el
    ;; org
    org-journal
    org-tree-slide
    org-beautify-theme
    ;; ox-html
    ;; ox-reveal
    ))

(defun ha-org/init-org ()
  (use-package org
    :init
    (setq org-return-follows-link t
          ;; Speed Commands: If point is at the beginning of a headline or code
          ;; block in org-mode, single keys do fun things. See
          ;; org-speed-command-help for details (or hit the ? key at a headline).
          org-use-speed-commands t
          org-hide-emphasis-markers t
          org-completion-use-ido t
          org-outline-path-complete-in-steps nil
          org-src-fontify-natively t   ;; Pretty code blocks
          org-src-tab-acts-natively t
          org-confirm-babel-evaluate nil
          org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
                              (sequence "|" "CANCELED(c)")))

    :config
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
    (add-hook 'org-mode-hook 'yas-minor-mode-on)

    (font-lock-add-keywords            ; A bit silly but my headers are now
     'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                            nil)))
                 ("^\\*+ \\(DOING\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                            nil)))
                 ("^\\*+ \\(CANCELED\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                            nil)))
                 ("^\\*+ \\(DONE\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                            nil)))
                 ;; Here is my approach for quickly making the
                 ;; initial asterisks for listing items and
                 ;; whatnot, appear as Unicode bullets (without
                 ;; actually affecting the text file or the
                 ;; behavior).
                 ("^ +\\([-*]\\) "
                  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Trying an experiment to see if I like inserting two spaces
    ;; at the end of a sentence:
    (defun ha/insert-two-spaces (N)
      "Inserts two spaces at the end of sentences."
      (interactive "p")
      (when (looking-back "[!?.] " 2)
        (insert " ")))
    (advice-add 'org-self-insert-command :after #'ha/insert-two-spaces)

    ;; For the most part, I like electric-indent-mode, however,
    ;; it doesn’t really play well with org-mode, so I just bind
    ;; the Return key to the org-return-indent function and get
    ;; the same effect (but only if I am not in a source code
    ;; block…which actually insert multiple new lines). This
    ;; return and indent feature is fine, since when I save a
    ;; file, I automatically strip off trailing whitespace.

    ;; (add-hook 'org-mode-hook
    ;; 	      (lambda ()
    ;; 		(define-key org-mode-map [remap org-return]
    ;; 		  (lambda () (interactive)
    ;; 		    (if (org-in-src-block-p)
    ;; 			(org-return)
    ;; 		      (org-return-indent))))))

    ;; From [[http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/][this discussion]], I got the code to replace ~M-RET~ in lists with
    ;; just ~RET~, so that Org acts more like other word processors.

    (defun ha/org-return (&optional ignore)
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
         ;; lists end with two blank lines, so we need to make sure we are also not
         ;; at the beginning of a line to avoid a loop where a new entry gets
         ;; created with only one blank line.
         ((and (org-in-item-p) (not (bolp)))
          (if (org-element-property :contents-begin (org-element-context))
              (org-insert-heading)
            (beginning-of-line)
            (setf (buffer-substring
                   (line-beginning-position) (line-end-position)) "")
            (org-return)))
         ((org-at-heading-p)
          (if (not (string= "" (org-element-property :title (org-element-context))))
              (progn (org-end-of-meta-data)
                     (org-insert-heading))
            (beginning-of-line)
            (setf (buffer-substring
                   (line-beginning-position) (line-end-position)) "")))
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
          (org-return)))))

    (add-hook 'org-mode-hook
        (lambda ()
            (define-key org-mode-map (kbd "RET")  #'ha/org-return)))

    (spacemacs|define-transient-state org-movement
      :title "Movement in org-mode files"
      :doc "\n[_p_] previous heading [_n_] next heading [_u_] up to parent [_q_] quit"
      :bindings
      ("p" org-previous-visible-heading)
      ("n" org-next-visible-heading)
      ("u" outline-up-heading)
      ("j" org-next-item)
      ("k" org-previous-item)
      ("h" org-beginning-of-item-list)
      ("l" org-end-of-item-list)
      ("q" nil :exit t))

    (spacemacs/set-leader-keys-for-major-mode 'org-mode "o" 'spacemacs/org-movement-transient-state/body)

    ;; I like the idea of more movement within org-files, so I may want to add
    ;; the following:

    ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode
    ;;   ",p"  'org-previous-visible-heading
    ;;   ",n"  'org-next-visible-heading
    ;;   "oln" 'org-end-of-item-list
    ;;   "olp" 'org-beginning-of-item-list
    ;;   "oty" 'org-table-copy-region
    ;;   "otp" 'org-table-paste-rectangle)

    ;; :bind (:map org-mode-map
    ;;             ("M-C-n" . org-end-of-item-list)
    ;;             ("M-C-p" . org-beginning-of-item-list)
    ;;             ("M-C-u" . outline-up-heading)
    ;;             ("M-C-w" . org-table-copy-region)
    ;;             ("M-C-y" . org-table-paste-rectangle))
    ))

;; (defun ha-org/post-init-org-mime ()
;;   (use-package org-mime
;;     :ensure t))

;; (defun ha-org/init-org-mime-htmlize ()
;;   (use-package org-mime-htmlize
;;     :ensure t))

(defun ha-org/init-org-journal ()
  (use-package org-journal
    :ensure t
    :init
    (setq org-journal-dir "~/journal/")
    (setq org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)")
    (setq org-journal-time-format "")

    :config
    (defun get-journal-file-today ()
      "Return filename for today's journal entry."
      (let ((daily-name (format-time-string "%Y%m%d")))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-today ()
      "Create and load a journal file based on today's date."
      (interactive)
      (find-file (get-journal-file-today)))

    (defun get-journal-file-yesterday ()
      "Return filename for yesterday's journal entry."
      (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
             (daily-name (format-time-string "%Y%m%d" yesterday)))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-yesterday ()
      "Creates and load a file based on yesterday's date."
      (interactive)
      (find-file (get-journal-file-yesterday)))

    ;; Nice to /automatically/ insert a specific header if the journal entry
    ;; file is empty using [[https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html][auto-insert]].
    ;;
    ;; When I create a new journal entry, I want a snappy title and a
    ;; checklist of daily tasks.  The template should insert a date that
    ;; matches the file's name, not necessarily the current date.
    ;;
    ;; Also the inserted daily information and check-lists should only
    ;; happen if I am creating today's journal, not catching up with the
    ;; past... oh, and we might have special dailies to be inserted based
    ;; on the day of the week. Guess I /could/ use YAS snippets, but then the
    ;; code amount of code would over-shadow the text, so we'll make a
    ;; function.

    ;; To use this, make the following files:
    ;; - =journal-dailies.org= to contain the /real/ dailies
    ;; - =journal-dailies-end.org= to contain any follow-up notes
    ;; - =journal-mon.org= for additional text to be inserted on Monday journals
    ;; - And a =journal-XYZ.org= for each additional weekday
    (defun journal-file-insert ()
      "Insert's the journal heading based on the file's name."
      (interactive)
      (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
             (month (string-to-number (substring (buffer-name) 4 6)))
             (day   (string-to-number (substring (buffer-name) 6 8)))
             (datim (encode-time 0 0 0 day month year)))

        (insert (format-time-string org-journal-date-format datim))
        (insert "\n\n  $0\n") ; Start with a blank separating line

        ;; Note: The `insert-file-contents' leaves the cursor at
        ;; the beginning, so the easiest approach is to insert
        ;; these files in reverse order:

        ;; If the journal entry I'm creating matches today's date:
        (when (equal (file-name-base (buffer-file-name))
                     (format-time-string "%Y%m%d"))
          (insert-file-contents "journal-dailies-end.org")

          ;; Insert dailies that only happen once a week:
          (let ((weekday-template (downcase
                                   (format-time-string "journal-%a.org"))))
            (when (file-exists-p weekday-template)
              (insert-file-contents weekday-template)))
          (insert-file-contents "journal-dailies.org")
          (insert "$0")

          (let ((contents (buffer-string)))
            (delete-region (point-min) (point-max))
            (yas-expand-snippet contents (point-min) (point-max))))))

    (define-auto-insert "/[0-9]\\{8\\}$" [journal-file-insert])

    ;; I really would really like to read what I did last year
    ;; "at this time", and by that, I mean, 365 days ago, plus or
    ;; minus a few to get to the same day of the week.

    (defun journal-last-year-file ()
      "Returns the string corresponding to the journal entry that
    happened 'last year' at this same time (meaning on the same day
    of the week)."
      (let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
             (last-year (seconds-to-time last-year-seconds))
             (last-year-dow (nth 6 (decode-time last-year)))
             (this-year-dow (nth 6 (decode-time)))
             (difference (if (> this-year-dow last-year-dow)
                             (- this-year-dow last-year-dow)
                           (- last-year-dow this-year-dow)))
             (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
             (target-date (seconds-to-time target-date-seconds)))
        (format-time-string "%Y%m%d" target-date)))

    (defun journal-last-year ()
      "Loads last year's journal entry, which is not necessary the
    same day of the month, but will be the same day of the week."
      (interactive)
      (let ((journal-file (concat org-journal-dir (journal-last-year-file))))
        (find-file journal-file)))

    ;; I've notice that while I really like taking notes in a meeting, I
    ;; don't always like the multiple windows I have opened, so I created
    ;; this function that I can easily call to eliminate distractions
    ;; during a meeting.

    (defun meeting-notes ()
      "Call this after creating an org-mode heading for where the notes for the meeting
     should be. After calling this function, call 'meeting-done' to reset the environment."
      (interactive)
      (outline-mark-subtree)                              ;; Select org-mode section
      (narrow-to-region (region-beginning) (region-end))  ;; Only show that region
      (deactivate-mark)
      (delete-other-windows)                              ;; Get rid of other windows
      (text-scale-set 2)                                  ;; Text is now readable by others
      (fringe-mode 0)
      (message "When finished taking your notes, run meeting-done."))

    ;; Of course, I need an 'undo' feature when the meeting is over...

    (defun meeting-done ()
      "Attempt to 'undo' the effects of taking meeting notes."
      (interactive)
      (widen)                                       ;; Opposite of narrow-to-region
      (text-scale-set 0)                            ;; Reset the font size increase
      (fringe-mode 1)
      (winner-undo))                                ;; Put the windows back in place

    ;; Let's say you were in the middle of something, but would like to
    ;; /take a quick note/, but without affecting the file you are
    ;; working on. This is called a "capture", and is bound to the
    ;; following key:
    ;;
    ;; General notes are stored in [[file:~/personal/@SUMMARY.org][@SUMMARY.org]], and tasks synced with my
    ;; Google Task list are stored in [[file:~/personal/tasks.org][tasks.org]]:

    (defvar org-default-notes-file "~/personal/@SUMMARY.org")
    (defvar org-default-tasks-file "~/personal/tasks.org")

    ;; This will bring up a list of /note capturing templates/. I actually
    ;; override this in my [[file:emacs-local.org::*Org%20Configuration][system-specific "local" configuration]] file.

    (defun ha/first-header ()
      (goto-char (point-min))
      (search-forward-regexp "^\* ")
      (beginning-of-line 1)
      (point))

    (setq org-capture-templates
          '(("n" "Thought or Note"  entry
             (file org-default-notes-file)
             "* %?\n\n  %i\n\n  See: %a" :empty-lines 1)
            ("j" "Journal Note"     entry
             (file (get-journal-file-today))
             "* %?\n\n  %i\n\n  From: %a" :empty-lines 1)
            ("t" "Task Entry"        entry
             (file+function org-default-tasks-file ha/load-org-tasks)
             "* %?\n\n  %i\n\n  From: %a" :empty-lines 1)
            ("w" "Website Announcement" entry
             (file+function "~/website/index.org" ha/first-header)
             "* %?
      :PROPERTIES:
      :PUBDATE: %t
      :END:
      ,#+HTML: <div class=\"date\">%<%e %b %Y></div>

      %i

      [[%F][Read more...]" :empty-lines 1)))

    (spacemacs/set-leader-keys
      "fj" 'journal-file-today
      "fy" 'journal-file-yesterday
      "fY" 'journal-last-year
      "fd" 'dired-jump)

    ;; The trick to literate programming is in the [[http://orgmode.org/worg/org-contrib/babel/intro.html][Babel project]], which
    ;; allows org-mode to not only interpret source code blocks, but
    ;; evaluate them and tangle them out to a file.
    (add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((shell      . t)
                                   (js         . t)
                                   (emacs-lisp . t)
                                   (clojure    . t)
                                   (python     . t)
                                   (ruby       . t)
                                   (dot        . t)
                                   (css        . t)
                                   (plantuml   . t)))))

(defun ha-org/init-org-tree-slide()
  (use-package org-tree-slide
    :ensure t
    :init
    (setq org-tree-slide-skip-outline-level 4)
    (org-tree-slide-simple-profile)))

;; (defun ha-org/init-org-bullets ()
;;   (use-package org-bullets
;;     :ensure t
;;     :init
;;     (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun ha-org/init-org-beautify-theme ()
  (use-package org-beautify-theme
    :ensure t
    :init
    ;; (add-hook 'org-mode-hook 'org-bullets-mode)
    (load-theme 'org-beautify)))

(spacemacs|use-package-add-hook ox-html
  :post-init
  (setq org-html-postamble nil
        org-export-with-section-numbers nil
        org-export-with-toc nil
        org-html-head-extra "
          <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
          <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
          <style type='text/css'>
             body {
                font-family: 'Source Sans Pro', sans-serif;
             }
             pre, code {
                font-family: 'Source Code Pro', monospace;
             }
          </style>"))

;; Generate presentations from my org-mode files using
;; [[https://github.com/yjwen/org-reveal][org-reveal]]. Just download and make the results available to the
;; HTML output:

(spacemacs|use-package-add-hook ox-reveal
  :post-init
  (setq org-reveal-postamble "Howard Abrams"
        org-reveal-root (concat "file://" (getenv "HOME") "/Public/js/reveal.js")))

;;; packages.el ends here
