
;; Yeah, yeah, yeah...I love Emacs, but some functions could make
;; better assumptions to what you want, and of course, we can
;; waste any good bindings. For instance, a function may require
;; a region to be selected before working, but a keybinding
;; shouldn't. It should make good assumptions, where the 'region'
;; would simply override those assumptions.

;; Case in point, is the 'line' work bound to =Ctrl-k= ... cut to
;; the end of the line, fine, but giving it a prefixed number
;; will cut multiple lines, but not the full text of the current
;; line...like why would you ever do that?

;; *Note:* I want to look at the old Tiny Tools project and steal
;;  their deletion code.

;; * Aligning Variables

;;   I like the =align-= collection of functions, however,
;;   setting the regular expression for =align-regexp= can be
;;   daunting to remember for some tasks.

;;   For instance, I like to have my maps and hashtables and
;;   other variable assignments aligned, but calling
;;   =align-regexp= interactively leaves a bit to be desired.

;;   This amounts to two primary steps:
;;   1. Move each line in the region to the column specified by
;;      the first line
;;   2. Increase the spaces (after the initial indention) so that
;;      this “second column” lines up (done with =align-regexp=).

(defun align-variables (start end)
  "Attempts to align all variables in an assignment list or keys
    in a hash table. For instance:

      (\"org-mode\"
       :base-extension \"org\"
       :recursive t
       :headline-levels 4  ; Just the default for this project.
       :auto-sitemap t     ; Generate sitemap.org automagically
      )

    Turns into the following if the region begins on the first line
    with the colon:

      (\"org-mode\"
        :base-extension  \"org\"
        :recursive       t
        :headline-levels 4  ; Just the default for this project.
        :auto-sitemap    t     ; Generate sitemap.org automagically
      )

    Note: This currently does not align the comments.

    All lines in region will be indented to the position of the
    first line. For most languages/modes, this should be
    sufficient, but if it doesn't work, start the region as the
    column they should all be indented. For instance:

       var x = 10,
           start = beginningOfFile,
           end = File.end();

    Start the region at the x, to achieve:

       var x     = 10,
           start = beginningOfFile,
           end   = File.end();"
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let* ((times (count-lines start end))
           (real-start (if (looking-at-p "[ \\t(]")
                           (1- (search-forward-regexp "[^ \\t(]" end t))
                         start))
           (real-end nil)  ;; Will be set later
           (dest-column (progn
                          (goto-char real-start)
                          (current-column))))

      ;; Step 1. Align all lines to the column of the text in the first line
      (dotimes (line times)
        (forward-line)
        (indent-line-to dest-column))
      (setq real-end (point))

      ;; Step 2. Align all the values in a second column
      (align-regexp real-start real-end "\\(\\s-*\\)\\(\\S-*\\)\\(\\s-*\\)" 3 1 nil))))

;; * Kill Entire Lines

;;   According to [[http://endlessparentheses.com/kill-entire-line-with-prefix-argument.html][this article]], killing the rest of the line is fine,
;;   but =C-3 C-k= kills only 2½ lines. Not so useful.

;;   This creates a macro that moves to the beginning of the line
;;   and then calls a function given to it. Quite an interesting
;;   approach:

(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
    Except it moves to beginning of line before calling FUNCTION when
    called with a prefix argument. The FUNCTION still receives the
    prefix argument."
  (let ((name (intern (format "ha/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to the beginning of the line when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

;; And we re-bind them to functions that use them.

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap sp-kill-hybrid-sexp] (bol-with-prefix sp-kill-hybrid-sexp))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))

(global-set-key (kbd "C-k") (bol-with-prefix kill-line))

;; * Better Newline
;;
;;    Since =paredit= and other modes automatically insert final
;;    characters like semi-colons and parenthesis, what I really
;;    want is to hit return from the /end of the line/. Pretty
;;    simple function.

(defun newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;; And we can bind that to the free, /Meta-Return/:

(global-set-key (kbd "C-<return>") 'newline-for-code)

;; Remember, this works everywhere /except/ for org-mode.

;; * Join Lines

;;    I like how =M-SPC= removes all but one space, and =M-\=
;;    removes all spaces. Would be nice to remove all /newlines/
;;    in the same way.

;;    Sure, =C-x C-o= removes all following newlines, so if at
;;    the end of the first line that should be /joined/, then
;;    this acts somewhat like =M-SPC=.

(defun join-lines (prefix)
  "If at the end of the line, will join the following line to the
    end of this one...unless it is blank, in which case, it will
    keep joining lines until the next line with text is
    connected."
  (interactive "P")

  ;; Move to the the beginning of the white space before
  ;; attempting this process. This allows us to join lines even
  ;; if we are in the middle of some empty lines.
  (if prefix
      (re-search-backward "[^[:space:]\\r\\n]"))
  (forward-char)

  ;; Just in case we have some trailing whitespace we can't see,
  ;; let's just get rid of it. Won't do anything if in the middle
  ;; of a line, or if there is not trailing whitespace.
  (delete-trailing-whitespace (point) (point-at-eol))

  ;; While we are at the end of the line, join a line, remove the
  ;; whitespace, and keep on going until we're through...
  (while (eq (point-at-eol) (point))
    (delete-char 1)
    (delete-trailing-whitespace (point) (point-at-eol))))

;; (global-set-key (kbd "C-RET") 'join-lines)

;;    I would like to have =M-RET= remove the lines similar to
;;    the way =M-SPC= works, but that is already bound in
;;    =org-mode= to making a special header, so I'll just bind it
;;    to Control.


;; * Better Beginning of Line

;;    This [[http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/][Emacs Redux article]] has a great suggestion for having =C-a= go
;;    to the beginning of the line's content instead of the actual
;;    beginning of the line. Hit =C-a= a second to get to the actual
;;    beginning.

;; (defun smarter-move-beginning-of-line (arg)
;;   "Move point back to indentation of beginning of line.

;;   Move point to the first non-whitespace character on this line.
;;   If point is already there, move to the beginning of the line.
;;   Effectively toggle between the first non-whitespace character and
;;   the beginning of the line.

;;   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
;;   point reaches the beginning or end of the buffer, stop there."
;;     (interactive "^p")
;;     (setq arg (or arg 1))

;;     ;; Move lines first
;;     (when (/= arg 1)
;;       (let ((line-move-visual nil))
;;         (forward-line (1- arg))))

;;     (let ((orig-point (point)))
;;       (back-to-indentation)
;;       (when (= orig-point (point))
;;         (move-beginning-of-line 1))))

;;   ;; remap C-a to `smarter-move-beginning-of-line'
;;   (global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
;;   (global-set-key [remap org-beginning-of-line]  'smarter-move-beginning-of-line)


;; * Next and Previous File

;;    Sometimes it is obvious what is the /next file/ based on the one
;;    I'm currently reading. For instance, in my journal entries, the
;;    filename is a number that can be incremented. Same with
;;    presentation files...

(defun split-string-with-number (string)
  "Returns a list of three components of the string, the first is
  the text prior to any numbers, the second is the embedded number,
  and the third is the rest of the text in the string."
  (let* ((start (string-match "[0-9]+" string))
         (end (string-match "[^0-9]+" string start)))
    (if start
        (list (substring string 0 start)
              (substring string start end)
              (if end  (substring string end)  "")))))

;;    Which means that the following defines this function:

;; (split-string-with-number "abc42xyz")  ;; ("abc" "42" "xyz")
;; (split-string-with-number "42xyz")     ;; ("" "42" "xyz")
;; (split-string-with-number "abc42")     ;; ("abc" "42" "")
;; (split-string-with-number "20140424")  ;; ("" "20140424" "")
;; (split-string-with-number "abcxyz")    ;; nil

;;    Given this splitter function, we create a function that takes some
;;    sort of operator and return a new filename based on the conversion
;;    that happens:

(defun find-file-number-change (f)
  (let* ((filename (buffer-file-name))
         (parts    (split-string-with-number
                    (file-name-base filename)))
         (new-name (number-to-string
                    (funcall f (string-to-number (nth 1 parts))))))
    (concat (file-name-directory filename)
            (nth 0 parts)
            new-name
            (nth 2 parts))))

;; And this allows us to create two simple functions that can load the
;; "next" and "previous" files:

(defun find-file-increment ()
  "Takes the current buffer, and loads the file that is 'one
  more' than the file contained in the current buffer. This
  requires that the current file contain a number that can be
  incremented."
  (interactive)
  (find-file (find-file-number-change '1+)))

(defun find-file-decrement ()
  "Takes the current buffer, and loads the file that is 'one
  less' than the file contained in the current buffer. This
  requires that the current file contain a number that can be
  decremented."
  (interactive)
  (find-file (find-file-number-change '1-)))

(spacemacs/set-leader-keys
  "f+" 'find-file-increment
  "f-" 'find-file-decrement)


;; * Better Transpose Word

;;   Seems that the prefix feature of the =traspose-words= function is
;;   backwards... at least, for case my use whatever

(defun ha/transpose-words (pre)
  "Wrapper around `transpose-words' and `org-transpose-words'
    that works more intuitively when you are at the end of the line."
  (interactive "p")
  (let ((wrap-this (if (equal major-mode 'org-mode)
                       'org-transpose-words
                     'transpose-words)))
    ;; At the end of the line with a positive prefix?
    (when (looking-at "\s*$")
      (cond
       ;; No prefix? Send last word back before the previous...
       ((null pre) (setq pre -1))
       ;; Call it with a negative version of the prefix...
       ((> pre 0)  (setq pre (- 0 pre)))))
    (funcall wrap-this pre)))

(add-hook 'org-mode-hook (lambda()
                           (define-key org-mode-map [remap org-transpose-words] #'ha/transpose-words)))
(global-set-key [remap transpose-words] #'ha/transpose-words)

;; Unfilling a paragraph joins all the lines in a paragraph into a single line.
;; Taken from here: http://www.emacswiki.org/UnfillParagraph ... I use this all
;; the time.

(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(spacemacs/set-leader-keys
  "xq" 'fill-paragraph
  "xQ" 'unfill-paragraph)

;; However, auto insertion requires entering data for particular fields,
;; and for that Yasnippet is better, so in this case, we combine them:

(defun ha/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun is-mode-p (mode)
  "Predicate to return `true' if the current buffer's major mode
matches the requested MODE."
  (buffer-local-value 'major-mode (current-buffer))
  (eq major-mode mode))


;; Searching and File Location

(setq locate-command "mdfind")  ;; Use Mac OS X's Spotlight

;; However, the problem with locate, is it doesn’t show me any context. My
;; find-notes script uses both mdfind and grep to both better search and display
;; some useful context. Just need to wrap that in a function:

(defun find-notes (words)
  "Search `org-mode' files in specific directories for WORDS.

Uses `find-notes' shell script as a better grep utility.  Not only
does it show the results in a clickable list, it also highlights
the result, allowing us to put more context in the output."
  (interactive "sSearch for words:")
  (let ((program (concat (getenv "HOME") "/bin/find-notes"))
        (buffer-name (concat "*find-notes: " words "*")))
    (call-process program nil buffer-name t words)
    (switch-to-buffer buffer-name)
    (read-only-mode 1)
    (grep-mode)
    (toggle-truncate-lines)
    (beginning-of-buffer)
    (dolist (word (split-string words))
      (highlight-regexp word))))

(spacemacs/set-leader-keys
  "fn" 'find-notes
  "zb" 'text-scale-adjust)

;; The following idea to directly add the previous misspelled word to the
;; personal dictionary was taken from this discussion:
;; https://emacs.stackexchange.com/questions/16837/add-last-mistake-to-dictionary-with-flyspell-and-ispell

(defun flyspell-goto-previous-error ()
  "Go to previous spelling error."
  (interactive)
  (push-mark (point) t nil)
  (let ((pos (point))
        (min (point-min)))
    (while (and (> pos min)
                (let ((ovs (overlays-at pos))
                      (r '()))
                  (while (and (not r) (consp ovs))
                    (if (flyspell-overlay-p (car ovs))
                        (setq r t)
                      (setq ovs (cdr ovs))))
                  (not r)))
      (backward-word 1)
      (setq pos (point)))
    (goto-char pos)))

(defun flyspell-add-false-postive ()
  "Add previous false positive to dict"
  (interactive)
  (save-excursion
    (flyspell-goto-previous-error)
    (let* ((flyspell-info (flyspell-get-word))
           (word (car flyspell-info))
           (bounds (cdr flyspell-info)))
      (flyspell-do-correct 'save nil word (point) (car bounds) (cadr bounds) (point))
      (flyspell-delete-region-overlays (car bounds) (cadr bounds)))))

(spacemacs/set-leader-keys
  "SS" 'ispell-word
  "Sp" 'flyspell-goto-previous-error
  "Sa" 'add-false-postive)
