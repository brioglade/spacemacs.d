;;; FUNCS --- Functions to aid to processing output from shell commands
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2018, Howard Abrams, all rights reserved.
;; Created: 11 May 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    An idea to write some functions that manipulate the output from
;;    shell commands and lodged in the *Shell Command Output* Buffer.
;;
;;    See http://www.howardism.org/Technical/Emacs/project-piper.html
;;    for all the details and background.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should hAve received A copy of the GNU GenerAl Public License
;; Along with this progrAm; see the file COPYING.  If not, write to
;; the Free SoftwAre FoundAtion, Inc., 51 FrAnklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun pipe (command)
  "Replaces the contents of the buffer with the output from the
command given."
  (interactive "sCommand: ")
  (save-restriction
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (let ((current-prefix-arg '(4)))
      (shell-command-on-region (point-min) (point-max) command))))

;; I noticed that the `keep-lines' function operates on a region or the buffer
;; from the current location of the point on down. I would like it to operate on
;; the region or the _entire buffer_. This function will call another function
;; interactively with the point at the start of the buffer.

(defun piper--func-on-region-or-buffer (func)
  "Call the `func' function interactively if the region is
active, otherwise, go to the beginning of the buffer before
calling the function."
  (save-excursion
    (unless (region-active-p)
      (goto-char (point-min)))
    (call-interactively func)))

;; My trick in naming these functions with a prefix of `spacemacs/' means
(defun spacemacs/keep-lines ()
  "Removes all lines expect those that match a given regular
expression either in an active region, or in the entire buffer."
  (interactive)
  (piper--func-on-region-or-buffer #'keep-lines))

(defun spacemacs/flush-lines ()
  "Removes all lines that match a given regular expression either
in an active region, or in the entire buffer."
  (interactive)
  (piper--func-on-region-or-buffer #'flush-lines))


(defun source-environment (file)
  "Add all environment variable settings from a script file into
the current Emacs environment, via the `setenv' function."
      (interactive "fSource file:")
      (save-excursion
        (with-temp-buffer
          (insert-file-contents file)
          ;; This hairy regular expression matches KEY=VALUE shell expressions:
          (while (re-search-forward "\\([A-z_]*\\) *= *[\"']?\\(.*?\\)[\"']?$" nil t)
            (let* ((key (match-string 1))
                   (env-value (match-string 2))
                   ;; Since the value could contain references to other environment
                   ;; variables, we'll try to substitute what we find:
                   (value (replace-regexp-in-string "${?\\([A-z_]*\\)}?"
                                                    (lambda (p)
                                                      (getenv (match-string 1 p)))
                                                    env-value t)))
              (setenv key value)
              (message "Stored environment variable %s = %s" key value))))))

;; ------------------------------------------------
;;  A replacement for the `cut' cli program...

(defun range-of-numbers (str)
  "Convert a string, STR, that specifies a range of numbers into a list of integers."
  (when (string-blank-p str)
    '())

  (let* ( ; Start with a dash? Let's have that as a range from 1:
         (initial-one (replace-regexp-in-string "^ *- *" "1-" str))
         ;; Since we can separate on spaces, make sure dashes don't have spaces:
         (spaceless-dash (replace-regexp-in-string " *- *" "-" initial-one)))
    (-flatten
     (-map (lambda (n)
               (if (not (string-match "\\([0-9]*\\)[ \t]*-[ \t]*\\([0-9]*\\)" n))
                   (string-to-number n)
                 ;; Got a range, convert to a sequence:
                 (number-sequence (string-to-number (match-string 1 n))
                                  (string-to-number (match-string 2 n)))))
             (split-string spaceless-dash " *[ ,] *" t)))))

(defun column-lines-for-line (line delimiter fields)
  "Separates LINE into a list of strings delimited with
DELIMITER. Returns elements specified by FIELDS, which could be a
single integer number, a collection of columns (separated by
commas), or a range with two numbers separated by a dash."
  (let* ((elements (split-string line delimiter)))
    (-map (lambda (n)
            (nth-value (1- n) elements))
          fields)))

(defun column-lines (delimiter fields)
  "Replaces contents of buffer with a subset where each line is a
particular field separated by a delimiter. This is similar to
the `cut' command, but without all the features...yet."
  (interactive "sDelimiter: \nsColumn Numbers(s): ")

  (let ((field-choices (range-of-numbers fields)))
    (cl-flet* ((line-splitter (line)
                             (column-lines-for-line line delimiter field-choices))
              (line-processor (line) ; Split and rejoin the line:
                              (mapconcat #'identity (line-splitter line) delimiter)))
      (save-excursion
        (goto-char (point-min))
        ;; The fastest approach is _not_ to use regular expressions, but...
        (while (re-search-forward "^\\(.*\\)$" nil t)
          (replace-match (line-processor (match-string 1))))))))


;;  But what if we want to simply convert a section of tabular-formatted
;;  text into an s-expression, essentially a list of lists. We could then
;;  further process it...

(defun table-to-lists (delimiter &optional trim)
  "Converts the contents of the buffer (or a region) into a list
of lists where each line should be separated by DELIMITER. If
TRIM to `t', each column element is trimmed of whitespace."
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end   (if (region-active-p) (region-end)       (point-max))))
    (mapcar (lambda (line) (split-string line delimiter nil trim))
            (split-string (buffer-substring-no-properties start end) "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; funcs.el ends here
