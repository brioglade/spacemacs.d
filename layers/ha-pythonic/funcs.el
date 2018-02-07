;;; FUNCS --- Python-specific helper functions
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2018, Howard Abrams, all rights reserved.
;; Created: 19 January 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;     Along with the use of pipenv to support a virtual environment, we are
;;     experimenting in writing some helper functions for programming Python.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun ha/pipenv-shell (directory)
  "Creates a terminal containing a pipenv shell command."
  (interactive (list (read-directory-name "Project: "
                                          (file-name-directory (or (buffer-file-name)
                                                                   "~/Other")))))
  (let ((default-directory directory))
    (shell "*pythonic*")
    (insert "source ~/.profile")
    (comint-send-input)
    (insert "pipenv shell")
    (comint-send-input)
    (insert "PS1='$ '")
    (comint-send-input)
    (insert "clear")
    (comint-send-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; funcs.el ends here
