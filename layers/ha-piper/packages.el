;;; packages.el --- ha-piper layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; URL: https://github.com/howardabrams/spacemacs.d/layers/ha-piper
;;
;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;;  Offers a number of functions placed text -> lines Spacemacs submenu, next to
;;  the `sort-lines' and `uniquify-lines'

;;; Code:

(defconst ha-piper-packages
  '(load-env-vars)
  "The list of Lisp packages required by the ha-piper layer
should be minimal. ")

;; Configure a package that allows you to source in environment variables.
;; See: https://github.com/diasjorge/emacs-load-env-vars/

(defun ha-org/init-load-env-vars ()
  (use-package load-env-vars))

(defun ha-org/init-dash ()
  (use-package dash
    :ensure t))

;;; packages.el ends here
