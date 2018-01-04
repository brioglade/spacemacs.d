;;; packages.el --- my-goodies layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Howard Abrams <howard.abrams@HABRAMS-02>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-goodies-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-goodies/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-goodies/pre-init-PACKAGE' and/or
;;   `my-goodies/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ha-goodies-packages
  '(ag visual-regexp visual-regexp-steroids)
  "The list of Lisp packages required by the my-goodies layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun ha-goodies/init-ag ()
  (use-package ag
    :ensure t
    :config
    (spacemacs/set-leader-keys
      "p s" 'projectile-ag)))

(defun ha-goodies/init-visual-regexp ()
  (use-package visual-regexp
    :ensure t
    :bind (("C-c r" . vr/replace)
           ("C-c q" . vr/query-replace))
    :config
    (spacemacs/set-leader-keys
      "r r" 'vr/replace
      "r q" 'vr/query-replace
      "r m" 'vr/mc-mark)))

(defun ha-goodies/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :ensure t))

(defun ha-goodies/post-init-autoinsert
    (use-package autoinsert
      :init
      (setq auto-insert-directory (concat (getenv "HOME")
                                          "/.spacemacs.d/templates/"))
      ;; Don't want to be prompted before insertion:
      (setq auto-insert-query nil)

      (add-hook 'find-file-hook 'auto-insert)
      (auto-insert-mode 1)))

;;; packages.el ends here
