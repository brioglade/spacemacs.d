;;; packages.el --- my-goodies layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
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
  '(ag
    autoinsert
    visual-regexp
    visual-regexp-steroids
    git-timemachine
    floobits)
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

(defun ha-goodies/init-autoinsert ()
  (use-package autoinsert
    :init
    (setq
     auto-insert 'other
     auto-insert-directory (concat (getenv "HOME")
                                   "/.spacemacs.d/templates/")
     ;; Don't want to be prompted before insertion:
     auto-insert-query nil)
    (add-hook 'find-file-hook 'auto-insert)
    (auto-insert-mode 1)

    :config
    ;; Every template that begins with `default' will be automatically added as
    ;; the default template for a particular file extension.
    (dolist (template (directory-files auto-insert-directory nil "default.*"))
      (let* ((ext (file-name-extension template))
             (pattern (concat "\\." ext "$")))
        (define-auto-insert pattern (vector template 'ha/autoinsert-yas-expand))))

    (define-auto-insert "Sprint.*\\.\\org\\'" ["sprint.org" ha/autoinsert-yas-expand])
    (define-auto-insert "test_.*\\.\\rb\\'" ["test_spec.rb" ha/autoinsert-yas-expand])
    (define-auto-insert "/bin/"  ["default.sh" ha/autoinsert-yas-expand])))

(defun ha-goodies/init-git-timemachine ()
  "Configure git-timemachine: https://github.com/pidu/git-timemachine"
  (use-package git-timemachine
    :defer t
    :ensure t
    :keys (:local
           "c" git-timemachine-show-commit)
    :config
    (spacemacs/set-leader-keys
      "g f t" 'git-timemachine)

    (defun git-timemachine-show-commit ()
      (interactive)
      (magit-show-commit (car git-timemachine-revision))))

(defun ha-goodies/init-floobits ()
  "Floobits: https://github.com/Floobits/floobits-emacs"
  (use-package floobits
    :ensure t
    :config
    (spacemacs/declare-prefix "a f" "floobits")
    (spacemacs/set-leader-keys
      "a f J" 'floobits-join-workspace
      "a f S" 'floobits-share-dir-private   ; Create a workspace and populate it with the contents of the directory, DIR (or make it).
      "a f s" 'floobits-summon              ; Summon everyone in the workspace to your cursor position
      "a f f" 'floobits-follow-mode-toggle  ; Toggle following of recent changes.
      "a f c" 'floobits-clear-highlights    ; Clears all mirrored highlights.
      "a f L" 'floobits-leave-workspace)))

;;; packages.el ends here
