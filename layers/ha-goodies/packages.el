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
    full-ack
    autoinsert
    fancy-narrow
    ;; magithub
    visual-regexp
    visual-regexp-steroids)
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

(defun ha-goodies/init-full-ack ()
  (use-package full-ack
    :ensure t
    :init
    (autoload 'ack-same "full-ack" nil t)
    (autoload 'ack "full-ack" nil t)
    (autoload 'ack-find-same-file "full-ack" nil t)
    (autoload 'ack-find-file "full-ack" nil t)

    :config
    ;; Having troubles with `projectile-ack', and perhaps this direct wrapper
    ;; will help with both using `full-ack' as well as address the annoying
    ;; mode-line issues associated with `ag':
    (spacemacs/set-leader-keys "p s" 'ack)
    (spacemacs/set-leader-keys "p /" 'projectile-ack)))

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

(defun ha-goodies/post-init-git-timemachine ()
  "Configure git-timemachine: https://github.com/pidu/git-timemachine"
  (spacemacs/set-leader-keys
    "g f t" 'git-timemachine)

  (defun git-timemachine-show-commit ()
    (interactive)
    (magit-show-commit (car git-timemachine-revision))))

(defun ha-goodies/init-fancy-narrow ()
  "Configure the fancy-narrow package: https://github.com/Bruce-Connor/fancy-narrow

This project works really well for code-reviews and
presentations, and I just make it a little easier to work with by
wrapping the standard functions. "

  (use-package fancy-narrow
    :ensure t
    :config
    (defun ha/highlight-block ()
      "Highlights a 'block' in a buffer defined by the first
      blank line before and after the current cursor position.
      Uses the 'fancy-narrow' mode to high-light the block."
      (interactive)
      (let (cur beg end)
        (setq cur (point))
        (setq end (or (re-search-forward  "^\s*$" nil t) (point-max)))
        (goto-char cur)
        (setq beg (or (re-search-backward "^\s*$" nil t) (point-min)))
        (fancy-narrow-to-region beg end)
        (goto-char cur)))

    (defun ha/highlight-dwim (num)
      "If some of the buffer is highlighted with the `fancy-narrow'
      mode, then un-highlight it by calling `fancy-widen'.

      If region is active, call `fancy-narrow-to-region'.

      If NUM is 0, highlight the current block (delimited by
      blank lines). If NUM is positive or negative, highlight
      that number of lines. Otherwise, called
      `fancy-narrow-to-defun', to highlight current function."
         (interactive "p")
         (cond
          ((fancy-narrow-active-p)  (fancy-widen))
          ((region-active-p)        (progn
                                      (fancy-narrow-to-region (region-beginning) (region-end))
                                      (deactivate-mark)))
          ((= num 0)                (ha/highlight-block))
          ((= num 1)                (fancy-narrow-to-defun))
          (t                        (progn (ha/expand-region num)
                                           (fancy-narrow-to-region (region-beginning) (region-end))
                                           (setq mark-active nil)))))

    :bind (("S-<f16>" . ha/highlight-dwim))))

;;; packages.el ends here
