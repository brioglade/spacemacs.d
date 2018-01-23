(defconst ha-emacs-lisp-packages
  '(paren evil-cleverparens suggest))

(defun ha-emacs-lisp/init-paren ()
  "The reverse mode of the default parenthesis matching doesn’t match well,
  and this package makes it bold and more obvious. "
  (use-package paren
    :init
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#afa")
    (set-face-attribute  'show-paren-match nil :weight 'black)
    (set-face-background 'show-paren-mismatch (face-background 'default))
    (set-face-foreground 'show-paren-mismatch "#c66")
    (set-face-attribute  'show-paren-mismatch nil :weight 'black)))

(defun ha-emacs-lisp/init-evil-cleverparens ()
  "evil-cleverparens (https://github.com/luxbock/evil-cleverparens)
is a keybinding layer (a minor mode for lisp-modes) that gives
evil-lisp-state-like keys to evil's normal state."
  (use-package evil-cleverparens
    :after evil
    :ensure t
    :diminish evil-cleverparens-mode
    :commands (evil-cleverparens-mode)
    :init
    (setq evil-move-beyond-eol t
          evil-cleverparens-use-additional-movement-keys t)
    (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
    (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)))

(defun ha-emacs-lisp/init-suggest ()
  "This separate application helps me find the Lisp function
based on behavior (input and output). Call its main function:
suggest"
  (use-package suggest
    :ensure t))
