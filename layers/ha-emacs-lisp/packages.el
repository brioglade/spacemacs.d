(defconst ha-emacs-lisp-packages
  '(paren))

(defun ha-emacs-lisp/init-paren ()
  ;; The reverse mode of the default parenthesis matching doesn’t match as well,
  ;; so this code just makes it bold and more obvious:
  (use-package paren
    :init
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#afa")
    (set-face-attribute  'show-paren-match nil :weight 'black)
    (set-face-background 'show-paren-mismatch (face-background 'default))
    (set-face-foreground 'show-paren-mismatch "#c66")
    (set-face-attribute  'show-paren-mismatch nil :weight 'black)))
