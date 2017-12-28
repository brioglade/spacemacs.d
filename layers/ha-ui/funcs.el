;;; funcs.el --- Configuration functions for the UI and Display  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Howard Abrams

;; Author: Howard Abrams <howard.abrams@HABRAMS-02>
;; Keywords: convenience


(defface spaceline-ruby-rvm
  '((t (:foreground "red1" :distant-foreground "DarkRed")))
  "Face for highlighting the Ruby RVM."
  :group 'spaceline)

(spaceline-define-segment ruby-rvm
  "The current Ruby virtual machine.  Works with `rvm'."
  (when (and active
             (eq 'ruby-mode major-mode)
             (bound-and-true-p rvm--current-ruby))
    (propertize
     (concat (replace-regexp-in-string "ruby-" "" rvm--current-ruby)
             (propertize "\xe92b" ; "\xe92a"
                         'face `(:family "all-the-icons" :height 1.0)
                         'display '(raise -0.1))
             (when rvm--current-gemset
               rvm--current-gemset))
     'face 'spaceline-ruby-rvm
     'help-echo (format "RVM Gem Path: %s" (rvm--emacs-gemhome)))))

;; While my segment seems to return correctly, I don't know how to actually
;; insert it into the modeline without recreating an existing modeline... should
;; I base it off of: (powerline-center-evil-theme)
