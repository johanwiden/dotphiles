(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/jw/.config/emacs/.local/etc/bookmarks")
 '(custom-safe-themes t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages '(consult org-roam))
 '(safe-local-variable-values
   '((denote-mode . t)
     (eval ispell-change-dictionary "en_US")
     (eval org-expiry-deinsinuate)
     (org-blank-before-new-entry
      (heading . auto)
      (plain-list-item . auto))
     (org-list-description-max-indent . 5)
     (org-list-two-spaces-after-bullet-regexp)
     (eval add-hook 'before-save-hook
      (lambda nil
        (if
            (fboundp 'org-make-toc)
            (org-make-toc)
          (message-box "Please install org-make-toc.")))
      nil t)
     (org-edit-src-content-indentation 0)
     (eval cl-flet
      ((enhance-imenu-lisp
        (&rest keywords)
        (dolist
            (keyword keywords)
          (let
              ((prefix
                (when
                    (listp keyword)
                  (cl-second keyword)))
               (keyword
                (if
                    (listp keyword)
                    (cl-first keyword)
                  keyword)))
            (add-to-list 'lisp-imenu-generic-expression
                         (list
                          (purecopy
                           (concat
                            (capitalize keyword)
                            (if
                                (string=
                                 (substring-no-properties keyword -1)
                                 "s")
                                "es" "s")))
                          (purecopy
                           (concat "^\\s-*("
                                   (regexp-opt
                                    (list
                                     (if prefix
                                         (concat prefix "-" keyword)
                                       keyword)
                                     (concat prefix "-" keyword))
                                    t)
                                   "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                          2))))))
      (enhance-imenu-lisp
       '("bookmarklet-command" "define")
       '("class" "define")
       '("command" "define")
       '("ffi-method" "define")
       '("ffi-generic" "define")
       '("function" "define")
       '("internal-page-command" "define")
       '("internal-page-command-global" "define")
       '("mode" "define")
       '("parenscript" "define")
       "defpsmacro"))))
 '(send-mail-function 'mailclient-send-it)
 '(session-use-package t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(warning-suppress-log-types
   '((doom-after-init-hook)
     (doom-after-init-hook)
     (emacs)
     (defvaralias)))
 '(warning-suppress-types '((doom-after-init-hook) (emacs) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "white" :background "red" :weight bold :height 2.5 :box (:line-width 10 :color "red")))))
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
