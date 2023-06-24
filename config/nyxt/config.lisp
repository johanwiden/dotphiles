(in-package #:nyxt-user)

;; (defvar *my-keymap* (make-keymap "my-map"))
;; (define-key *my-keymap*
;;   "C-M-j" 'nyxt/web-mode:follow-hint-new-buffer-focus)

;; (define-mode my-mode ()
;;   "Dummy mode for the custom key bindings in `*my-keymap*'."
;;   ((keymap-scheme (keymap:make-scheme
;;                    scheme:cua *my-keymap*
;;                    scheme:emacs *my-keymap*
;;                    scheme:vi-normal *my-keymap*))))

;; (define-configuration browser
;;   ((external-editor-program '("/usr/local/bin/emacsclient"))))
;; (define-configuration browser
;;   ((session-restore-prompt :never-restore)))

;; (define-configuration (buffer web-buffer)
;;   ((default-modes (append '(my-mode) %slot-default%))))

;; (load "~/quicklisp/setup.lisp")
;;; SLY setup
;; (ql:quickload :slynk)
;; (define-command-global start1-slynk (slynk-port)
;;   (slynk:create-server :port slynk-port :dont-close t :interface "0.0.0.0")
;;   (echo "Slynk-server started at port ~a" slynk-port))

;; (start-slynk 1984)
;; Default port is 4006, nyxt and emacs config must agree on port number.
(start-slynk)

;; aartaka config
(let ((*default-pathname-defaults* (uiop:pathname-directory-pathname (files:expand *config-file*))))
  (load "init"))
