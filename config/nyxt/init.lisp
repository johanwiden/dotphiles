(in-package #:nyxt-user)

;;; Reset ASDF registries to allow loading Lisp systems from
;;; everywhere.
;; #+nyxt-3 (reset-asdf-registries)

;;; Load quicklisp. Not sure it works.
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *web-buffer-modes*
  '(;; :emacs-mode
    :vi-normal-mode
    ;; :blocker-mode
    :force-https-mode
    ;; :reduce-tracking-mode
    #+nyxt-3 :user-script-mode
    #+nyxt-3 :bookmarklets-mode)
  "The modes to enable in web-buffer by default.
Extension files (like dark-reader.lisp) are to append to this list.

Why the variable? Because it's too much hassle copying it everywhere.")

;;; Loading files from the same directory.
;;; Can be done individually per file, dolist is there to simplify it.
;; #+nyxt-3
;; (define-nyxt-user-system-and-load nyxt-user/basic-config
;;   :components ("keybinds" "passwd" "status" "commands" "hsplit" "style" "unpdf"))
(define-nyxt-user-system-and-load nyxt-user/basic-config
  :components ("keybinds" "passwd" "commands" "hsplit" "style" "unpdf"))

;;; Loading extensions and third-party-dependent configs. See the
;;; matching files for where to find those extensions.
;;;
;;; Usually, though, it boils down to cloning a git repository into
;;; your `*extensions-path*' (usually ~/.local/share/nyxt/extensions)
;;; and adding a `load-after-system' (Nyxt 2) /
;;; `define-nyxt-user-system-and-load' (Nyxt 3) line mentioning a
;;; config file for this extension.
(defmacro load-after-system* (system &optional file)
  "Helper macro to load configuration for extensions.
Loads a newly-generated ASDF system depending on SYSTEM.
FILE, if provided, is loaded after the generated system successfully
loads."
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) ,@(when file
                               `(:components (,file)))))

(load-after-system* :nx-search-engines "search-engines")
;; (load-after-system* :nx-kaomoji "kaomoji")
;; (load-after-system* :nx-ace "ace.lisp")
;; (load-after-system* :slynk "slynk")
;; (load-after-system* :nx-fruit)
;; (load-after-system* :nx-freestance-handler "freestance")
#+nyxt-3 (load-after-system* :nx-dark-reader "dark-reader")
;; #+nyxt-3 (load-after-system* :nx-tailor "tailor")

;; Turn the Nyxt-native debugging on. Only works in Nyxt 3.
;; #+nyxt-3 (toggle-debug-on-error t)

;; (flet ((construct-autofill (&rest args)
;;          (apply #+nyxt-2 #'make-autofill
;;                 #+nyxt-3 #'nyxt/autofill-mode:make-autofill
;;                 args)))
;;   (defvar *autofills*
;;     (list (construct-autofill :name "Crunch" :fill "Ну что, кранчим сегодня в Дискорде?")
;;           *debug-autofill*)))

;; Basic modes setup for web-buffer.
(define-configuration web-buffer
  ((default-modes `(,@*web-buffer-modes*
                    ,@%slot-value%))))

;;; Set new buffer URL (a.k.a. start page, new tab page).
;;; It does not change the first buffer opened if you're on 2.*.
(define-configuration browser
  ((remote-execution-p t)
   (default-new-buffer-url (quri:uri "https://lispcookbook.github.io/cl-cookbook/"))
   (external-editor-program
    (list "emacsclient" "-cn" "-a" ""))))
;; (define-configuration browser
;;   (
;;    ;; Whether code sent to the socket gets executed.  You must understand the
;;    ;; risks before enabling this: a privileged user with access to your system
;;    ;; can then take control of the browser and execute arbitrary code under your
;;    ;; user profile.
;;    (remote-execution-p t)))

;;; Enable proxy in nosave (private, incognito) buffers.
(define-configuration nosave-buffer
  ((default-modes `(:proxy-mode
                    ,@*web-buffer-modes*
                    ,@%slot-value%))))

;;; Set up QWERTY home row as the hint keys.
(define-configuration :hint-mode
  ((hints-alphabet "DSJKHLFAGNMXCWEIO")))

;;; This makes auto-mode to prompt me about remembering this or that
;;; mode when I toggle it.
(define-configuration modable-buffer
  ((prompt-on-mode-toggle-p t)))

;;; Setting WebKit-specific settings. Not exactly the best way to
;;; configure Nyxt. See
;;; https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html
;;; for the full list of settings you can tweak this way.
(defmethod ffi-buffer-make :after ((buffer nyxt/renderer/gtk::gtk-buffer))
  (when (slot-boundp
         buffer #+nyxt-3 'nyxt/renderer/gtk::gtk-object
         #+nyxt-2 'nyxt::gtk-object)
      (let* ((settings (webkit:webkit-web-view-get-settings
                        (nyxt/renderer/gtk::gtk-object buffer))))
        (setf
         ;; Resizeable textareas. It's not perfect, but still a cool feature to have.
         (webkit:webkit-settings-enable-resizable-text-areas settings) t
         ;; Write console errors/warnings to the shell, to ease debugging.
         (webkit:webkit-settings-enable-write-console-messages-to-stdout settings) t
         ;; "Inspect element" context menu option available at any moment.
         (webkit:webkit-settings-enable-developer-extras settings) t
         ;; Enable WebRTC.
         (webkit:webkit-settings-enable-media-stream settings) t
         ;; Use Cantarell-18 as the default font.
         (webkit:webkit-settings-default-font-family settings) "Iosevka Comfy Duo"
         (webkit:webkit-settings-default-font-size settings) 16
         ;; Use Hack-17 as the monospace font.
         (webkit:webkit-settings-monospace-font-family settings) "Iosevka Comfy Fixed"
         (webkit:webkit-settings-default-monospace-font-size settings) 16
         ;; Use Unifont for pictograms.
         (webkit:webkit-settings-pictograph-font-family settings) "Unifont")))
  ;; Set the view background to black.
  (cffi:foreign-funcall
   "webkit_web_view_set_background_color"
   :pointer (g:pointer (nyxt/renderer/gtk:gtk-object buffer))
   ;; GdkRgba is simply an array of four doubles.
   :pointer (cffi:foreign-alloc
             :double
             :count 4
             ;; red green blue alpha
             :initial-contents '(0d0 0d0 0d0 1d0))))
