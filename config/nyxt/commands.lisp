(in-package #:nyxt-user)

;; (defvar *debug-autofill*
;;   (make-instance
;;    #+nyxt-2 'nyxt:autofill
;;    #+nyxt-3 'nyxt/autofill-mode:autofill
;;    :name "Debug"
;;    :fill (lambda ()
;;            (#+(and nyxt-3 (not nyxt-3-pre-release-1)) nyxt:ps-eval
;;             #-(and nyxt-3 (not nyxt-3-pre-release-1)) nyxt:peval
;;              (setf (ps:@ document active-element value) ""))
;;            (format
;;             nil "**Describe the bug**

;; **Precise recipe to reproduce the issue**

;; For website-specific issues:
;; Can you reproduce this issue with Epiphany / GNOME Web (https://wiki.gnome.org/Apps/Web)?

;; **Information**
;; - OS name+version: GuixSD
;; ```sh
;; $ guix describe
;; ~a
;; ```
;; - Graphics card and driver: Intel UHD 620, `i915`
;; ``` sh
;; $ lspci -v
;; ...
;; 00:02.0 VGA compatible controller: Intel Corporation UHD Graphics (rev 01) (prog-if 00 [VGA controller])
;; 	Subsystem: Lenovo Device 5089
;; 	Flags: bus master, fast devsel, latency 0, IRQ 165
;; 	Memory at 601c000000 (64-bit, non-prefetchable) [size=16M]
;; 	Memory at 4000000000 (64-bit, prefetchable) [size=256M]
;; 	I/O ports at 3000 [size=64]
;; 	Expansion ROM at 000c0000 [virtual] [disabled] [size=128K]
;; 	Capabilities: <access denied>
;; 	Kernel driver in use: i915
;; 	Kernel modules: i915
;; ...
;; ```
;; - Desktop environment / Window manager name+version: StumpWM 20.11
;; - How you installed Nyxt (Guix pack, package manager, build from source): `guix package -f nyxt.scm`
;; - Information from `show-system-information`:
;; ```
;; ~a
;; ```

;; **Output when started from a shell** "
;;             (uiop:run-program "guix describe"
;;                               :output '(:string :stripped t))
;;             (nyxt::system-information)))))

;; (nyxt::define-command-global report-bug ()
;;   "Report the bug on Nyxt GitHub, filling all the necessary information in the process."
;;   (let* ((title (prompt1
;;                   :prompt "Title of the issue"
;;                   :sources (list (make-instance 'prompter:raw-source))))
;;          (buffer (make-buffer-focus
;;                   :url (quri:uri (format nil "https://github.com/atlas-engineer/nyxt/issues/new?&template=bug_report.md&title=~a"
;;                                          title)))))
;;     (hooks:once-on (buffer-loaded-hook buffer)
;;         (buffer)
;;       (#+(and nyxt-3 (not nyxt-3-pre-release-1)) nyxt:ps-eval
;;        #-(and nyxt-3 (not nyxt-3-pre-release-1)) nyxt:peval
;;         (ps:chain (nyxt/ps:qs document "#issue_body") (focus)))
;;       (ffi-buffer-paste buffer (funcall (nyxt/autofill-mode:autofill-fill *debug-autofill*))))))

;; (nyxt:define-command-global new-feature-request ()
;;   "Open a new feature request in Nyxt repo."
;;   (let* ((title (prompt1
;;                  :prompt "Title of the issue"
;;                  :sources (list (make-instance 'prompter:raw-source))))
;;          (buffer (make-buffer-focus
;;                   :url (quri:uri (format nil "https://github.com/atlas-engineer/nyxt/issues/new?assignees=&labels=feature&template=feature_request.md&title=~a"
;;                                          title)))))
;;     (hooks:once-on (buffer-loaded-hook buffer)
;;         (buffer)
;;       (#+(and nyxt-3 (not nyxt-3-pre-release-1)) nyxt:ps-eval
;;        #-(and nyxt-3 (not nyxt-3-pre-release-1)) nyxt:peval
;;         (ps:chain (nyxt/ps:qs document "#issue_body") (focus))))))

;; #+(and nyxt-3 (not nyxt-3-pre-release-1))
;; (nyxt/bookmarklets-mode:define-bookmarklet-command-global post-to-hn
;;   "Post the link you're currently on to Hacker News"
;;   "window.location=\"https://news.ycombinator.com/submitlink?u=\" + encodeURIComponent(document.location) + \"&t=\" + encodeURIComponent(document.title)")

(define-command-global open-in-nosave-buffer ()
  "Make a new nosave buffer with URL at point."
  (let ((url (url-at-point (current-buffer))))
    (make-nosave-buffer :url url)))

#+(and nyxt-3 (not nyxt-3-pre-release-1))
(ffi-add-context-menu-command
 (lambda ()
   (when (url-at-point (current-buffer))
     (make-nosave-buffer :url (url-at-point (current-buffer)))))
 "Open Link in New Nosave Buffer")

#+(and nyxt-gtk nyxt-3)
(define-command-global make-new-buffer-with-url-and-context ()
  "Make a new buffer with a user-chosen context and a URL under pointer."
  (#-(and nyxt-3 (not (or nyxt-3-pre-release-2 nyxt-3-pre-release-1)))
   make-buffer-with-context
   #+(and nyxt-3 (not (or nyxt-3-pre-release-2 nyxt-3-pre-release-1)))
   nyxt/renderer/gtk:make-buffer-with-context
   :url (url-at-point (current-buffer))))

#+(and nyxt-gtk nyxt-3 (not nyxt-3-pre-release-1))
(ffi-add-context-menu-command
 'make-new-buffer-with-url-and-context
 "Open Link in New Buffer with Context")

#+nyxt-3
(define-panel-command-global search-translate-selection (&key (selection (ffi-buffer-copy (current-buffer))))
    (panel "*Translate panel*" :right)
  "Open the translation of the selected word in a panel buffer."
  (setf
   #-(and nyxt-3 (not (or nyxt-3-pre-release-2 nyxt-3-pre-release-1)))
   (ffi-window-panel-buffer-width (current-window) panel)
   #+(and nyxt-3 (not (or nyxt-3-pre-release-2 nyxt-3-pre-release-1)))
   (ffi-width panel) 550)
  (run-thread "search translation URL loader"
    (sleep 0.3)
    (buffer-load (quri:uri (format nil (nyxt::search-url (nyxt::default-search-engine))
                                   (str:concat "translate " (ffi-buffer-copy (current-buffer)))))
                 :buffer panel))
  "")

#+(and nyxt-3 (not nyxt-3-pre-release-1))
(ffi-add-context-menu-command
 'search-translate-selection
 "Translate Selection")

(define-command-global add-autofill ()
  "Add an autofill with the selected text to the list of `autofill-mode' autofills."
  (push (make-instance 'nyxt/mode/autofill:autofill
                       :name (prompt1 :prompt "Autofill key" :sources 'prompter:raw-source)
                       :fill (ffi-buffer-copy (current-buffer)))
        (nyxt/mode/autofill::autofills (current-mode :autofill))))

(ffi-add-context-menu-command
 'add-autofill
 "Add Temporary Autofill")

(ffi-add-context-menu-command
 (lambda ()
   (let ((url (url-at-point (current-buffer))))
     (nyxt/mode/bookmark:bookmark-add
      url :title (fetch-url-title url))))
 "Bookmark this URL")

(defmethod nyxt:value->html :around ((value string) &optional compact-p)
  (declare (ignorable compact-p))
  (if (html-string-p value)
      (spinneret:with-html-string
        (:label
         (:raw (call-next-method))
         (:br)
         (:raw value)))
      (call-next-method)))
