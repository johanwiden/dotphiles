;;; -*-  mode: lisp; -*-

(redirect-all-output (data-dir-file "debug-output" "txt"))

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "slynk")
(ql:quickload "stumpwm")
(in-package :stumpwm)
;;(run-shell-command "sh ~/.xprofile")


;;; Helpers
(defun tr-define-key (key command)
  (define-key *top-map* (kbd (concat "s-" key )) command)
  (define-key *root-map* (kbd key) command))

(defun file-readable-p (file)
  "Return t, if FILE is available for reading."
  (handler-case
      (with-open-file (f file)
        (read-line f))
    (stream-error () nil)))

(defun executable-p (name)
  "Tell if given executable is present in PATH."
  (let ((which-out (string-trim '(#\  #\linefeed) (run-shell-command (concat "which " name) t))))
    (unless (string-equal "" which-out) which-out)))

;; (run-shell-command "xmodmap -e 'clear mod4'" t)
;; (run-shell-command "xmodmap -e 'add mod4 = Super_L'" t)
;; (set-prefix-key (kbd "Super_L"))
;; (run-shell-command "xmodmap -e 'clear mod4'" t)
;; (run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
;; (set-prefix-key (kbd "F20"))

;;; Undo And Redo Functionality
(load-module "winner-mode")
(define-key *root-map* (kbd "u") "winner-undo")
(define-key *root-map* (kbd "C-r") "winner-redo")
(add-hook *post-command-hook* (lambda (command)
                                (when (member command winner-mode:*default-commands*)
                                  (winner-mode:dump-group-to-file))))

;;; Emacs integration
(defcommand emacs () () ; override default emacs command
  "Start emacs if emacsclient is not running and focus emacs if it is
running in the current group"
  (run-or-raise "emacsclient -c -a 'emacs'" '(:class "Emacs")))
;; Treat emacs splits like Xorg windows
(defun is-emacs-p (win)
  "nil if the WIN"
  (when win
    (string-equal (window-class win) "Emacs")))

(defmacro exec-el (expression)
  "execute emacs lisp do not collect it's output"
  `(eval-string-as-el (write-to-string ',expression)))

(defun eval-string-as-el (elisp &optional collect-output-p)
  "evaluate a string as emacs lisp"
  (let ((result (run-shell-command
                 (format nil "timeout --signal=9 1m emacsclient --eval \"~a\""
                         elisp)
                 collect-output-p)))
    (handler-case (read-from-string result)
      ;; Pass back a string when we can't read from the string
      (error () result))))

(defmacro eval-el (expression)
  "evaluate emacs lisp and collect it's output"
  `(eval-string-as-el ,(write-to-string expression :case :downcase) t))

(defun emacs-winmove (direction)
  "executes the emacs function winmove-DIRECTION where DIRECTION is a string"
  (eval-string-as-el (concat "(windmove-" direction ")") t))

;; Used for warping the cursor
(load-module "beckon")
(define-key *root-map* (kbd "B") "beckon")

;; Graceful logout, suspend, shutdown, reboot
(load-module "end-session")

(defun better-move-focus (ogdir)
  "Similar to move-focus but also treats emacs windows as Xorg windows"
  (declare (type (member :up :down :left :right) ogdir))
  (flet ((mv () (progn (move-focus ogdir)
                       ;; Make sure there is even a window to move the
                       ;; mouse to
                       (when (current-window)
                         (beckon:beckon)))))
    (if (is-emacs-p (current-window))
        (when ;; There is not emacs window in that direction
            (= (length (emacs-winmove (string-downcase (string ogdir))))
               1)
          (mv))
        (mv))))

(defcommand my-mv (dir) ((:direction "Enter direction: "))
  (when dir (better-move-focus dir)))

(define-key *top-map* (kbd "s-h") "my-mv left")
(define-key *top-map* (kbd "s-j") "my-mv down")
(define-key *top-map* (kbd "s-k") "my-mv up")
(define-key *top-map* (kbd "s-l") "my-mv right")

;;; SLY setup
;;(defvar *slynk-port* slynk::default-server-port)
(defvar *slynk-port* 4047)
(defparameter *stumpwm-slynk-session* nil)

(defcommand start-slynk (&optional (port *slynk-port*)) ()
  (handler-case
      (defparameter *stumpwm-slynk-session*
        (slynk:create-server
         :dont-close t
         :port port))
    (error (c)
      (format *error-output* "Error starting slynk: ~a~%" c)
      )))

(defcommand restart-slynk () ()
  "Restart Slynk and reload source.
This is needed if Sly updates while StumpWM is running"
  (stop-slynk)
  (start-slynk))

(defcommand stop-slynk () ()
  "Restart Slynk and reload source.
This is needed if Sly updates while StumpWM is running"
  (slynk:stop-server *slynk-port*))

;; Not working, because sly-connect reads from minibuffer
;; Instead use my/connect-to-stumpwm, defined in my emacs config.org
(defcommand connect-to-sly () ()
  (unless *stumpwm-slynk-session*
    (start-slynk))
;;  (exec-el (sly-connect "localhost" *slynk-port*))
;;  (exec-el (sly-connect "localhost" 4047))
  (eval-string-as-el "(sly-connect \"localhost\" 4047)" t)
  (emacs))

;; (defcommand slynk (port) ((:string "Port number: "))
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (slynk:create-server :port (parse-integer port) :dont-close t))
;;    :name "slynk-manual"))

;; Focus Follow Mouse
(setf *mouse-focus-policy* :sloppy)

;; Remember commands and offers orderless completion
;; https://github.com/landakram/stumpwm-prescient
(ql:quickload :stumpwm-prescient)
(setf *input-refine-candidates-fn* 'stumpwm-prescient:refine-input)


;; General Top Level Bindings
(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")
(define-key *top-map* (kbd "s-TAB") "fnext")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "fprev")

(tr-define-key "f" "fullscreen")
(tr-define-key "q" "only")
(tr-define-key "d" "exec dmenu_run")
(tr-define-key "RET" "exec emacsclient -c -a 'emacs'")


;; Don't jump between groups when switching apps
(setf *run-or-raise-all-groups* nil)
(define-key *groups-map* (kbd "=") "change-default-split-ratio 1/2")
(define-key *groups-map* (kbd "l") "change-default-layout")
(define-key *groups-map* (kbd "d") "gnew-dynamic")
(define-key *groups-map* (kbd "s") "gselect")

(load-module "globalwindows")
(define-key *groups-map* (kbd "b") "global-pull-windowlist")

;;;; Hide and Show Windows
(defun window-menu-format (w)
  (list (format-expand *window-formatters* *window-format* w) w))

(defun window-from-menu (windows)
  (when windows
    (second (select-from-menu
             (group-screen (window-group (car windows)))
             (mapcar 'window-menu-format windows)
             "Select Window: "))))

(defun windows-in-group (group)
  (group-windows (find group (screen-groups (current-screen))
                       :key 'group-name :test 'equal)))

(defcommand pull-from-trash () ()
  (let* ((windows (windows-in-group ".trash"))
         (window  (window-from-menu windows)))
    (when window
      (move-window-to-group window (current-group))
      (stumpwm::pull-window window))))

(defcommand move-to-trash () ()
  (stumpwm:run-commands "gmove .trash"))

(tr-define-key "]" "move-to-trash")
(tr-define-key "[" "pull-from-trash")
