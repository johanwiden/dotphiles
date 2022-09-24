;;; emacs-with-nyxt.el --- some code to run Nyxt via Emacs.

(defun emacs-with-nyxt-sly-connect (host port)
  "Connect Sly to HOST and PORT ignoring version mismatches."
  (sly-connect host port)
  (sleep-for 1))

(defvar emacs-with-nyxt-sly-nyxt-delay 0.3)
(defun emacs-with-nyxt-start-and-connect-to-nyxt (&optional no-maximize)
  "Start Nyxt with swank capabilities. Optionally skip window maximization with NO-MAXIMIZE."
  (interactive)
  (async-shell-command (format "nyxt -e \"(nyxt-user::start-slynk)\""))
  (while (not (ignore-errors (not (emacs-with-nyxt-sly-connect "localhost" "4006"))))
    (message "Starting Swank connection...")
    (sleep-for emacs-with-nyxt-sly-nyxt-delay))
  (sleep-for 5)
  (emacs-with-nyxt-sly-repl-send-sexps
   `(load "~/quicklisp/setup.lisp"))
  (emacs-with-nyxt-sly-repl-send-sexps
   `(defun replace-all (string part replacement &key (test #'char=))
      "Return a new string in which all the occurences of the part is replaced with replacement."
      (with-output-to-string (out)
			     (loop with part-length = (length part)
				   for old-pos = 0 then (+ pos part-length)
				   for pos = (search part string
						     :start2 old-pos
						     :test test)
				   do (write-string string out
						    :start old-pos
						    :end (or pos (length string)))
				   when pos do (write-string replacement out)
				   while pos))))

  (emacs-with-nyxt-sly-repl-send-sexps
   `(defun eval-in-emacs (&rest s-exps)
      "Evaluate S-EXPS with emacsclient."
      (let ((s-exps-string (replace-all
			    (write-to-string
			     `(progn ,@s-exps) :case :downcase)
			    ;; Discard the package prefix.
			    "nyxt::" "")))
	(format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
	(uiop:run-program
	 (list "emacsclient" "--eval" s-exps-string) :output :string)))))

(defun browse-url-nyxt (url &optional new-window)
  "Browse URL with Nyxt. NEW-WINDOW is ignored."
  (interactive "sURL: ")
  (unless (sly-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-browse-url-nyxt url url))

(defun emacs-with-nyxt-browse-url-nyxt (url &optional buffer-title)
  "Open URL with Nyxt and optionally define BUFFER-TITLE."
  (interactive "sURL: ")
  (emacs-with-nyxt-sly-repl-send-sexps
   (cl-concatenate
    'list
    (list
     'buffer-load
     url)
    (if buffer-title
	(progn
	  `(:buffer (make-buffer :title ,buffer-title))
	  nil))))
  (unless buffer-title
    (midraal/nyxt-switch-buffer buffer-title)))

(defun emacs-with-nyxt-search-first-in-nyxt-current-buffer (string)
  "Search current Nyxt buffer for STRING."
  (interactive "sString to search: ")
  (unless (sly-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-sly-repl-send-sexps
   `(nyxt/web-mode::highlight-selected-hint
     :link-hint
     (car (nyxt/web-mode::matches-from-json
	   (nyxt/web-mode::query-buffer :query ,string)))
     :scroll 't)))

(defun emacs-with-nyxt-sly-repl-send-sexps (&rest s-exps)
  "Evaluate S-EXPS with Nyxt Sly session."
  (let ((s-exps-string (s-join "" (--map (prin1-to-string it) s-exps))))
    (if (sly-connected-p)
	(cdr (sly-eval `(slynk:eval-and-grab-output ,s-exps-string)))
      (error "Sly is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first"))))

(defun midraal/get-nyxt-buffers ()
  (when (sly-connected-p)
    (read
     (emacs-with-nyxt-sly-repl-send-sexps
      '(map 'list (lambda (el) (slot-value el 'title)) (buffer-list))))))

(defun midraal/nyxt-switch-buffer (&optional title)
  (interactive)
  (if (sly-connected-p)
      (let ((title (or title (completing-read "Title: " (midraal/get-nyxt-buffers)))))
	(emacs-with-nyxt-sly-repl-send-sexps
	 `(switch-buffer :id (slot-value (find-if #'(lambda (el) (equal (slot-value el 'title) ,title)) (buffer-list)) 'id))))
    (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide))))

(defun midraal/get-nyxt-commands ()
  (when (sly-connected-p)
    (read
     (emacs-with-nyxt-sly-repl-send-sexps
      `(let ((commands (make-instance 'command-source)))
	 (map 'list (lambda (el) (slot-value el 'name)) (funcall (slot-value commands 'prompter:CONSTRUCTOR) commands)))))))

(defun midraal/nyxt-run-command (&optional command)
  (interactive)
  (if (sly-connected-p)
      (let ((command (or command (completing-read "Execute command: " (midraal/get-nyxt-commands)))))
	(emacs-with-nyxt-sly-repl-send-sexps `(nyxt::run-async ',(read command))))
    (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide))))

(defun midraal/nyxt-take-over-prompt ()
  (interactive)
  (emacs-with-nyxt-sly-repl-send-sexps
   `(progn
      (defun flatten (structure)
        (cond ((null structure) nil)
              ((atom structure) (list structure))
              (t (mapcan #'flatten structure))))
      (defun prompt (&REST args)
        (flet ((ensure-sources (specifiers)
                               (mapcar (lambda (source-specifier)
                                         (cond
                                          ((and (symbolp source-specifier)
                                                (c2cl:subclassp source-specifier 'source))
                                           (make-instance source-specifier))
                                          (t source-specifier)))
                                       (uiop:ensure-list specifiers))))
              (sleep 0.1)
              (let* ((promptstring (list (getf args :prompt)))
                     (sources (ensure-sources (getf args :sources)))
                     (names (mapcar (lambda (ol) (slot-value ol 'prompter:attributes)) (flatten (mapcar (lambda (el) (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS)) sources))))
                     (completed (read-from-string (eval-in-emacs `(midraal/nyxt-complete ',promptstring ',names))))
                     (suggestion
                      (find-if (lambda (el) (equal completed (slot-value el 'PROMPTER::ATTRIBUTES))) (flatten (mapcar (lambda (el) (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS)) sources))))
                     (selected-class (find-if (lambda (el) (find suggestion (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS))) sources)))
                (if selected-class
                    (funcall (car (slot-value selected-class 'PROMPTER::ACTIONS)) (list (slot-value suggestion 'PROMPTER:VALUE)))
                  (funcall (car (slot-value (car sources) 'PROMPTER::ACTIONS)) (list completed)))))))))

(defun midraal/nyxt-complete (prompt names)
  (let* ((completions (--map (s-join "\t" (--map (s-join ": " it) it)) names))
         (completed-string (completing-read (s-append ": " (car prompt)) completions))
         (completed-index (-elem-index  completed-string completions)))
    (if (numberp completed-index)
        (nth completed-index names)
      completed-string)))


(with-eval-after-load 'consult
  (consult--define-state nyxt)
  (defvar nyxt-buffer-source
    `(:name "Nyxt"
	    :hidden t
	    :narrow ?n
	    :category browser
	    :items ,#'midraal/get-nyxt-buffers
	    :action ,#'midraal/nyxt-switch-buffer))
  (add-to-list 'consult-buffer-sources 'nyxt-buffer-source 'append))

(setq browse-url-browser-function 'browse-url-nyxt)
