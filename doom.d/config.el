;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Johan Widén"
      user-mail-address "j.e.widen@gmail.com")

(setq search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited)

(require 'modus-themes)                 ; common code
(require 'modus-operandi-theme)         ; light theme
(require 'modus-vivendi-theme)          ; dark theme
(setq modus-themes-variable-pitch-headings t)
(setq modus-themes-scale-headings t)
(setq doom-theme 'modus-vivendi)

(setq doom-font (font-spec :family "Ubuntu Mono" :size 18)
      ;; doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 15)
      ;;doom-variable-pitch-font (font-spec :family "FiraGO" :size 15)
      ;;doom-variable-pitch-font (font-spec :family "Libre Baskerville" :height 1.0)
      ;;doom-serif-font (font-spec :family "Libre Baskerville" :height 1.0)
      )
(set-face-attribute 'default nil :font "Ubuntu Mono-18")
;;(set-face-attribute 'default nil :font "Iosevka-16")
(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
;;(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "Overpass" :height 1.0)
;;(set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)
;;(set-face-attribute 'variable-pitch nil :family "Libre Baskerville" :height 1.0)
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(defvar jw/paradox-github-token nil)

(let ((secret.el (expand-file-name ".secret.el" "~")))
  (when (file-exists-p secret.el)
    (load secret.el)))

(server-start)

(setq-default
 help-window-select t             ; Focus new help windows when opened
 ;;debug-on-error t
 ;;jit-lock-defer-time 0
 ;;fast-but-imprecise-scrolling t ; Set by doom
 ;;sentence-end-double-space nil    ; End a sentence after a dot and a space. Set by doom
 window-combination-resize t      ; Resize windows proportionally
 history-delete-duplicates t
 )

(global-auto-revert-mode t)

(setq org-directory "~/org/")
(setq org-use-speed-commands t)

(require 'find-lisp)

(setq jethro/org-agenda-directory (file-truename "~/org-files/"))
(setq org-agenda-files
      (find-lisp-find-files jethro/org-agenda-directory "\.org$"))

(use-package! org-agenda
  :init
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun jethro/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :bind (:map org-agenda-mode-map
              ("i" . org-agenda-clock-in)
              ("r" . jethro/org-process-inbox)
              ("R" . org-agenda-refile)
              ("c" . jethro/org-inbox-capture))
  :config
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "To Refile")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "inbox.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "emails.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "someday.org")
                                                                  ,(concat jethro/org-agenda-directory "projects.org")
                                                                  ,(concat jethro/org-agenda-directory "next.org")))
                                              ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Projects")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")))
                                              ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(defun jethro/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
           "* TODO %?")
          ("e" "email" entry (file+headline ,(concat jethro/org-agenda-directory "emails.org") "Emails")
               "* TODO [#A] Reply: %a :@home:@school:"
               :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
               "* TODO [[%:link][%:description]]\n\n %i"
               :immediate-finish t)
          ("w" "Weekly Review" entry (file+olp+datetree ,(concat jethro/org-agenda-directory "reviews.org"))
           (file ,(concat jethro/org-agenda-directory "templates/weekly_review.org")))
          ("r" "Reading" todo ""
               ((org-agenda-files '(,(concat jethro/org-agenda-directory "reading.org")))))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            (:newline)
                            ("CANCELLED" . ?c))))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:level . 1))))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the EFFORT property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the 'org-agenda'."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun jethro/org-inbox-capture ()
  "Capture a task in agenda mode."
  (interactive)
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(defun jethro/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO."
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)

(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))

(use-package! org-journal
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "~/org/roam/"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d"))

(after! org
  (use-package! org-pdfview
    :config
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))
  )

(after! org
  (require 'ob-emacs-lisp)
  (require 'ob-ledger)
  (require 'ob-python)
  (require 'ob-shell)
  (require 'ob-core)
  (require 'ob-tangle)
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (ledger . t)
                                   (python . t)
                                   (shell . t)  ; in my case /bin/bash
)))

(setq org-babel-python-command "python3")

(after! org
  (require 'ox-gfm nil t))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/org/roam/")
        org-roam-db-location (file-truename "~/org/roam/org-roam.db")
        org-roam-graph-exclude-matcher "private"
        org-roam-completion-system 'helm
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t)))
  ;;(set-company-backend! 'org-mode '(company-capf))
  )

(use-package! org-roam-protocol
  :after org-protocol)

(after! (org ox-hugo)
  (defun jethro/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'jethro/conditional-hugo-enable))

  (setq reftex-default-bibliography '("~/org/roam/biblio/references.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/org/roam/bibli/notes.org"
        org-ref-default-bibliography '("~/org/roam/biblio/references.bib")
        org-ref-pdf-directory "~/org/roam/pdfs/")

(setq org-noter-always-create-frame nil
      org-noter-notes-search-path '("~/org/roam/org-noter"))

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "lit/${slug}"
           :head ,(concat
                   "#+setupfile: ./hugo_setup.org\n"
                   "#+title: ${=key=}: ${title}\n"
                   "#+roam_key: ${ref}\n\n"
                   "* ${title}\n"
                   "  :PROPERTIES:\n"
                   "  :Custom_ID: ${=key=}\n"
                   "  :URL: ${url}\n"
                   "  :AUTHOR: ${author-or-editor}\n"
                   "  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                   "  :NOTER_PAGE: \n"
                   "  :END:\n")
           :unnarrowed t))))

(use-package! org-roam-server)

(use-package! org-roam-server)

(use-package! org-recoll)

(use-package! org-recoll)

(use-package! org-similarity
  :config
  (setq org-similarity-directory org-roam-directory)
  )

(setq display-line-numbers-type nil)

(use-package! helm
  :init
  (progn
      (require 'helm-config)
      (require 'helm-grep)
      (require 'helm-projectile)
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
      (define-key helm-map (kbd "C-j")  'helm-select-action) ; list actions using C-z

      (setq
       ;; helm-net-prefer-curl t ; test if this works
       ;; helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>. Default nil, 1 is suggested
       helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
       helm-candidate-number-limit 150
       ;; you can customize helm-do-grep to execute ack-grep
       helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
       helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
       helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
       helm-ff-file-name-history-use-recentf t
       helm-ff-auto-update-initial-value t
       helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
       helm-completion-style 'helm-fuzzy
       helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                     ; useful in helm-mini that lists buffers
       helm-buffer-skip-remote-checking t
       helm-locate-fuzzy-match t
       )
      (global-set-key (kbd "C-h b b") 'helm-descbinds)

      ;; use helm to list eshell history
      (add-hook 'eshell-mode-hook
                #'(lambda ()
                    (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

      ;; show minibuffer history with Helm
      (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
      (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien)
    )
  )

(global-set-key [remap describe-mode] #'helm-describe-modes)

  (use-package! helm-proc)

  (use-package! helm-proc)

  (use-package helm-pydoc)

  (use-package helm-pydoc)

  (use-package helm-tramp)

  (use-package helm-tramp)

  (setq helm-grep-ag-command (concat "rg"
                                     " --color=never"
                                     " --smart-case"
                                     " --no-heading"
                                     " --line-number %s %s %s")
        helm-grep-file-path-style 'relative)
  (defun mu-helm-rg (directory &optional with-types)
    "Search in DIRECTORY with RG.
  With WITH-TYPES, ask for file types to search in."
    (interactive "P")
    (require 'helm-adaptive)
    (helm-grep-ag-1 (expand-file-name directory)
                    (helm-aif (and with-types
                                   (helm-grep-ag-get-types))
                        (helm-comp-read
                         "RG type: " it
                         :must-match t
                         :marked-candidates t
                         :fc-transformer 'helm-adaptive-sort
                         :buffer "*helm rg types*"))))
  (defun mu--project-root ()
    "Return the project root directory or `helm-current-directory'."
    (require 'helm-ls-git)
    (if-let (dir (helm-ls-git-root-dir))
        dir
      (helm-current-directory)))
  (defun mu-helm-project-search (&optional with-types)
    "Search in current project with RG.
  With WITH-TYPES, ask for file types to search in."
    (interactive "P")
    (mu-helm-rg (mu--project-root) with-types))

  (defun mu-helm-file-search (&optional with-types)
    "Search in `default-directory' with RG.
  With WITH-TYPES, ask for file types to search in."
    (interactive "P")
    (mu-helm-rg default-directory with-types))

(defun my/helm-insert-kill-ring ()
  "Get an entry from the kill ring and insert."
  (interactive)
  (require 'helm-ring)
  (let* ((helm-kill-ring-actions '(("Get" . identity)))
         (delete-range (when (region-active-p)
                         (cons (region-beginning) (region-end))))
         (result (helm-show-kill-ring)))
    (when result
      (deactivate-mark)
      (when delete-range
        (goto-char (car delete-range))
        (delete-char (- (cdr delete-range) (car delete-range))))
      (insert (substring-no-properties result)))))

  (use-package! exwm)
  (require 'exwm-randr)
  (defun jw/env-list (env-string)
      "Return list of strings in environment variable env-string.
  nil if empty or undefined."
      (let ((env-var (getenv env-string)))
        (if env-var
            (split-string env-var)
          nil)))
  (defun jw/env-str (env-string)
      "Return string in environment variable env-string.
  nil if empty or undefined."
      (let ((env-var (getenv env-string)))
        (if (> (length env-var) 0)
            env-var
          nil)))

    (defun jw/build-workspace-monitor-plist (list)
      (let (transformed-list first second (rev-list (reverse list)))
        (while rev-list
          (setq second (car rev-list))
          (setq first (string-to-number (car (cdr rev-list))))
          (setq transformed-list (cons first (cons second transformed-list)))
          (setq rev-list (cdr (cdr rev-list)))
          )
        transformed-list))

    (defun jw/xrandr-output-list ()
      "Return list of connected X11 screens, according to xrandr."
      (interactive)
      (let* ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
             (find-outputs
              (lambda ()
                (let (output-list)
                  (call-process "/usr/bin/xrandr" nil t nil)
                  (goto-char (point-min))
                  (while (re-search-forward xrandr-output-regexp nil 'noerror)
                    (setq output-list (cons (match-string 1) output-list))
                    (forward-line))
                  (reverse output-list))))
             (output-list (with-temp-buffer
                            (funcall find-outputs))))
         output-list))

    (setq jw/x11-screen-list (jw/env-list "X11_SCREEN_LIST"))
    (setq jw/x11-screen-order-list (jw/env-list "X11_SCREEN_ORDER_LIST"))
    (setq jw/x11-screen-mode-list (jw/env-list "X11_SCREEN_MODE_LIST"))
    (setq jw/x11-screen-rate-list (jw/env-list "X11_SCREEN_RATE_LIST"))
    (setq jw/x11-screen-disabled-list (jw/env-list "X11_SCREEN_DISABLED_LIST"))
    (setq jw/exwm-workspace-list (jw/env-list "EXWM_WORKSPACE_LIST"))
    (setq jw/x11-screen-preferred (jw/env-str "X11_SCREEN_PREFERRED"))
    (setq jw/x11-display-dpi (jw/env-str "X11_DISPLAY_DPI"))
    (let ((env-var (getenv "X11_SCREEN_USE_ALL_AVAILABLE")))
      (setq jw/x11-screen-use-all-available
            (if (and (> (length env-var) 0) (string= "yes" env-var))
                t
              nil)))

    (setq exwm-randr-workspace-monitor-plist (jw/build-workspace-monitor-plist jw/exwm-workspace-list))

    (defun jw/exwm-change-screen-hook ()
      "Execute xrandr to select and position available screens according to X11_SCREEN_* environment variables."
      (let* ((output-list (jw/xrandr-output-list))
             (available-screens (seq-intersection jw/x11-screen-list output-list))
             (available-order-screens (seq-intersection jw/x11-screen-order-list output-list))
             ;; See "--auto" in xrandr(1) and https://github.com/ch11ng/exwm/issues/529.
             (unavailable-screens (seq-difference jw/x11-screen-list output-list))
             (available-disabled-screens (seq-intersection jw/x11-screen-disabled-list output-list))
             (available-screen-modes
              (let (mode-list
                    mode screen
                    (x-screen-list jw/x11-screen-list)
                    (x-mode-list jw/x11-screen-mode-list))
                (while x-screen-list
                  (setq screen (car x-screen-list))
                  (setq x-screen-list (cdr x-screen-list))
                  (setq mode (car x-mode-list))
                  (setq x-mode-list (cdr x-mode-list))
                  (if (seq-contains available-screens screen)
                      (setq mode-list (cons mode mode-list))))
                (reverse mode-list)))
             (available-screen-rates
              (let (rate-list
                    rate screen
                    (x-screen-list jw/x11-screen-list)
                    (x-rate-list jw/x11-screen-rate-list))
                (while x-screen-list
                  (setq screen (car x-screen-list))
                  (setq x-screen-list (cdr x-screen-list))
                  (setq rate (car x-rate-list))
                  (setq x-rate-list (cdr x-rate-list))
                  (if (seq-contains available-screens screen)
                      (setq rate-list (cons rate rate-list))))
                (reverse rate-list))))
        (if available-screens
            ;; Start building xrandr command line
            (let* ((x-primary-screen
                    (if (and jw/x11-screen-preferred (seq-contains available-screens jw/x11-screen-preferred))
                        jw/x11-screen-preferred
                      (car available-screens)))
                   (screen-pos (seq-position available-screens x-primary-screen))
                   (x-primary-mode (elt available-screen-modes screen-pos))
                   (x-primary-rate (elt available-screen-rates screen-pos))
                   (xrandr-dpi-args
                    (if jw/x11-display-dpi
                        (list jw/x11-display-dpi "--dpi")))
                   (xrandr-primary-args (list x-primary-rate "--rate" x-primary-mode "--mode" "--primary" x-primary-screen "--output"))
                   screen
                   disabled-list
                   (xrandr-disabled-args
                    (progn
                      (while available-disabled-screens
                        (setq screen (car available-disabled-screens))
                        (setq available-disabled-screens (cdr available-disabled-screens))
                        (setq disabled-list (cons "--output" disabled-list))
                        (setq disabled-list (cons screen disabled-list))
                        (setq disabled-list (cons "--off" disabled-list)))
                      disabled-list))
                   (unavailable-screen-list unavailable-screens)
                   u-s-list
                   (xrandr-unavailable-screen-args
                    (progn
                      (while unavailable-screen-list
                        (setq screen (car unavailable-screen-list))
                        (setq unavailable-screen-list (cdr unavailable-screen-list))
                        (setq u-s-list (cons "--output" u-s-list))
                        (setq u-s-list (cons screen u-s-list))
                        ;; (setq u-s-list (cons "--auto" u-s-list))
                        (setq u-s-list (cons "--off" u-s-list)))
                      u-s-list))
                   (screen-list available-screens)
                   rest-list
                   (xrandr-rest-available-screen-args
                    (if jw/x11-screen-use-all-available
                         ;; Add remaining available screens, except the primary screen
                         (progn
                            (while screen-list
                               (setq screen (car screen-list))
                               (setq screen-list (cdr screen-list))
                               (if (not (string= screen x-primary-screen))
                                   (progn
                                     (setq rest-list (cons "--output" rest-list))
                                     (setq rest-list (cons screen rest-list))
                                     (setq rest-list (cons "--mode" rest-list))
                                     (setq rest-list (cons (elt available-screen-modes (seq-position available-screens screen)) rest-list))
                                     (setq rest-list (cons "--rate" rest-list))
                                     (setq rest-list (cons (elt available-screen-rates (seq-position available-screens screen)) rest-list)))))
                            rest-list)
                         ;; Disable remaining available screens, except the primary screen
                         (progn
                            (while screen-list
                               (setq screen (car screen-list))
                               (setq screen-list (cdr screen-list))
                               (if (not (string= screen x-primary-screen))
                                   (progn
                                     (setq rest-list (cons "--output" rest-list))
                                     (setq rest-list (cons screen rest-list))
                                     (setq rest-list (cons "--off" rest-list)))))
                            rest-list)))
                   (screen-order-list available-order-screens)
                   order-list
                   left-screen
                   (xrandr-screen-order-args
                    (if (and jw/x11-screen-use-all-available
                             (> (length screen-order-list) 1))
                        (progn
                           (setq left-screen (car screen-order-list))
                           (setq screen-order-list (cdr screen-order-list))
                           (while screen-order-list
                              (setq screen (car screen-order-list))
                              (setq screen-order-list (cdr screen-order-list))
                              (setq order-list (cons "--output" order-list))
                              (setq order-list (cons screen order-list))
                              (setq order-list (cons "--right-of" order-list))
                              (setq order-list (cons left-screen order-list))
                              (setq left-screen screen))
                           (reverse order-list))))
                   (xrandr-args (reverse (append xrandr-rest-available-screen-args xrandr-unavailable-screen-args
                                                 xrandr-disabled-args xrandr-primary-args xrandr-dpi-args))))
               (progn
                 (setq jw/debug-output-list output-list)
                 (setq jw/debug-xrandr-args xrandr-args)
                 (setq jw/debug-xrandr-order-args xrandr-screen-order-args)
                 (apply #'call-process
                        "/usr/bin/xrandr" nil nil nil
                        xrandr-args)
                 (if xrandr-screen-order-args
                     (apply #'call-process
                            "/usr/bin/xrandr" nil nil nil
                            xrandr-screen-order-args)))
            )
          )
        )
      )

    (add-hook 'exwm-randr-screen-change-hook 'jw/exwm-change-screen-hook)
    (exwm-randr-enable)

  (require 'ido)
  (use-package! windower)
  (require 'browse-url)
  (require 'exwm-manage)

  (defun ambrevar/call-process-to-string (program &rest args)
    "Call PROGRAM with ARGS and return output.
  See also `process-lines'."
    ;; Or equivalently:
    ;; (with-temp-buffer
    ;;   (apply 'process-file program nil t nil args)
    ;;   (buffer-string))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply 'process-file program nil t nil args))))

  ;; (defun jw/xmodmap ()
  ;;   "Execute xmodmap"
  ;;   (progn
  ;;     (remove-hook 'exwm-manage-finish-hook 'jw/xmodmap)
  ;;     (ambrevar/call-process-to-string "/usr/bin/touch" "/tmp/jw_xmodmap")
  ;;     (ambrevar/call-process-to-string "/usr/bin/xmodmap" "/home/jw/.Xmodmap.exwm")))

  (defun jw/xmodmap ()
    "Execute xmodmap"
    (progn
      ;; (remove-hook 'exwm-manage-finish-hook 'jw/xmodmap)
      (ambrevar/call-process-to-string "/home/jw/bin/set_xmodmap.sh")))

  (setq browse-url-generic-program
        (or
         (executable-find (or (getenv "BROWSER") ""))
         (when (executable-find "xdg-mime")
           (let ((desktop-browser (ambrevar/call-process-to-string "xdg-mime" "query" "default" "text/html")))
             (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
         (executable-find browse-url-chrome-program)))

  (defun my-exwm-config-setup ()
    "My modified configuration for EXWM. Based on exwm-config.el"
    (setq exwm-manage-force-tiling t)
    ;; Set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 4))
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))
    ;; Global keybindings. 0-9 bcDfFgGhHijJkKlLmoOQrRwWå !"#¤%&/()= tab f2 backspace
    (unless (get 'exwm-input-global-keys 'saved-value)
      (setq exwm-input-global-keys
            `(
              ;; (,(kbd "s-b") . exwm-workspace-switch-to-buffer)
              (,(kbd "s-b") . helm-mini) ;; list and select buffers
              (,(kbd "s-c") . helm-resume) ;; Continue in latest helm selection buffer
              (,(kbd "s-G") . helm-locate) ;; locate file, based in Linux locate command
              (,(kbd "s-g") . mu-helm-file-search) ;; Grep search in files
              (,(kbd "s-r") . helm-run-external-command) ;; Start an application, such as google-chrome
              (,(kbd "s-W") . helm-exwm-switch-browser) ;; Switch to some browser windows
              (,(kbd "s-m") . (lambda () ;; Toggle display of mode-line and minibuffer, in an EXWM window
                                (interactive)
                                (exwm-layout-toggle-mode-line)
                                (exwm-workspace-toggle-minibuffer)))
              (,(kbd "s-i") . exwm-input-toggle-keyboard) ;; Toggle between "line-mode" and "char-mode" in an EXWM window
              ;; 's-r': Reset (to line-mode).
              (,(kbd "s-R") . exwm-reset) ;; Try to reset EXWM to a sane mode. Panic key
              ;; Interactively select, and switch to, a workspace. Only works in non EXWM windows.
              (,(kbd "s-w") . exwm-workspace-switch)
              ;; 's-å': Launch application.
              ;; (,(kbd "s-å") . (lambda (command)
              ;;              (interactive (list (read-shell-command "$ ")))
              ;;              (start-process-shell-command command nil command)))
              ;; 's-N': Switch to certain a workspace.
              ,@(mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                            (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,i))))
                        (number-sequence 0 9))
              ;; 'S-s-N': Move window to, and switch to, a certain workspace.
              ,@(cl-mapcar (lambda (c n)
                             `(,(kbd (format "s-%c" c)) .
                               (lambda ()
                                 (interactive)
                                 (exwm-workspace-move-window ,n)
                                 (exwm-workspace-switch ,n))))
                           '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                           (number-sequence 0 9))

              ;; Bind "s-<f2>" to "slock", a simple X display locker.
              (,(kbd "s-<f2>") . (lambda ()
                                   (interactive)
                                   (start-process "" nil "/usr/bin/slock")))
              (,(kbd "s-h") . windmove-left)  ;; Move to window to the left of current one. Uses universal arg
              (,(kbd "s-j") . windmove-down)  ;; Move to window below current one. Uses universal arg
              (,(kbd "s-k") . windmove-up)    ;; Move to window above current one. Uses universal arg
              (,(kbd "s-l") . windmove-right) ;; Move to window to the right of current one. Uses universal arg
              ;; (,(kbd "s-f") . find-file)
              (,(kbd "s-f") . helm-find-files)
              (,(kbd "s-<tab>") . windower-switch-to-last-buffer) ;; Switch to last open buffer in current window
              (,(kbd "s-s") . windower-toggle-single) ;; Toggle between multiple windows, and a single window
              (,(kbd "s-S") . windower-toggle-split)  ;; Toggle between vertical and horizontal split. Only works with exactly two windows.
              (,(kbd "s-H") . windower-swap-left)  ;; Swap current window with the window to the left
              (,(kbd "s-J") . windower-swap-below) ;; Swap current window with the window below
              (,(kbd "s-K") . windower-swap-above) ;; Swap current window with the window above
              (,(kbd "s-L") . windower-swap-right) ;; Swap current window with the window to the right
              (,(kbd "s-F") . exwm-floating-toggle-floating) ;; Toggle the current window between floating and non-floating states
              (,(kbd "s-Q") . exwm-layout-toggle-fullscreen) ;; Toggle fullscreen mode
              (,(kbd "s-D") . kill-this-buffer)
              (,(kbd "s-<backspace>") . kill-this-buffer)
              )))
    ;; Line-editing shortcuts: abBcdefFknpqsvwx
    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
            `((,(kbd "H-b") . ,(kbd "<left>"))
              (,(kbd "H-B") . ,(kbd "C-<left>"))
              (,(kbd "H-f") . ,(kbd "<right>"))
              (,(kbd "H-F") . ,(kbd "C-<right>"))
              (,(kbd "H-p") . ,(kbd "<up>"))
              (,(kbd "H-n") . ,(kbd "<down>"))
              (,(kbd "H-a") . ,(kbd "<home>"))
              (,(kbd "H-e") . ,(kbd "<end>"))
              ;; q and w are convenient if Caps Lock key is Hyper key
              (,(kbd "H-q") . ,(kbd "<prior>"))
              (,(kbd "H-w") . ,(kbd "<next>"))
              (,(kbd "H-d") . ,(kbd "<delete>"))
              (,(kbd "H-k") . ,(kbd "S-<end> <delete>"))
              ;; cut/paste.
              (,(kbd "H-x") . ,(kbd "C-x"))
              (,(kbd "H-c") . ,(kbd "C-c"))
              (,(kbd "H-v") . ,(kbd "C-v"))
              ;; search
              (,(kbd "H-s") . ,(kbd "C-f"))
              )))
    ;; Default is save-buffers-kill-terminal, but that may kill daemon before its finished
    (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
    (add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)
    ;; Ensure that EXWM input mode is displayed in mode line
    (add-hook 'exwm-input--input-mode-change-hook
              'force-mode-line-update)
    ;; Called once, to configure X11 keyboard layout
    (add-hook 'exwm-manage-finish-hook
              'jw/xmodmap t)
    ;; Allow resizing of non-floating windows, with mouse.
    (setq window-divider-default-bottom-width 2
          window-divider-default-right-width 2)
    (window-divider-mode)
    ;; Allow switching to EXWM buffers not belonging to current workspace.
    ;; This behaviour takes some getting used to, I guess thats why its not default
    (setq exwm-layout-show-all-buffers t)
    ;; Configure Ido
    (my-exwm-config-ido)
    ;; Other configurations
    (my-exwm-config-misc))

  ;; This is copied from exwm-config.el
  (defun my-exwm-config--fix/ido-buffer-window-other-frame ()
    "Fix `ido-buffer-window-other-frame'."
    (defalias 'exwm-config-ido-buffer-window-other-frame
      (symbol-function #'ido-buffer-window-other-frame))
    (defun ido-buffer-window-other-frame (buffer)
      "This is a version redefined by EXWM.

  You can find the original one at `exwm-config-ido-buffer-window-other-frame'."
      (with-current-buffer (window-buffer (selected-window))
        (if (and (derived-mode-p 'exwm-mode)
                 exwm--floating-frame)
            ;; Switch from a floating frame.
            (with-current-buffer buffer
              (if (and (derived-mode-p 'exwm-mode)
                       exwm--floating-frame
                       (eq exwm--frame exwm-workspace--current))
                  ;; Switch to another floating frame.
                  (frame-root-window exwm--floating-frame)
                ;; Do not switch if the buffer is not on the current workspace.
                (or (get-buffer-window buffer exwm-workspace--current)
                    (selected-window))))
          (with-current-buffer buffer
            (when (derived-mode-p 'exwm-mode)
              (if (eq exwm--frame exwm-workspace--current)
                  (when exwm--floating-frame
                    ;; Switch to a floating frame on the current workspace.
                    (frame-selected-window exwm--floating-frame))
                ;; Do not switch to exwm-mode buffers on other workspace (which
                ;; won't work unless `exwm-layout-show-all-buffers' is set)
                (unless exwm-layout-show-all-buffers
                  (selected-window)))))))))

  (defun my-exwm-config-ido ()
    "Configure Ido to work with EXWM."
    ;; (ido-mode 1)
    (add-hook 'exwm-init-hook #'my-exwm-config--fix/ido-buffer-window-other-frame))

  (defun my-exwm-config-misc ()
    "Other configurations."
    ;; Make more room
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1))

  ;; Rename buffer to window title.
  (defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))

  (my-exwm-config-setup) ;; Does not start X11 or EXWM. Start should be done from commandline.

  (use-package! telephone-line)
  (defun ambrevar/bottom-right-window-p ()
    "Determines whether the last (i.e. bottom-right) window of the
    active frame is showing the buffer in which this function is
    executed."
    (let* ((frame (selected-frame))
           (right-windows (window-at-side-list frame 'right))
           (bottom-windows (window-at-side-list frame 'bottom))
           (last-window (car (seq-intersection right-windows bottom-windows))))
      (eq (current-buffer) (window-buffer last-window))))

  (defun jw/telephone-misc-if-exwm-or-last-window ()
    "Renders the mode-line-misc-info string for display in the
    mode-line if the currently active window is the last one in the
    frame, or an exwm window.

    The idea is to not display information like the current time,
    load, battery levels on all buffers.
    And to display input mode only in exwm windows."

    (when (or (ambrevar/bottom-right-window-p)
              exwm-window-type)
      (telephone-line-raw mode-line-misc-info t)))

  (defun jw/input-mode-str ()
    "Return string representing input mode, if window is of type EXWM"
    (if exwm-window-type
        (if (eq exwm--input-mode 'line-mode)
          (format "l")
          (format "c"))
      (format "")))

  (defun jw/workspace-index ()
    "Return string representing current EXWM workspace index"
    (if (ambrevar/bottom-right-window-p)
      (format "[%s]" (exwm-workspace--position (selected-frame)))
      (format "")))

  (defun jw/format-workspace-index-and-input-mode ()
    "Return string [workspace_index]input-mode depending on exwm-window or bottom-right window"
    (format "%s%s" (jw/workspace-index) (jw/input-mode-str)))

  (defun ambrevar/telephone-line-setup ()
    (telephone-line-defsegment telephone-line-last-window-segment ()
      (jw/telephone-misc-if-exwm-or-last-window))

    ;; Display the current EXWM workspace index in the mode-line
    (telephone-line-defsegment telephone-line-exwm-workspace-index ()
      (jw/format-workspace-index-and-input-mode))

    ;; Define a highlight font for ~ important ~ information in the last
    ;; window.
    (defface special-highlight '((t (:foreground "white" :background "#5f627f"))) "")
    (add-to-list 'telephone-line-faces
                 '(highlight . (special-highlight . special-highlight)))

    (setq telephone-line-lhs
          '((nil . (telephone-line-position-segment))
            (accent . (telephone-line-buffer-segment))))

    (setq telephone-line-rhs
          '((accent . (telephone-line-major-mode-segment))
            (nil . (telephone-line-last-window-segment
                    telephone-line-exwm-workspace-index))))

    (setq telephone-line-primary-left-separator 'telephone-line-tan-left
          telephone-line-primary-right-separator 'telephone-line-tan-right
          telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
          telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)

    (telephone-line-mode 1))

  (ambrevar/telephone-line-setup)

(use-package! helm-exwm
  :config
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))

(setq epkg-repository "~/epkgs/")

(setq epkg-repository "~/epkgs/")

(global-set-key [(hyper up)]
                (lambda ()
                  (interactive)
                  (let ((scroll-preserve-screen-position nil))
                    (scroll-down 1))) )
(global-set-key [(hyper down)]
                (lambda ()
                  (interactive)
                  (let ((scroll-preserve-screen-position nil))
                    (scroll-up 1))) )

(setq save-interprogram-paste-before-kill t)

(pcre-mode t)

(pcre-mode t)

(use-package! visual-regexp
  :bind (;; Replace the regular query replace with the regexp query
         ;; replace provided by this package.
         ("M-%" . vr/query-replace)))

(use-package! visual-regexp-steroids
  :after visual-regexp
  :config
  ;; Use Perl-style regular expressions by default.
  (setq vr/engine 'pcre2el))

(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "C-s") 'swiper)

(set-cursor-color "firebrick")
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")

(defun my-set-cursor-color ()
  "Change cursor color according to themes/init.el"
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color "firebrick"))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'my-set-cursor-color)

(global-whitespace-mode t) ; Tell Doom that I want control over whitespace-style
(setq-default whitespace-style
              '(face
                tabs
                trailing
                empty
                )
              )
;; show unncessary whitespace that can mess up your diff
;; (add-hook 'diff-mode-hook
;;           (lambda ()
;;             (setq-local whitespace-style
;;                         '(face
;;                           tabs
;;                           tab-mark
;;                           spaces
;;                           space-mark
;;                           trailing
;;                           indentation::space
;;                           indentation::tab
;;                           newline
;;                           newline-mark))
;;             (whitespace-mode 1)))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq-local whitespace-style
;;                   (append whitespace-style '(trailing))))
;;           t) ; Add near end of hooks list of functions

(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(add-hook 'vterm-mode-hook
          (lambda ()
            (whitespace-mode -1)
            (setq whitespace-style nil)))

(use-package! hungry-delete
  :config
  (global-hungry-delete-mode))

(windmove-default-keybindings)
(global-set-key (kbd "<kp-4>") 'windmove-left)
(global-set-key (kbd "<kp-6>") 'windmove-right)
(global-set-key (kbd "<kp-8>") 'windmove-up)
(global-set-key (kbd "<kp-2>") 'windmove-down)

(setq ibuffer-saved-filter-groups
      '(("home"
         ("dired" (mode . dired-mode))
         ("org" (name .  ".*org$"))
;;          ("helm" (predicate string-match "Helm" mode-name))
         ("web" (or (mode .  web-mode) (mode .  js2-mode)))
         ("shell" (or (mode . eshell-mode) (mode .  shell-mode)))
         ("programming" (or (mode . python-mode) (mode . c++-mode)))
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Bookmark List\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Messages\\*$")))
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")))
         ("martinowen.net" (filename . "martinowen.net"))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("code" (filename . "code"))
         ("Web Dev" (or (mode . html-mode)
                        (mode . css-mode)))
         ("Subversion" (name . "\*svn"))
         ("Magit" (name . "\*magit"))
         ("ERC" (mode . erc-mode))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(setq python-shell-interpreter "python3")

(use-package! thingatpt+)

(use-package! thingatpt+)

(use-package! hide-comnt)

(use-package! hide-comnt)

  (use-package! thing-cmds)

  (use-package! thing-cmds)

(use-package! hexrgb)

(use-package! hexrgb)

  (use-package! palette)

  (use-package! palette)

  (use-package! facemenu+)

  (use-package! facemenu+)

  (use-package! highlight)

  (use-package! highlight)

  (use-package! mouse3)

  (use-package! mouse3)

(setq dired-clean-up-buffers-too nil) ; Avoid pesky questions about deleting orphan buffers
(defconst my-dired-media-files-extensions
 '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "wmv" "mkv" "mov" "wma")
  "Media file extensions that should launch in VLC.
Also used for highlighting.")

  (bind-keys :map dired-mode-map
             ("ö" . dired-filter-map)
             ("ä" . dired-filter-mark-map))
  (use-package! dired-filter
    :config
    (setq dired-filter-group-saved-groups
       (make-list 1 '("default"
                      ("Epub"
                       (extension . "epub"))
                      ("PDF"
                       (extension . "pdf"))
                      ("LaTeX"
                       (extension "tex" "bib"))
                      ("Org"
                       (extension . "org"))
                      ("Archives"
                       (extension "zip" "rar" "gz" "bz2" "tar")))))
  )

(use-package! dired-narrow
  :commands dired-narrow
  :init
  (map! :map dired-mode-map
        :desc "Live filtering" "å" #'dired-narrow))

(use-package! dired-launch)
(dired-launch-enable)

  (use-package! dired-ranger
    :config
    (setq dired-ranger-bookmark-LRU ?l)
    ;; (bind-keys :map dired-mode-map
    ;;            :prefix "c"
    ;;            :prefix-map dired-ranger-map
    ;;            :prefix-docstring "Map for ranger operations."
    ;;   ("c" . dired-ranger-copy)
    ;;   ("p" . dired-ranger-paste)
    ;;   ("m" . dired-ranger-move))
    :bind (:map dired-mode-map
                ("W" . dired-ranger-copy)
                ("X" . dired-ranger-move)
                ("Y" . dired-ranger-paste)
                ("'" . dired-ranger-bookmark)
                ("l" . dired-ranger-bookmark-visit))
  )
(ranger-override-dired-mode -1)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  (bind-keys :map dired-mode-map
    ("<delete>" . dired-unmark-backward)
    ("<backspace>" . dired-up-directory))

  (dired-filter-mode t)
  (dired-filter-group-mode t)
  ;; (dired-collapse-mode 1)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))
(add-hook 'dired-mode-hook 'my-dired-init)

  (use-package! dired+
    :config
    (setq diredp-image-preview-in-tooltip 300))

  (use-package! bookmark+)

  (use-package! bookmark+)

  (use-package! w3m
    :config
    (setq w3m-key-binding 'info)
     (define-key w3m-mode-map [up] 'previous-line)
     (define-key w3m-mode-map [down] 'next-line)
     (define-key w3m-mode-map [left] 'backward-char)
     (define-key w3m-mode-map [right] 'forward-char)
    (setq w3m-default-display-inline-images t)
    (setq w3m-make-new-session t)
    (setq w3m-use-cookies t)
    (setq w3m-default-save-directory "~/Downloads/")
    (add-hook 'w3m-display-hook
            (lambda (url)
              (rename-buffer
               (format "*w3m: %s*"
                       (or w3m-current-title w3m-current-url)) t)))
    (defun wicked/w3m-open-current-page-in-chrome ()
      "Open the current URL in Google Chrome."
      (interactive)
      (browse-url-chrome w3m-current-url)) ;; (1)

    (defun wicked/w3m-open-link-or-image-in-chrome ()
      "Open the current link or image in Chrome."
      (interactive)
      (browse-url-chrome (or (w3m-anchor) ;; (2)
                             (w3m-image)))) ;; (3)
    (define-key w3m-mode-map (kbd "f") 'wicked/w3m-open-current-page-in-chrome)
    (define-key w3m-mode-map (kbd "F") 'wicked/w3m-open-link-or-image-in-chrome)
  )

(eval-after-load "w3m-search"
  '(progn
    (add-to-list 'w3m-search-engine-alist
                 '("archwiki"
                   "https://wiki.archlinux.org/index.php?search=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("ask"
                   "https://www.ask.com/web?q=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("bbcnews"
                   "http://search.bbc.co.uk/search?scope=all&tab=ns&q=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("cia"
                   "https://www.cia.gov/search?q=%s&site=CIA&client=CIA&proxystylesheet=CIA&output=xml_no_dtd&myAction=%2Fsearch&submitMethod=get"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("cpan"
                   "https://metacpan.org/search?q=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("debian-wiki"
                   "https://wiki.debian.org/FindPage?action=fullsearch&titlesearch=0&value=%s&submit=Search+Text"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("loc"
                   "http://www.loc.gov/search/?q=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("py2doc"
                   "http://docs.python.org/2/search.html?q=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("py3doc"
                   "http://docs.python.org/3/search.html?q=%s"
                   nil))
    (add-to-list 'w3m-search-engine-alist
                 '("reddit"
                   "http://www.reddit.com/search?q=%s"
                   nil))
   )
)

  (use-package! ace-link
    :config
    (ace-link-setup-default))

  (setq browse-url-mosaic-program nil)
  (setq browse-url-browser-function 'w3m-browse-url
        browse-url-new-window-flag t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'browse-url-interactive-arg "browse-url")

  (use-package! helm-w3m)

(define-prefix-command 'launcher-map)
(define-key launcher-map "c" #'link-hint-copy-link)
(define-key launcher-map "C" #'org-capture)
(define-key launcher-map "d" #'helpful-at-point)
(define-key launcher-map "e" #'er/expand-region)
(define-key launcher-map "E" #'er/contract-region)
(define-key launcher-map "f" #'find-dired)
(define-key launcher-map "g" #'w3m-search)
(define-key launcher-map "j" #'org-journal-new-entry)
(define-key launcher-map "l" #'browse-url-at-point)
(define-key launcher-map "o" #'link-hint-open-link)
(define-key launcher-map "t" #'proced) ; top
;;(define-key launcher-map "u" #'my/copy-id-to-clipboard)
(define-key launcher-map "w" #'w3m-goto-url)
(global-set-key (kbd "H-l") 'launcher-map)

  ;;shortcut functions
  (defun bjm/elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
  (defun bjm/elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  (defun bjm/elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))
  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))
  (defun mz/elfeed-browse-url (&optional use-generic-p)
      "Visit the current entry in your browser using `browse-url'.
    If there is a prefix argument, visit the current entry in the
    browser defined by `browse-url-generic-program'."
      (interactive "P")
      (let ((entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (if use-generic-p
                        (browse-url-chrome (elfeed-entry-link entry))
                      (browse-url (elfeed-entry-link entry))))
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
        ;;(forward-line)
  )))
  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  (use-package! elfeed
    :bind (:map elfeed-search-mode-map
               ("A" . bjm/elfeed-show-all)
               ("E" . bjm/elfeed-show-emacs)
               ("D" . bjm/elfeed-show-daily)
               ("b" . mz/elfeed-browse-url)
               ("B" . elfeed-search-browse-url)
               ("j" . mz/make-and-run-elfeed-hydra)
               ("m" . elfeed-toggle-star)
               ("q" . bjm/elfeed-save-db-and-bury))
    :config
    (defalias 'elfeed-toggle-star
       (elfeed-expose #'elfeed-search-toggle-all 'star))
  )

  ;; Load elfeed-org
  (use-package! elfeed-org
    :init
    (setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
    :config
    (elfeed-org))
  (defun z/hasCap (s) ""
    (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" s)))
  (defun z/get-hydra-option-key (s)
    "returns single upper case letter (converted to lower) or first"
    (interactive)
    (let ( (loc (z/hasCap s)))
    (if loc
      (downcase (substring s loc (+ loc 1)))
      (substring s 0 1)
  )))
  (defun mz/make-elfeed-cats (tags)
    "Returns a list of lists. Each one is line for the hydra configuratio in the form
    (c function hint)"
    (interactive)
    (mapcar (lambda (tag)
      (let* (
             (tagstring (symbol-name tag))
             (c (z/get-hydra-option-key tagstring)))
        (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring  )))
      tags))
  (defmacro mz/make-elfeed-hydra ()
    `(defhydra mz/hydra-elfeed ()
      "filter"
      ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
      ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
      ("M" elfeed-toggle-star "Mark")
      ("A" (elfeed-search-set-filter "@6-months-ago") "All")
      ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
      ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
      ("q" nil "quit" :color blue)
  ))
  (defun mz/make-and-run-elfeed-hydra ()
    ""
    (interactive)
    (mz/make-elfeed-hydra)
    (mz/hydra-elfeed/body))
  (defun my-elfeed-tag-sort (a b)
    (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
           (b-tags (format "%s" (elfeed-entry-tags b))))
      (if (string= a-tags b-tags)
          (< (elfeed-entry-date b) (elfeed-entry-date a)))
      (string< a-tags b-tags)))
  (setf elfeed-search-sort-function #'my-elfeed-tag-sort)

(use-package! nov
  :init
  (push '("\\.epub\\'" . nov-mode) auto-mode-alist)
  :bind
  (:map nov-mode-map
        ("<home>" . move-beginning-of-line)
        ("<end>" . move-end-of-line)
        ))

  ;; (defun my-window-displaying-calibredb-entry-p (window)
  ;;   (equal (with-current-buffer (window-buffer window) major-mode)
  ;;          'calibredb-show))

  ;; (defun my-position-calibredb-entry-buffer (buffer alist)
  ;;   (let ((agenda-window (car (cl-remove-if-not #'my-window-displaying-calibredb-entry-p (window-list)))))
  ;;     (when agenda-window
  ;;       (set-window-buffer agenda-window  buffer)
  ;;       agenda-window)))

  (use-package! calibredb
    :config
    (setq sql-sqlite-program "/usr/bin/sqlite3")
    (setq calibredb-program "/usr/bin/calibredb")
    (setq calibredb-root-dir (expand-file-name "~/calibre_library"))
    (setq calibredb-db-dir (concat calibredb-root-dir "/metadata.db"))
    (setq calibredb-library-alist '(("~/calibre_library")))

    ;; (add-to-list 'display-buffer-alist (cons "\\*calibredb-entry\\*" (cons #'my-position-calibredb-entry-buffer nil)))
    )

  (use-package! good-scroll
    :config
    (good-scroll-mode 1))

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(use-package! mixed-pitch)

(use-package! mixed-pitch)

(use-package! smartparens)

(use-package! smartparens)

  (use-package! hyperbole
    :config
    (require 'hyperbole)
    ;; (hyperbole-mode 1)
    (setq hsys-org-enable-smart-keys t)
    (global-set-key (kbd "s-<return>") 'hkey-either)
    (global-set-key (kbd "S-s-<return>") 'assist-key)
    (global-set-key (kbd "<mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<double-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<triple-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<down-mouse-9>") 'action-key-depress-emacs)
    (global-set-key (kbd "<drag-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<left-fringe> <mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<left-fringe> <down-mouse-9>") 'action-key-depress-emacs)
    (global-set-key (kbd "<left-fringe> <drag-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<right-fringe> <mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<right-fringe> <down-mouse-9>") 'action-key-depress-emacs)
    (global-set-key (kbd "<right-fringe> <drag-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<vertical-line> <mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<vertical-line> <down-mouse-9>") 'action-key-depress-emacs)
    (global-set-key (kbd "<vertical-line> <drag-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<mode-line> <mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<mode-line> <down-mouse-9>") 'action-key-depress-emacs)
    (global-set-key (kbd "<mode-line> <drag-mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<header-line> <mouse-9>") 'action-mouse-key-emacs)
    (global-set-key (kbd "<header-line> <down-mouse-9>") 'action-key-depress-emacs)
    (global-set-key (kbd "<header-line> <drag-mouse-9>") 'action-mouse-key-emacs)
    (hkey-ace-window-setup)
    ;; (global-set-key (kbd "s-o") 'hkey-operate)
    )

(defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
        ("f" helm-follow-mode))

(define-key helm-map (kbd "H-o") 'hydra-helm/body)

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

;; (use-package ggtags
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (ggtags-mode 1))))
;;   )

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

;; Change "Jane Joplin & John B Doe_" -> "Jane Joplin_ & Doe, John B"
(fset 'jw/swap_author
      (kmacro-lambda-form [?\M-b left ?\M-d ?\M-x ?s ?e ?a ?r ?c ?h ?- ?b ?a ?c ?k ?w ?a ?r ?d ?s backspace return ?& return ?\C-f ?\C-y ?, ?\M-b ?\M-b ?\M-f] 0 "%d"))

;; Replace "," with " &"
(fset 'jw/comma_to_ampersand
      (kmacro-lambda-form [?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?, return ?  ?& return] 0 "%d"))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; (setq session-use-package t nil (session))
;; session will be save if a buffer is save to a file.
(add-hook 'after-save-hook #'session-save-session)
