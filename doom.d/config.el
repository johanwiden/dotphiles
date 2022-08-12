;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Johan Widén"
      user-mail-address "j.e.widen@gmail.com")

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

;; The concise one which relies on "implicit fallback values"
(setq fontaine-presets
      '((tiny
         :default-family "Iosevka Comfy Wide Fixed"
         :default-height 70)
        (small
         :default-family "Iosevka Comfy Fixed"
         :default-height 90)
        (regular
         :default-height 100)
        (source-code
         :default-family "Source Code Pro"
         :variable-pitch-family "Source Sans Pro"
         :default-height 110
         :bold-weight semibold)
        (medium
         :default-weight semilight
         :default-height 140)
        (large
         :default-weight semilight
         :default-height 180
         :bold-weight extrabold)
        (t ; our shared fallback properties
         :default-family "Iosevka Comfy"
         :default-weight normal
         :variable-pitch-family "Iosevka Comfy Duo"
         ;; :variable-pitch-family "FiraGO"
         :variable-pitch-height 1.05)))

(use-package! fontaine
  ;; :config
  ;; (fontaine-restore-latest-preset)

  ;; ;; Set `fontaine-recovered-preset' or fall back to desired style from
  ;; ;; `fontaine-presets'.
  ;; (if-let ((state fontaine-recovered-preset))
  ;;     (fontaine-set-preset state)
  ;;   (fontaine-set-preset 'regular))

  ;; ;; The other side of `fontaine-restore-latest-preset'.
  ;; (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  )

(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-completions
        (quote ((matches . (extrabold background intense))
                (selection . (semibold accented intense))
                (popup . (accented)))))
  (setq modus-themes-mixed-fonts t)
  ;; (setq modus-themes-italic-constructs t
  ;;       modus-themes-bold-constructs nil
  ;;       modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
  (setq doom-theme 'modus-vivendi)
  ;; :bind ("<f5>" . modus-themes-toggle)
  )

(defun ap/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive
   (list (intern (completing-read
                  "Theme: " (->> (custom-available-themes)
                              (-map #'symbol-name)
                              (--select (string-prefix-p "doom-" it)))))))
  (ap/switch-theme theme))

(defun ap/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive
   (list (intern (completing-read
                  "Theme: " (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

(defvar jw/paradox-github-token nil)

(let ((secret.el (expand-file-name ".secret.el" "~")))
  (when (file-exists-p secret.el)
    (load secret.el)))

(setq-default
 help-window-select t             ; Focus new help windows when opened
 ;;debug-on-error t
 ;;jit-lock-defer-time 0
 ;;fast-but-imprecise-scrolling t ; Set by doom
 ;;sentence-end-double-space nil    ; End a sentence after a dot and a space. Set by doom
 window-combination-resize t      ; Resize windows proportionally
 history-delete-duplicates t
 next-error-message-highlight t
 completions-detailed t
 describe-bindings-outline t
 save-interprogram-paste-before-kill t
 )
(after! recentf (setq recentf-max-saved-items 1000))

(global-auto-revert-mode t)

(defmacro defkeys (mapname &rest body)
  `(let ((defs '(,@body)))
     (while defs
       (define-key ,mapname
                   (if (vectorp (car defs))
                       (car defs)
                     (read-kbd-macro (car defs)))
                   (if (or (listp (cadr defs)) (functionp (cadr defs)))
                       (cadr defs)
                     (if `(keymapp (bound-and-true-p ,(cadr defs)))
                         (eval (cadr defs)))))
       (setq defs (cddr defs)))))

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-line
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(map! [remap dabbrev-expand] #'hippie-expand)

(setq org-directory "~/org/")
(setq org-attach-id-dir "~/org/attachments/")

(after! org
  (progn
    (setq org-use-speed-commands t)
    (add-to-list
     'org-capture-templates
     `("P" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
       "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
    (add-to-list
     'org-capture-templates
     `("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
       "* %? [[%:link][%:description]] \nCaptured On: %U"))
    (add-to-list
     'org-capture-templates
     `("l" "Link" entry (file+headline ,(concat org-directory "notes.org") "Links")
       "* %a %^g\n %?\n %T\n %i"))
    (add-to-list
     'org-capture-templates
     `("w" "Web site" entry (file "")
       "* %a :website:\n\n%U %?\n\n%:initial"))))

(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))

(use-package! org-journal
;;   :defer t
  :after org
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "~/org/roam/"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  (add-to-list 'org-agenda-files org-journal-dir)
)

(after! org
  (+org--babel-lazy-load 'python)
  (+org--babel-lazy-load 'shell)
  ;; (require 'ob-emacs-lisp)
  ;; ;; (require 'ob-ledger)
  ;; (require 'ob-python)
  ;; (require 'ob-shell)
  ;; (require 'ob-core)
  ;; (require 'ob-tangle)
  ;; (setq org-babel-load-languages '((emacs-lisp . t)
  ;;                                  (ledger . t)
  ;;                                  (python . t)
  ;;                                  (shell . t)  ; in my case /bin/bash
  ;;                                  ))
  )

(after! org
  (require 'ox-gfm nil t))

(setq org-roam-v2-ack t)
(setq org-roam-directory (file-truename "~/org/roam/")
      org-roam-db-location (file-truename "~/org/roam/org-roam.db")
      org-id-link-to-org-use-id t)

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-bibliography '("/home/jw/org/roam/biblio/references.bib")
        bibtex-completion-library-path "/home/jw/org/roam/pdfs"
        bibtex-completion-notes-path "/home/jw/org/roam/biblio/helm-bibtex-notes"
        bibtex-completion-notes-template-multiple-files "#+TITLE: Notes on: ${author-or-editor} (${year}): ${title}\n\nSee [cite/t:@${=key=}]\n"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
	    '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	      (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	      (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	      (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	      (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-open-function 'org-open-file
))

(use-package! org-menu
 :after org
 :config
 (define-key org-mode-map (kbd "C-c m") 'org-menu)
  )

(use-package! org-recoll)

(use-package! org-similarity
  :config
  (setq org-similarity-directory org-roam-directory)
  )

(setq display-line-numbers-type nil)

(after! helm
  (progn
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
      (define-key helm-map (kbd "C-j")  'helm-select-action) ; list actions using C-z

      (setq helm-candidate-number-limit 150
            helm-display-header-line t
            helm-ff-auto-update-initial-value t
            helm-ff-DEL-up-one-level-maybe t)
      ;; (unless (featurep! :completion new-helm +helm-popup-layout)
      ;;   (progn
      ;;     (setq helm-split-window-inside-p t)
      ;;     (helm-autoresize-mode t)))
      (when (featurep! :completion new-helm +childframe)
        (setq helm-posframe-border-width 16))

      ;; Was bound to the consult variant
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      ;; Was bound to the vertico variant
      (global-set-key (kbd "C-x b") 'helm-mini)

      ;; use helm to list eshell history
      (add-hook 'eshell-mode-hook
                #'(lambda ()
                    (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

      ;; show minibuffer history with Helm
      ;; (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
      ;; (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)
      ))

(after! helm-projectile
  ;; (setq projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))
(after! (helm consult-recoll)
  (add-to-list 'helm-completing-read-handlers-alist (cons #'consult-recoll nil))
)
;; (after! vertico
;;   (setq completion-category-overrides nil))

(use-package! helm-bibtex)

(use-package! helm-ls-git)

(use-package! helm-pydoc)

(use-package! helm-tramp)

(after! helm
  (progn
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
      (mu-helm-rg default-directory with-types))))

(use-package! org-ql)
(use-package! helm-org-ql)

(use-package! helm-org-rifle)

(after! helm
  (define-prefix-command 'C-z-map)
  (global-set-key (kbd "C-z") 'C-z-map)
  (defkeys global-map
    "C-z C-b" helm-buffers-list
    "C-z a"   mu-helm-project-search
    "C-z b"   helm-filtered-bookmarks
    "C-z c"   helm-company
    "C-z d"   helm-dabbrev
    "C-z e"   helm-calcul-expression
    "C-z g"   helm-google-suggest
    "C-z h"   helm-descbinds
    "C-z k"   helm-show-kill-ring
    "C-z f"   helm-find-files
    "C-z m"   helm-mini
    "C-z o"   helm-occur
    "C-z p"   helm-browse-project
    "C-z q"   helm-apropos
    "C-z r"   helm-recentf
    "C-z s"   swiper-helm
    "C-z C-c" helm-colors
    "C-z x"   helm-M-x
    "C-z y"   helm-yas-complete
    "C-z C-g" helm-ls-git-ls
    "C-z SPC" helm-all-mark-rings))

(use-package! citeproc
  :after org)
(use-package! oc
  :config
  (require 'oc-csl))
(use-package! org-ref-cite-core
  :after org)
(use-package! org-ref-cite
  :after org
  :config
  ;; I like green links
  (set-face-attribute 'org-cite nil :foreground "DarkSeaGreen4")
  (set-face-attribute 'org-cite-key nil :foreground "forest green")
  (setq
   org-cite-global-bibliography bibtex-completion-bibliography
   ;; https://github.com/citation-style-language/styles
   ;; or https://www.zotero.org/styles
   org-cite-csl-styles-dir "/home/jw/Zotero/styles"
   org-cite-insert-processor 'org-ref-cite
   org-cite-follow-processor 'org-ref-cite
   org-cite-activate-processor 'org-ref-cite
   org-cite-export-processors '((html csl "elsevier-with-titles.csl")
			        (latex org-ref-cite)
			        (t basic)))

  (define-key org-mode-map (kbd "C-c \\") 'org-cite-insert))

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
  ;; Setting exwm-manage-force-tiling t has the unfortunate side effect that new floating windows
  ;; are unresponsive for a considerable time (30 seconds or so)
  ;; (setq exwm-manage-force-tiling t)
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
                                    helm-source-recentf
                                    helm-source-bookmarks))
  )

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

;; (after! swiper
;;   (global-set-key (kbd "C-s") 'swiper))
(global-set-key (kbd "C-s") 'swiper)

(after! avy
  (setq avy-all-windows t)
  (setq avy-single-candidate-jump nil)
  ;; Avoid collision with action keys
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?e ?l))
  (global-set-key (kbd "M-j") 'avy-goto-char-timer)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; Bind to semicolon (flyspell uses C-;)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  ;; You can combine Hyperbole with Avy by creating an avy-dispatch function to press the Hyperbole action-key at target.
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2022-06/msg00686.html
  (after! hyperbole
    (add-to-list 'avy-dispatch-alist '(?: . (lambda (pt)
                                              (goto-char pt)
                                              (hkey-either))))))

(use-package! counsel
  :defer t
  :config
  (defun counsel-recoll-function (str)
    "Run recoll for STR."
    (or
     (ivy-more-chars)
     (progn
       (counsel--async-command
        (format "recollq -t -b %s"
                (shell-quote-argument str)))
       nil))))

(use-package! consult-recoll)

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

(use-package! unfill)

;; https://stackoverflow.com/questions/42595418/how-to-remove-hyphens-during-fill-paragraph
(defadvice fill-delete-newlines (before my-before-fill-delete-newlines)
  "Replace -\\n with an empty string when calling `unfill-paragraph' or `unfill-region'."
  (when (or (eq this-command 'unfill-paragraph)
            (eq this-command 'unfill-region))
    ;; (setq jw/arg0 (ad-get-arg 0))
    ;; (setq jw/arg1 (ad-get-arg 1))
    (goto-char (ad-get-arg 0))
    (while (search-forward "-\n" (ad-get-arg 1) t)
      (replace-match "")
      (ad-set-arg 1 (- (ad-get-arg 1) 2)))))

(ad-activate 'fill-delete-newlines)

(windmove-default-keybindings)
;; (global-set-key (kbd "<kp-4>") 'windmove-left)
;; (global-set-key (kbd "<kp-6>") 'windmove-right)
;; (global-set-key (kbd "<kp-8>") 'windmove-up)
;; (global-set-key (kbd "<kp-2>") 'windmove-down)

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

(use-package! thingatpt+
  :defer t)

(use-package! hide-comnt
  :defer t)

(use-package! thing-cmds
:defer t)

(use-package! hexrgb
  :defer t)

(use-package! palette
:defer t)

(use-package! facemenu+
:defer t)

(use-package! highlight
:defer t)

(global-set-key (kbd "S-<down-mouse-1>") #'mouse-set-mark)
;; was: mouse-appearance-menu
(use-package! mouse3)

(after! dired
  (progn
    (setq dired-clean-up-buffers-too nil) ; Avoid pesky questions about deleting orphan buffers
    (defconst my-dired-media-files-extensions
      '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "wmv" "mkv" "mov" "wma")
      "Media file extensions that should launch in VLC.
Also used for highlighting.")
    ))

(use-package! dired-filter
  :after dired
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
  (bind-keys :map dired-mode-map
             ("ö" . dired-filter-map)
             ("ä" . dired-filter-mark-map)))

(use-package! dired-narrow
  :after dired
  :commands dired-narrow
  :config
  (map! :map dired-mode-map
        :desc "Live filtering" "å" #'dired-narrow))

(use-package! dired-launch
  :after dired
  :config
  (dired-launch-enable))

(after! dired
  (progn
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
    (add-hook 'dired-mode-hook 'my-dired-init)))

(use-package! bookmark+
  :after dired
  ;;:defer t
  )

(use-package! w3m
  :defer t
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
                   nil))))

(use-package! ace-link
  :defer t
  :config
  (ace-link-setup-default))

(setq browse-url-mosaic-program nil)
(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'browse-url-interactive-arg "browse-url")

(use-package! helm-w3m
  :after w3m)

(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URL of current line or selection.

Example:
 %28D%C3%BCrer%29
becomes
 (Dürer)

Example:
 %E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
 文本编辑器

URL `http://xahlee.info/emacs/emacs/emacs_url_percent_decode.html'
Version 2018-10-26"
  (interactive)
  (let ( $p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-unhex-string $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert (decode-coding-string $newStr 'utf-8))))))

(defun jw/clean-org-protocol-l-result ()
  "Decode percent encoded result from org-protocol, capture key l. Delete text before url, add newline before title."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (xah-html-decode-percent-encoded-url)
    (goto-char (region-beginning))
    (if (re-search-forward "org-protocol.*url=" nil t)
        (replace-match "" nil nil))
    (if (search-forward "&title=" nil t)
        (replace-match "\ntitle=" nil nil))
    (if (search-forward "&body=" nil t)
        (replace-match "\nbody=" nil nil))
    )
  )

(defun tina/test-finalize ()
  (let ((key  (plist-get org-capture-plist :key))
        (desc (plist-get org-capture-plist :description)))
    (if org-note-abort
        (message "Template with key %s and description “%s” aborted" key desc)
      (message "Template with key %s and description “%s” run successfully" key desc))))

(defun jw/hook-clean-org-protocol-l-result ()
  "Wrapper around jw/clean-org-protocol-l-result, for add to hook."
  (when (and (not org-note-abort)
             (equal (plist-get org-capture-plist :key) "l"))
    (jw/clean-org-protocol-l-result))
  )

;; https://emacs.stackexchange.com/questions/45270/in-org-mode-how-can-i-make-a-post-capture-hook-run-only-for-certain-capture-tem
;; (after! org (add-hook 'org-capture-after-finalize-hook 'tina/test-finalize))
(after! org
  (add-hook 'org-capture-prepare-finalize-hook 'jw/hook-clean-org-protocol-l-result))

(use-package! org-protocol-capture-html
  :after org)

(after! (w3m org-journal)
  (progn
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
    ;;(define-key launcher-map "u" #'my/copy-id-to-clipboard)
    (define-key launcher-map "w" #'w3m-goto-url)
    (global-set-key (kbd "H-l") 'launcher-map)))

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
    (unless (or elfeed-search-remain-on-entry (use-region-p)))))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(use-package! elfeed
  :defer t
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
    (elfeed-expose #'elfeed-search-toggle-all 'star)))

(use-package! elfeed-org
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
  :config

  (defun z/hasCap (s) ""
         (let ((case-fold-search nil))
           (string-match-p "[[:upper:]]" s)))

  (defun z/get-hydra-option-key (s)
    "returns single upper case letter (converted to lower) or first"
    (interactive)
    (let ( (loc (z/hasCap s)))
      (if loc
          (downcase (substring s loc (+ loc 1)))
        (substring s 0 1))))

  (defun mz/make-elfeed-cats (tags)
    "Returns a list of lists. Each one is line for the hydra configuration in the form (c function hint)"
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
       ("q" nil "quit" :color blue)))

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

  (elfeed-org))

(use-package! nov
  :defer t
  :init
  (push '("\\.epub\\'" . nov-mode) auto-mode-alist)
  :bind
  (:map nov-mode-map
        ("<home>" . move-beginning-of-line)
        ("<end>" . move-end-of-line)))

;; (defun my-window-displaying-calibredb-entry-p (window)
;;   (equal (with-current-buffer (window-buffer window) major-mode)
;;          'calibredb-show))

;; (defun my-position-calibredb-entry-buffer (buffer alist)
;;   (let ((agenda-window (car (cl-remove-if-not #'my-window-displaying-calibredb-entry-p (window-list)))))
;;     (when agenda-window
;;       (set-window-buffer agenda-window  buffer)
;;       agenda-window)))

(use-package! calibredb
  :defer t
  :config
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-root-dir (expand-file-name "~/calibre_library"))
  (setq calibredb-db-dir (concat calibredb-root-dir "/metadata.db"))
  (setq calibredb-library-alist '(("~/calibre_library")))
  (setq calibredb-date-width 0)
  (setq calibredb-download-dir (expand-file-name "~/Downloads"))
  (setq calibredb-library-alist '(("/home/jw/calibre_library")
                                  ("https://bookserver.archive.org/catalog/")
                                  ("http://arxiv.maplepop.com/catalog/")
                                  ("https://m.gutenberg.org/ebooks.opds/")
                                  ))

  ;; (add-to-list 'display-buffer-alist (cons "\\*calibredb-entry\\*" (cons #'my-position-calibredb-entry-buffer nil)))
  )

(use-package! good-scroll
  :config
  (good-scroll-mode 1))

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(use-package! mixed-pitch)

(use-package! hyperbole
  :defer t
  :config
  ;; (require 'hyperbole)
  ;; (hyperbole-mode 1)
  (setq hsys-org-enable-smart-keys t)
  (global-set-key (kbd "H-<return>") 'hkey-either)
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

(after! helm
  (progn
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

    (define-key helm-map (kbd "H-o") 'hydra-helm/body)))

(after! helm-projectile
  (progn
    (defhydra hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("f"  projectile-find-file-other-window        "file")
      ("g"  projectile-find-file-dwim-other-window   "file dwim")
      ("d"  projectile-find-dir-other-window         "dir")
      ("b"  projectile-switch-to-buffer-other-window "buffer")
      ("q"  nil                                      "cancel" :color blue))

    ;; (use-package! ggtags
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
      ("q"   nil "cancel" :color blue))))

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
(add-to-list 'session-globals-exclude 'consult--buffer-history)
(add-to-list 'session-globals-exclude 'vertico-repeat-history)

(use-package! zoxide
  :defer t)

(use-package! hledger-mode
  :defer t
  :init
  ;; To open files with .journal extension in hledger-mode
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  :config
  ;; Provide the path to you journal file.
  ;; The default location is too opinionated.
  (setq hledger-jfile "/home/jw/Dokument/hledger/test/test1.journal")
  (load "~/.config/doomemacs.d/ob-hledger")
  (require 'ob-hledger))

;; Out of sync with hledger
;; (use-package! flycheck-hledger
;;   :after (flycheck hledger-mode)
;;   :demand t)

(set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))

(use-package engine-mode
  :config
  (engine-mode t))

(setq common-lisp-hyperspec-root
;; “http://www.lispworks.com/reference/HyperSpec/&#8221;)
"file:///home/jw/lisp/HyperSpec/")
;; (setq browse-url-browser-function ‘eww-browse-url)
(setq common-lisp-hyperspec-symbol-table "/home/jw/lisp/HyperSpec/Data/Map_Sym.txt")
;; block images in EWW browser
;; (setq-default shr-inhibit-images t)
(use-package! helm-sly
  :after sly-mrepl
  :config
  (add-hook 'sly-mrepl-hook #'company-mode)
  (require 'helm-company)

  (defun ambrevar/indent-and-helm-company (arg)
    "Indent then call `helm-company'.
  Good substitute for `sly-mrepl-indent-and-complete-symbol'."
    (interactive "P")
    (indent-for-tab-command arg)
    (helm-company))

  (define-key sly-mrepl-mode-map (kbd "<tab>") 'ambrevar/indent-and-helm-company)
  (add-hook 'lisp-mode-hook #'company-mode)
  (define-key lisp-mode-map (kbd "<tab>") 'ambrevar/indent-and-helm-company))

(use-package! arxiv-mode
  :defer t)

(use-package! pdftotext
  :defer t
  ;; For prettyness
  ;; (add-hook 'pdftotext-mode-hook #'spell-fu-mode-disable)
  ;; (add-hook 'pdftotext-mode-hook (lambda () (page-break-lines-mode 1)))
  ;; I have no idea why this is needed
  ;; (map! :map pdftotext-mode-map
  ;;       "<mouse-4>" (cmd! (scroll-down mouse-wheel-scroll-amount-horizontal))
  ;;       "<mouse-5>" (cmd! (scroll-up mouse-wheel-scroll-amount-horizontal)))
  :config
  (defun pdftotext-enable ()
    "Enable pdftotext-mode."
    (interactive)
    (after! pdf-tools (pdftotext-install)))

  (defun pdftotext-disable ()
    "Disable pdftotext-mode."
    (interactive)
    (after! pdf-tools (progn
                        (pdftotext-uninstall)
                        (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)
                        (add-to-list 'magic-mode-alist pdf-tools-magic-mode-alist-entry)))))

(use-package! xah-math-input
  :defer t
  :config
  (xah-math-input--add-to-hash
   '(
     ["zws" "​"]
     )))
