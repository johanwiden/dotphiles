;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Johan Widén"
      user-mail-address "j.e.widen@gmail.com")

(defvar my/is-termux
  (string-suffix-p
   "Android" (string-trim (shell-command-to-string "uname -a")))
  "Truthy value indicating if Emacs is currently running in termux.")
(defvar my/is-terminal
  (not window-system)
  "Truthy value indicating if Emacs is currently running in a terminal.")

(global-set-key (kbd "§") (lookup-key global-map (kbd "C-x")))
(global-set-key (kbd "<insert>") (lookup-key global-map (kbd "C-x")))

(defun my/shell-command-on-file (command)
  "Execute COMMAND asynchronously on the current file."
  (interactive (list (read-shell-command
                      (concat "Async shell command on " (buffer-name) ": "))))
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (async-shell-command (concat command " " filename))))
(bind-key (kbd "C-M-&") #'my/shell-command-on-file)

;; (setq doom-modeline-height 20)
;; mono 18, var 15
(setq doom-font (font-spec :family "Iosevka Comfy Fixed" :size 15)
      ;; doom-font (font-spec :family "Iosevka" :size 16)
      ;; doom-variable-pitch-font (font-spec :family "Iosevka Comfy" :size 13)
      doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :size 15)
      ;;doom-variable-pitch-font (font-spec :family "Overpass" :size 12)
      ;;doom-variable-pitch-font (font-spec :family "FiraGO" :size 15)
      ;;doom-variable-pitch-font (font-spec :family "Libre Baskerville" :height 1.0)
      ;;doom-serif-font (font-spec :family "Libre Baskerville" :height 1.0)
      )
(set-face-attribute 'default nil :font "Iosevka Comfy Fixed-15")
;;(set-face-attribute 'default nil :font "Iosevka-16")
;;(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy Fixed" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Duo" :height 1.0)
;;(set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)
;;(set-face-attribute 'variable-pitch nil :family "Libre Baskerville" :height 1.0)
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))
;; doom modeline
;; (custom-set-faces!
;;   '(mode-line :height 0.9)
;;   '(mode-line-inactive :height 0.9))
(require 'doom-modeline)

;;
;; evil-state
;;

(doom-modeline-def-segment evil-state
  "The current evil state.  Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (s-trim-right (evil-state-property evil-state :tag t))))

(doom-modeline-def-modeline 'main
  '(bar workspace-name window-number evil-state modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time))

(doom-modeline-def-modeline 'minimal
  '(bar evil-state matches buffer-info-simple)
  '(media-info major-mode time))

(doom-modeline-def-modeline 'special
  '(bar window-number evil-state modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process time))

(doom-modeline-def-modeline 'project
  '(bar window-number evil-state modals buffer-default-directory remote-host buffer-position)
  '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))

(doom-modeline-def-modeline 'dashboard
  '(bar window-number evil-state buffer-default-directory-simple remote-host)
  '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))

(doom-modeline-def-modeline 'vcs
  '(bar window-number evil-state modals matches buffer-info remote-host buffer-position parrot selection-info)
  '(compilation misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process time))

(doom-modeline-def-modeline 'package
  '(bar window-number evil-state package)
  '(compilation misc-info major-mode process time))

(doom-modeline-def-modeline 'info
  '(bar window-number evil-state buffer-info info-nodes buffer-position parrot selection-info)
  '(compilation misc-info buffer-encoding major-mode time))

(doom-modeline-def-modeline 'media
  '(bar window-number evil-state buffer-size buffer-info)
  '(compilation misc-info media-info major-mode process vcs time))

(doom-modeline-def-modeline 'message
  '(bar window-number evil-state modals matches buffer-info-simple buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode time))

(doom-modeline-def-modeline 'pdf
  '(bar window-number evil-state matches buffer-info pdf-pages)
  '(compilation  misc-info major-mode process vcs time))

(doom-modeline-def-modeline 'org-src
  '(bar window-number evil-state modals matches buffer-info buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process checker time))

(doom-modeline-def-modeline 'timemachine
  '(bar window-number evil-state modals matches git-timemachine buffer-position word-count parrot selection-info)
  '(misc-info minor-modes indent-info buffer-encoding major-mode time))

(doom-modeline-def-modeline 'calculator
  '(window-number evil-state modals matches calc buffer-position)
  '(misc-info minor-modes major-mode process))

(setq evil-normal-state-tag   (propertize "[Normal]" 'face '((:background "dark green" :foreground "black")))
      evil-emacs-state-tag    (propertize "[Emacs]" 'face '((:background "goldenrod" :foreground "black")))
      evil-insert-state-tag   (propertize "[Insert]" 'face '((:background "dark red") :foreground "white"))
      evil-motion-state-tag   (propertize "[Motion]" 'face '((:background "blue") :foreground "white"))
      evil-visual-state-tag   (propertize "[Visual]" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "[Operator]" 'face '((:background "purple"))))

;; Enable mode-line in vterm
(after! doom-modeline
  (add-to-list 'doom-modeline-mode-alist '(vterm-mode . main))
  (add-to-list 'doom-modeline-mode-alist '(shell-mode . main))
  (add-to-list 'doom-modeline-mode-alist '(eshell-mode . main)))

;; The concise one which relies on "implicit fallback values"
(setq fontaine-presets
      '((tiny
         :default-family "Iosevka Comfy Wide Fixed"
         :default-height 70)
        (small
         :default-family "Iosevka Comfy Motion"
         :default-height 90)
        (regular)
        (source-code
         :default-family "Source Code Pro"
         :variable-pitch-family "Source Sans Pro"
         :default-height 110
         :bold-weight semibold)
        (medium
         :default-weight semilight
         :default-height 140
         :bold-weight extrabold)
        (large
         :inherit medium
         :default-height 180
         )
        (t ; our shared fallback properties
         :default-family "Iosevka Comfy"
         :default-weight regular
         :default-height 100
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-family
         :fixed-pitch-serif-height 1.0
         :variable-pitch-family "Iosevka Comfy Motion Duo"
         :variable-pitch-weight nil
         ;; :variable-pitch-family "FiraGO"
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))

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

;; (defun my-update-active-mode-line-colors ()
;;   (set-face-attribute
;;    'mode-line nil
;;    :foreground (modus-themes-get-color-value 'fg-mode-line-active)
;;    :background "goldenrod"
;;    :box '(:line-width
;;           1
;;           :color
;;           (modus-themes-get-color-value 'border-mode-line-active))))
;; (defun my-update-active-mode-line-colors ()
;;   (set-face-attribute
;;    'mode-line nil
;;    :background "dark olive green"))
;; (defun my-update-active-mode-line-colors ()
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(mode-line ((t :background ,bg-yellow-subtle))))))

;; (add-hook 'modus-themes-post-load-hook #'my-update-active-mode-line-colors)

;; Can also be done with
(setq modus-themes-common-palette-overrides
      '((bg-mode-line-active bg-yellow-subtle)))

(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-completions
        (quote ((matches . (extrabold background intense))
                (selection . (semibold accented intense))
                (popup . (accented)))))
  ;; 1.5 1.3 1.8
  (setq modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-prompts '(bold)
        modus-themes-org-blocks 'tinted-background
        modus-themes-headings '((1 . (light variable-pitch 1.0))
                                (agenda-date . (1.0))
                                (agenda-structure . (variable-pitch light 1.0))
				(t . (medium))))
  :config
  (setq custom-safe-themes t)
  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (load-theme 'modus-vivendi-tinted)
  (setq doom-theme 'modus-vivendi-tinted)
  ;; (load-theme 'modus-vivendi)
  ;; (setq doom-theme 'modus-vivendi)
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

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Optional, but recommended. Tree-sitter enabled major modes are
;; distinct from their ordinary counterparts.
;;
;; You can remap major modes with `major-mode-remap-alist'. Note
;; that this does *not* extend to hooks! Make sure you migrate them
;; also
(dolist (mapping '((python-mode . python-ts-mode)
                   (sh-mode . bash-ts-mode)
                   (css-mode . css-ts-mode)
                   (c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   ;; (typescript-mode . tsx-ts-mode)
                   (js-mode . js-ts-mode)
                   (json-mode . json-ts-mode)
                   (css-mode . css-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;; Following has to be run when doom emacs is reinstalled.
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; Use the full theming potential of treesit
(setq treesit-font-lock-level 4)

;; tweak the new funcall face
(custom-theme-set-faces
 ;; for current theme
 (or (car custom-enabled-themes) 'user)

 ;; funcall face
 `(font-lock-function-call-face
   ((t :inherit font-lock-function-name-face
       :foreground "hot pink"
       :background "black"))))

(defvar jw/paradox-github-token nil)

(let ((secret.el (expand-file-name ".secret.el" "~")))
  (when (file-exists-p secret.el)
    (load secret.el)))

(setq-default
 bookmark-save-flag 1 ; Save bookmark list immediately when it has been updated.
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
 ;; Change this from 10MB to 100MB
 large-file-warning-threshold 500000000
 show-paren-context-when-offscreen 'overlay
 treemacs-no-load-time-warnings t
 )
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(after! recentf
  (progn(setq recentf-max-saved-items 10000)
        (run-at-time nil (* 5 60)
             (lambda ()
               (let ((save-silently t))
                 (recentf-save-list))))))
(after! savehist
  (setq savehist-autosave-interval 600))
(setq use-package-verbose t)
(add-hook 'text-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'prog-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'mistty-mode-hook (lambda () (visual-line-mode 1)))

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

(after! org-journal
  (progn
    ;; With re-search-forward: Do not attempt to search for the empty string. Use instead something like
    ;; "^\*\* ", to search for all org-journal top entries.
    (customize-set-variable 'org-journal-search-forward-fn 're-search-forward)
    (setq org-journal-date-prefix "#+TITLE: "
          org-journal-file-format "private-%Y-%m-%d.org"
          org-journal-dir "~/org/roam/"
          org-journal-carryover-items nil
          org-journal-date-format "%Y-%m-%d")
    (add-to-list 'org-agenda-files org-journal-dir)))

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

(use-package! org-tempo
 :after org
  )

(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(setq display-line-numbers-type nil)

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

(require 'transient)

(transient-define-prefix cc/isearch-menu ()
  "isearch Menu"
  [["Edit Search String"
    ("e"
     "Edit the search string (recursive)"
     isearch-edit-string
     :transient nil)
    ("w"
     "Pull next word or character word from buffer"
     isearch-yank-word-or-char
     :transient nil)
    ("s"
     "Pull next symbol or character from buffer"
     isearch-yank-symbol-or-char
     :transient nil)
    ("l"
     "Pull rest of line from buffer"
     isearch-yank-line
     :transient nil)
    ("y"
     "Pull string from kill ring"
     isearch-yank-kill
     :transient nil)
    ("t"
     "Pull thing from buffer"
     isearch-forward-thing-at-point
     :transient nil)]

   ["Replace"
    ("q"
     "Start ‘query-replace’"
     isearch-query-replace
     :if-nil buffer-read-only
     :transient nil)
    ("x"
     "Start ‘query-replace-regexp’"
     isearch-query-replace-regexp
     :if-nil buffer-read-only
     :transient nil)]]

  [["Toggle"
    ("X"
     "Toggle regexp searching"
     isearch-toggle-regexp
     :transient nil)
    ("S"
     "Toggle symbol searching"
     isearch-toggle-symbol
     :transient nil)
    ("W"
     "Toggle word searching"
     isearch-toggle-word
     :transient nil)
    ("F"
     "Toggle case fold"
     isearch-toggle-case-fold
     :transient nil)
    ("L"
     "Toggle lax whitespace"
     isearch-toggle-lax-whitespace
     :transient nil)]

   ["Misc"
    ("o"
     "occur"
     isearch-occur
     :transient nil)]])

(define-key isearch-mode-map (kbd "<f2>") 'cc/isearch-menu)

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
         ("journal" (name . "private-"))
         ("programming" (or (mode . python-mode)
                            (mode . c++-mode)))
         ("shell" (or (mode . eshell-mode)
                      (mode .  shell-mode)))
         ("sly" (name . "sly"))
         ("web" (or (mode .  web-mode)
                    (mode .  js2-mode)))
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Bookmark List\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Messages\\*$")))
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")
                             (filename . "config.org")
                             (filename . "config.el")))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("Magit" (name . "\*magit"))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("Web Dev" (or (mode . html-mode)
                        (mode . css-mode)))
         ("Windows" (mode . exwm-mode)))))
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
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

(use-package! dired+
    :after dired
    :config
    ;; diredp requires dired-actual-switches to be a string, not nil, but
    ;; this variable is only non nil in dired buffers
    (setq dired-actual-switches "-al")
    ;; (setq diredp-image-preview-in-tooltip 300)
    )
;; (after! dired
;;   (load "/home/jw/Downloads/dired+.el"))

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
;; (setq browse-url-browser-function 'w3m-browse-url)
(setq browse-url-new-window-flag t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'browse-url-interactive-arg "browse-url")

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
    (global-set-key (kbd "C-c l") 'launcher-map)))

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
  (setq rmh-elfeed-org-files (list "~/.config/doom/elfeed.org"))
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
  ;; (add-hook 'nov-mode-hook #'shrface-mode)
  ;; :config
  ;; (require 'shrface)
  ;; (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  ;; (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
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

(after! projectile
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

(progn
    (defhydra help/hydra/left/describe (:color blue
                                        :hint nil)
  "
Describe Something: (q to quit)
_a_ all help for everything screen
_A_ autodefs
_b_ bindings
_B_ personal bindings
_c_ char
_C_ coding system
_d_ Doom module
_D_ Doom help
_f_ function
_F_ flycheck checker
_h_ doom search headings
_H_ package homepage
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_L_ mode lineage
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_V_ custom variable
_w_ where is something defined
"
  ("A" doom/help-autodefs)
  ("b" describe-bindings)
  ("B" describe-personal-keybindings)
  ("C" describe-categories)
  ("c" describe-char)
  ("C" describe-coding-system)
  ("d" doom/help-modules)
  ("D" doom/help)
  ("f" describe-function)
  ("F" flycheck-describe-checker)
  ("h" doom/help-search-headings)
  ("H" doom/help-package-homepage)
  ("i" describe-input-method)
  ("K" describe-key)
  ("k" describe-key-briefly)
  ("l" describe-language-environment)
  ("L" help/parent-mode-display)
  ("M" describe-minor-mode)
  ("m" describe-mode)
  ("N" describe-current-coding-system)
  ("n" describe-current-coding-system-briefly)
  ("o" describe-minor-mode-from-indicator)
  ("O" describe-minor-mode-from-symbol)
  ;; ("p" describe-package)
  ("p" doom/help-packages)
  ("P" describe-text-properties)
  ("q" nil)
  ("a" help)
  ("s" describe-symbol)
  ("t" describe-theme)
  ("v" describe-variable)
  ("V" doom/help-custom-variable)
  ("w" where-is))
    (global-set-key (kbd "M-i") nil)
    (global-set-key (kbd "M-i") #'help/hydra/left/describe/body)

(after! parent-mode
  (defun help/parent-mode-display ()
    "Display this buffer's mode hierarchy."
    (interactive)
    (let ((ls (parent-mode-list major-mode)))
      (princ ls))))
    )

;; Change "Jane Joplin & John B Doe_" -> "Jane Joplin_ & Doe, John B"
(fset 'jw/swap_author
      (kmacro-lambda-form [?\M-b left ?\M-d ?\M-x ?s ?e ?a ?r ?c ?h ?- ?b ?a ?c ?k ?w ?a ?r ?d ?s backspace return ?& return ?\C-f ?\C-y ?, ?\M-b ?\M-b ?\M-f] 0 "%d"))

;; Replace "," with " &"
(fset 'jw/comma_to_ampersand
      (kmacro-lambda-form [?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?, return ?  ?& return] 0 "%d"))

(defun jw/skatt (utbetalt)
  "Given utbetalt calculate skatt, assuming 30% tax"
  (interactive)
  (/ utbetalt (- (/ 1.0 0.3) 1)))

(defun jw/skatt2 (fore)
  "Given before tax calculate payment and tax, assuming 30% tax"
  (interactive)
  (list fore (* fore 0.7) (* fore 0.3)))

(use-package! hledger-mode
  :defer t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  ;; :init
  ;; ;; To open files with .journal extension in hledger-mode
  ;; (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (:map hledger-mode-map
         ("C-c j" . hledger-run-command)
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  ;; Auto-completion for account names
  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company)))
  ;; Provide the path to you journal file.
  ;; The default location is too opinionated.
  (setq hledger-jfile "/home/jw/Dokument/hledger/pension/pension_2023.journal"))

(set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))

(use-package engine-mode
  :config
  (engine-mode t))

;; Note: This uses Company completion, so <F1> will display the candidates documentation.

(load "/home/jw/.roswell/lisp/quicklisp/clhs-use-local.el")
(load "/home/jw/.roswell/helper.el")
;; (setq common-lisp-hyperspec-root
;;       ;; “http://www.lispworks.com/reference/HyperSpec/&#8221;)
;;       "file:///home/jw/lisp/HyperSpec/")
;; (setq browse-url-browser-function ‘eww-browse-url)
;; (setq common-lisp-hyperspec-symbol-table "/home/jw/lisp/HyperSpec/Data/Map_Sym.txt")
;; block images in EWW browser
;; (setq-default shr-inhibit-images t)
;; (setq inferior-lisp-program "sbcl")
(setq sly-default-lisp 'roswell)
(setq ros-config "/home/jw/.roswell/ros-conf.lisp")
(setq sly-lisp-implementations
      `((sbcl ("sbcl") :coding-system utf-8-unix)
        (roswell ("ros" "-Q" "-l" ,ros-config "run"))
        (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
              :coding-system utf-8-unix)))

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

(use-package! xah-wolfram-mode
  :defer t)

(use-package! lexic
  :defer t
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search)))
  )

  (defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

(use-package! git-link
  :defer t)

(use-package! dwim-shell-command
  :defer t)

(use-package! persid
  :defer t
  )

(defun load-emacs-with-nyxt ()
  (interactive)
  (load "/home/jw/.config/doom/emacs-with-nyxt.el"))

(use-package! combobulate
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         ;; (typescript-ts-mode . combobulate-mode)
         ;; (tsx-ts-mode . combobulate-mode)
         ))

;; if you built from source
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (setq mu4e-compose-signature "Johan Widén, tel: +46705367346\nRisvägen 5 A, 192 73 Sollentuna, SWEDEN")
  (setq
    mu4e-sent-folder   "/gmail/[Gmail]/Skickat"       ;; folder for sent messages
    mu4e-drafts-folder "/gmail/[Gmail]/Utkast"        ;; unfinished messages
    mu4e-trash-folder  "/gmail/[Gmail]/Papperskorgen" ;; trashed messages
    mu4e-refile-folder "/gmail/[Gmail]/All e-post")   ;; saved messages
  (setq mu4e-maildir-shortcuts
        '((:maildir "/gmail/INBOX"                 :key . ?i)
          (:maildir "/gmail/[Gmail]/Skickat"       :key . ?s)
          (:maildir "/gmail/[Gmail]/Papperskorgen" :key . ?t)
          (:maildir "/gmail/[Gmail]/Utkast"        :key . ?d)
          (:maildir "/gmail/[Gmail]/All e-post"    :key . ?a)))
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        smtpmail-smtp-server "smtp.google.com"
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

(after! which-key
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-height 0.5)
  (setq which-key-side-window-max-width 0.5)
  ;; (setq which-key-allow-imprecise-window-fit nil)
  )

(use-package! vertico-flat
  ;; :bind (:map vertico-map
  ;;             ("M-q" . vertico-flat-mode))
  :after vertico)

(use-package! vertico-unobtrusive
  :after vertico-flat)

(use-package! vertico-multiform
  :commands vertico-multiform-mode
  :after vertico-flat
  :bind (:map vertico-map
              ("M-q" . vertico-multiform-flat)
              ("C-l" . my/vertico-multiform-unobtrusive)
              ("C-M-l" . embark-export))
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
         '((file my/vertico-grid-mode reverse)
           (jinx grid (vertico-grid-annotate . 20))
           (project-file my/vertico-grid-mode reverse)
           (imenu buffer)
           (consult-location buffer)
           (consult-grep buffer)
           ;; (notmuch-result reverse)
           (minor-mode reverse)
           ;; (reftex-label (:not unobtrusive))
           ;; (citar-reference reverse)
           (xref-location reverse)
           (history reverse)
           (url reverse)
           (consult-info buffer)
           (kill-ring reverse)
           (consult-compile-error reverse)
           ;; (buffer flat (vertico-cycle . t))
           (t buffer)))
  ;;  (setq vertico-multiform-commands
  ;;        '((jinx-correct reverse)
  ;;          (tab-bookmark-open reverse)
  ;;          (dired-goto-file unobtrusive)
  ;;          (load-theme my/vertico-grid-mode reverse)
  ;;          (my/toggle-theme my/vertico-grid-mode reverse)
  ;;          (org-refile reverse)
  ;;          (org-agenda-refile reverse)
  ;;          (org-capture-refile reverse)
  ;;          (affe-find reverse)
  ;;          (execute-extended-command unobtrusive)
  ;;          (dired-goto-file buffer)
  ;;          (consult-project-buffer buffer)
  ;;          (consult-dir-maybe reverse)
  ;;          (consult-dir reverse)
  ;;          (consult-flymake reverse)
  ;;          (consult-history reverse)
  ;;          (consult-completion-in-region reverse)
  ;;          (consult-recoll buffer)
  ;;          (citar-insert-citation reverse)
  ;;          (completion-at-point reverse)
  ;;          (org-roam-node-find reverse)
  ;;          (embark-completing-read-prompter reverse)
  ;;          (embark-act-with-completing-read reverse)
  ;;          (embark-prefix-help-command reverse)
  ;;          (embark-bindings reverse)
  ;;          (consult-org-heading reverse)
  ;;          (consult-dff unobtrusive)
  ;;          (embark-find-definition reverse)
  ;;          (xref-find-definitions reverse)
  ;;          (my/eshell-previous-matching-input reverse)
  ;;          (tmm-menubar reverse)))

   (defun my/vertico-multiform-unobtrusive ()
     "Toggle between vertico-unobtrusive and vertico-reverse."
     (interactive)
     (vertico-multiform-vertical 'vertico-reverse-mode)))

(use-package! vertico-grid
  :after vertico
  ;; :bind (:map vertico-map ("M-q" . vertico-grid-mode))
  :config
  (defvar my/vertico-count-orig vertico-count)
  (define-minor-mode my/vertico-grid-mode
    "Vertico-grid display with modified row count."
    :global t :group 'vertico
    (cond
     (my/vertico-grid-mode
      (setq my/vertico-count-orig vertico-count)
      (setq vertico-count 4)
      (vertico-grid-mode 1))
     (t (vertico-grid-mode 0)
        (setq vertico-count my/vertico-count-orig))))
  (setq vertico-grid-separator "    ")
  (setq vertico-grid-lookahead 50))

(use-package! vertico-reverse
  ;; :disabled
  :after vertico)

(use-package! vertico-buffer
  :after vertico
  ;; :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

(after! embark
  (global-set-key (kbd "C-:") 'embark-dwim)
  (global-set-key (kbd "C-*") 'embark-select)

  (define-key minibuffer-local-map (kbd "M-.") #'my-embark-preview)
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim))))))

(use-package! embark-consult
  :after (embark consult))

(after! consult
  ;; Consult has automatic preview for some (light weight) item types,
  ;; but if we want to preview for example bookmarks
  ;; we need to invoke this explicitly, using a key binding.
  ;; Also: Having a preview key binding for a command, turns off automatic preview for that command.
  ;; This is customized in doom config for vertico.
  ;; (consult-customize
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  :preview-key "M-.")
  (global-set-key (kbd "C-s") 'consult-line)

  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  ;; (defun consult-info-completion ()
  ;;   "Search through completion info pages."
  ;;   (interactive)
  ;;   (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
  ;;                 "corfu" "cape" "tempel"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "orderless" "embark" "company")))

(use-package! wgrep
  :defer t
  )

;; To turn off automatic in buffer completion:
;; (set (make-local-variable 'company-idle-delay) nil)

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

(setq browse-url-generic-program
      (or
       (executable-find (or (getenv "BROWSER") ""))
       (when (executable-find "xdg-mime")
         (let ((desktop-browser (ambrevar/call-process-to-string "xdg-mime" "query" "default" "text/html")))
           (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
       (executable-find browse-url-chrome-program)))

(use-package! ement
  :defer t
  )

(use-package! mistty
  :defer t
  :config
  (setq explicit-shell-file-name "/usr/bin/fish")
  ;; (setq explicit-shell-file-name "/usr/bin/bash")
  )

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
;; (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(use-package! igist
  :defer t
  :config
  (setq igist-current-user-name "johanwiden")
  (setq igist-auth-marker 'igist))

(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)
