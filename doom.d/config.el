;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setopt user-full-name "Johan Widén"
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
(global-set-key (kbd "C-å") 'evil-force-normal-state)
(global-set-key (kbd "C-ш") 'evil-force-normal-state)

(defun my/shell-command-on-file (command)
  "Execute COMMAND asynchronously on the current file."
  (interactive (list (read-shell-command
                      (concat "Async shell command on " (buffer-name) ": "))))
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (async-shell-command (concat command " " filename))))
(bind-key (kbd "C-M-&") #'my/shell-command-on-file)

(setopt doom-modeline-height 20)
  ;; mono 18, var 15
  ;; (setopt doom-font "Iosevka Comfy Fixed-15")

;;  (setopt doom-font (font-spec :family "Iosevka Comfy Fixed" :size 15)
        ;; doom-font (font-spec :family "Iosevka" :size 16)
        ;; doom-variable-pitch-font (font-spec :family "Iosevka Comfy" :size 13)
;;        doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :size 15)
        ;;doom-variable-pitch-font (font-spec :family "Overpass" :size 12)
        ;;doom-variable-pitch-font (font-spec :family "FiraGO" :size 15)
        ;;doom-variable-pitch-font (font-spec :family "Libre Baskerville" :height 1.0)
        ;;doom-serif-font (font-spec :family "Libre Baskerville" :height 1.0)
;;        )
  
  ;; (set-face-attribute 'default nil :font "Iosevka Comfy Fixed-15")
  ;; ;;(set-face-attribute 'default nil :font "Iosevka-16")
  ;; ;;(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
  ;; (set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy Fixed" :height 1.0)
  ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Duo" :height 1.0)
  ;; ;;(set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)
  ;; ;;(set-face-attribute 'variable-pitch nil :family "Libre Baskerville" :height 1.0)
  ;; (custom-set-faces!
  ;;   '(aw-leading-char-face
  ;;     :foreground "white" :background "red"
  ;;     :weight bold :height 2.5 :box (:line-width 10 :color "red")))
  (defun jw/default-font ()
    (interactive)
    (setopt doom-font (font-spec :family "Aporetic Sans Mono" :size 15 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 15))
    (doom/reload-font))
  (defun jw/small-font ()
    (interactive)
    (setopt doom-font (font-spec :family "Aporetic Sans Mono" :size 12 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 12))
    (doom/reload-font))
  (defun jw/regular-font ()
    (interactive)
    (setopt doom-font (font-spec :family "Aporetic Sans Mono" :size 15 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 15))
    (doom/reload-font))
  (defun jw/medium-font ()
    (interactive)
    (setopt doom-font (font-spec :family "Aporetic Sans Mono" :size 17 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 17))
    (doom/reload-font))
  (defun jw/large-font ()
    (interactive)
    (setopt doom-font (font-spec :family "Aporetic Sans Mono" :size 21 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Aporetic Sans" :size 21))
    (doom/reload-font))

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
    '(eldoc bar workspace-name window-number evil-state modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time))

  (doom-modeline-def-modeline 'minimal
    '(bar window-number evil-state modals matches buffer-info-simple)
    '(media-info major-mode time))

  (doom-modeline-def-modeline 'special
    '(eldoc bar window-number evil-state modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process time))

  (doom-modeline-def-modeline 'project
    '(bar window-number evil-state modals buffer-default-directory remote-host buffer-position)
    '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))

  (doom-modeline-def-modeline 'dashboard
    '(bar window-number evil-state modals buffer-default-directory-simple remote-host)
    '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))

  (doom-modeline-def-modeline 'vcs
    '(bar window-number evil-state modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(compilation misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process time))

  (doom-modeline-def-modeline 'package
    '(bar window-number evil-state modals package)
    '(compilation misc-info major-mode process time))

  (doom-modeline-def-modeline 'info
    '(bar window-number evil-state modals buffer-info info-nodes buffer-position parrot selection-info)
    '(compilation misc-info buffer-encoding major-mode time))

  (doom-modeline-def-modeline 'media
    '(bar window-number evil-state modals buffer-size buffer-info)
    '(compilation misc-info media-info major-mode process vcs time))

  (doom-modeline-def-modeline 'message
    '(eldoc bar window-number evil-state modals matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode time))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number evil-state modals matches buffer-info pdf-pages)
    '(compilation misc-info major-mode process vcs time))

  (doom-modeline-def-modeline 'org-src
    '(eldoc bar window-number evil-state modals matches buffer-info buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process check time))

  (doom-modeline-def-modeline 'helm
    '(bar evil-state helm-buffer-id helm-number helm-follow helm-prefix-argument)
    '(helm-help time))

  (doom-modeline-def-modeline 'timemachine
    '(eldoc bar window-number evil-state modals matches git-timemachine buffer-position word-count parrot selection-info)
    '(misc-info minor-modes indent-info buffer-encoding major-mode time))

  (doom-modeline-def-modeline 'calculator
    '(window-number evil-state modals matches calc buffer-position)
    '(misc-info minor-modes major-mode process))

  (setopt evil-normal-state-tag   (propertize "[Normal]" 'face '((:background "dark green" :foreground "black")))
        evil-emacs-state-tag    (propertize "[Emacs]" 'face '((:background "goldenrod" :foreground "black")))
        evil-insert-state-tag   (propertize "[Insert]" 'face '((:background "dark red") :foreground "white"))
        evil-motion-state-tag   (propertize "[Motion]" 'face '((:background "blue") :foreground "white"))
        evil-visual-state-tag   (propertize "[Visual]" 'face '((:background "grey80" :foreground "black")))
        evil-replace-state-tag  (propertize "[Replace]" 'face '((:background "yellow" :foreground "red")))
        evil-operator-state-tag (propertize "[Operator]" 'face '((:background "purple"))))

  ;; Enable mode-line in vterm
  (after! doom-modeline
    (add-to-list 'doom-modeline-mode-alist '(vterm-mode . main))
    (add-to-list 'doom-modeline-mode-alist '(shell-mode . main))
    (add-to-list 'doom-modeline-mode-alist '(eshell-mode . main)))

;; The concise one which relies on "implicit fallback values"
;; (setopt fontaine-presets
;;       '((tiny
;;          :default-family "Iosevka Comfy Wide Fixed"
;;          :default-height 70)
;;         (small
;;          :default-family "Iosevka Comfy Motion"
;;          :default-height 90)
;;         (regular)
;;         (source-code
;;          :default-family "Source Code Pro"
;;          :variable-pitch-family "Source Sans Pro"
;;          :default-height 110
;;          :bold-weight semibold)
;;         (medium
;;          :default-weight semilight
;;          :default-height 140
;;          :bold-weight extrabold)
;;         (large
;;          :inherit medium
;;          :default-height 180
;;          )
;;         (t ; our shared fallback properties
;;          :default-family "Iosevka Comfy"
;;          :default-weight regular
;;          :default-height 100
;;          :fixed-pitch-family nil ; falls back to :default-family
;;          :fixed-pitch-weight nil ; falls back to :default-family
;;          :fixed-pitch-serif-height 1.0
;;          :variable-pitch-family "Iosevka Comfy Motion Duo"
;;          :variable-pitch-weight nil
;;          ;; :variable-pitch-family "FiraGO"
;;          :variable-pitch-height 1.0
;;          :bold-family nil ; use whatever the underlying face has
;;          :bold-weight bold
;;          :italic-family nil
;;          :italic-slant italic
;;          :line-spacing nil)))

(use-package! fontaine
  :config
  (setopt fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://github.com/protesilaos/iosevka-comfy>.
  (setopt fontaine-presets
      '((small
         :default-family "Aporetic Sans Mono"
         :default-height 100
         :variable-pitch-family "Aporetic Sans")
        (regular) ; like this it uses all the fallback values and is named `regular'
        (medium
         :default-weight normal
         :default-height 130
         :bold-weight bold)
        (large
         :inherit medium
         :default-height 160)
        (presentation
         :default-height 180)
        (t
         ;; I keep all properties for didactic purposes, but most can be
         ;; omitted.  See the fontaine manual for the technicalities:
         ;; <https://protesilaos.com/emacs/fontaine>.
         :default-family "Aporetic Sans Mono"
         :default-weight normal
         :default-height 115

         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0

         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Aporetic Sans"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0

         :mode-line-active-family nil ; falls back to :default-family
         :mode-line-active-weight nil ; falls back to :default-weight
         :mode-line-active-height 0.9

         :mode-line-inactive-family nil ; falls back to :default-family
         :mode-line-inactive-weight nil ; falls back to :default-weight
         :mode-line-inactive-height 0.9

         :header-line-family nil ; falls back to :default-family
         :header-line-weight nil ; falls back to :default-weight
         :header-line-height 0.9

         :line-number-family nil ; falls back to :default-family
         :line-number-weight nil ; falls back to :default-weight
         :line-number-height 0.9

         :tab-bar-family nil ; falls back to :default-family
         :tab-bar-weight nil ; falls back to :default-weight
         :tab-bar-height 1.0

         :tab-line-family nil ; falls back to :default-family
         :tab-line-weight nil ; falls back to :default-weight
         :tab-line-height 1.0

         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold

         :italic-family nil
         :italic-slant italic

         :line-spacing nil)))

  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)

  ;; fontaine does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

(use-package! modus-themes
  :init
  ;; Starting with version 5.0.0 of the `modus-themes', other packages
  ;; can be built on top to provide their own "Modus" derivatives.
  ;; For example, this is what I do with my `ef-themes' and
  ;; `standard-themes' (starting with versions 2.0.0 and 3.0.0,
  ;; respectively).
  ;;
  ;; The `modus-themes-include-derivatives-mode' makes all Modus
  ;; commands that act on a theme consider all such derivatives, if
  ;; their respective packages are available and have been loaded.
  ;;
  ;; Note that those packages can even completely take over from the
  ;; Modus themes such that, for example, `modus-themes-rotate' only
  ;; goes through the Ef themes (to this end, the Ef themes provide
  ;; the `ef-themes-take-over-modus-themes-mode' and the Standard
  ;; themes have the `standard-themes-take-over-modus-themes-mode'
  ;; equivalent).
  ;;
  ;; If you only care about the Modus themes, then (i) you do not need
  ;; to enable the `modus-themes-include-derivatives-mode' and (ii) do
  ;; not install and activate those other theme packages.
  (modus-themes-include-derivatives-mode 1)

  :config
   (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'modus-operandi)
  (setopt custom-safe-themes t)
  (load-theme 'modus-vivendi-tinted)
  (setopt doom-theme 'modus-vivendi-tinted)
  ;; (load-theme 'modus-vivendi)
  ;; (setopt doom-theme 'modus-vivendi)
  ;; :bind ("<f5>" . modus-themes-rotate)
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
 switch-to-buffer-obey-display-actions t
 ;; Change this from 10MB to 100MB
 large-file-warning-threshold 5000000000
 show-paren-context-when-offscreen 'overlay
 shr-color-visible-luminance-min 80)
(customize-set-variable 'user-emacs-directory "/home/jw/bookmarks/cache/")
(customize-set-variable 'projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory))
(customize-set-variable 'projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
(customize-set-variable 'fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" user-emacs-directory))
(setopt doom-cache-dir user-emacs-directory)
(setopt doom-profile-cache-dir user-emacs-directory)
(customize-set-variable 'bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
(customize-set-variable 'bookmark-save-flag 1) ; Save bookmark list immediately when it has been updated.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(after! recentf
  (progn
    (setopt recentf-max-saved-items 10000)
    (add-hook 'find-file-hook 'recentf-save-list)))
(after! savehist
  (setopt savehist-autosave-interval 600))
;; (auto-save-mode -1)
(require 'saveplace-pdf-view)
(save-place-mode 1)
(setopt use-package-verbose t)
(tooltip-mode 1)
(add-hook 'text-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'prog-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'mistty-mode-hook (lambda () (visual-line-mode 1)))
(after! auth-source
  (add-to-list 'auth-sources "secrets:Login"))
(customize-set-variable 'immersive-translate-chatgpt-model "gpt-4o")
(setopt magit-format-file-function #'magit-format-file-nerd-icons)
(setopt tab-bar-format
        '(tab-bar-format-menu-bar tab-bar-format-history tab-bar-format-tabs-groups
          tab-bar-separator tab-bar-format-add-tab))
(map! :map (shrface-mode-map wallabag-entry-mode-map nov-mode-map eww-mode-map mu4e-view-mode-map elfeed-show-mode-map)
      :n "TAB" 'shrface-outline-cycle
      :n "<tab>" 'shrface-outline-cycle
      :n "<backtab>" 'shrface-outline-cycle-buffer
      :localleader
      "k" 'shrface-previous-headline
      "j" 'shrface-next-headline
      "l" 'shrface-links-consult
      "i" 'imenu-list
      "h" 'shrface-headline-consult
      "o" 'shrface-occur
      "b" 'shrface-toggle-bullets
      ;; if you use paw.el, you can add some paw functions as well
      "a" 'paw-show-all-annotations
      "r" 'paw-clear-annotation-overlay
      "c" 'paw-add-annotation
      "," 'paw-list-annotations
)

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
       (setopt defs (cddr defs)))))

(setopt hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        ;; try-expand-line
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(map! [remap dabbrev-expand] #'hippie-expand)

(setopt org-log-done 'time)
(setopt org-log-into-drawer t)
(setopt org-directory "~/org/")
(setopt org-attach-id-dir "~/org/attachments/")
;; Learn about then ! and more by reading the relevant section of the Org manual.
;; Evaluate: (info "(org) Tracking TODO state changes")

(after! org
  (progn
    (setopt org-use-speed-commands t)
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
    (setopt org-journal-date-prefix "#+TITLE: "
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
  ;; (setopt org-babel-load-languages '((emacs-lisp . t)
  ;;                                  (ledger . t)
  ;;                                  (python . t)
  ;;                                  (shell . t)  ; in my case /bin/bash
  ;;                                  ))
  )

(after! org
  (require 'ox-gfm nil t))

(use-package! org-books
    :after org
    :config
    (setopt org-books-file "~/Dokument/Böcker/org-books.org"))

(setopt org-roam-v2-ack t)
(setopt org-roam-directory (file-truename "~/org/roam/")
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
    (setopt org-roam-ui-sync-theme t
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
  (setopt org-similarity-directory org-roam-directory)
  )

(use-package! org-tempo
 :after org
  )

(use-package! org-transclusion
  :after org
  :defer t
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(setopt display-line-numbers-type nil)

(setq! citar-bibliography '("/home/jw/Dokument/Böcker/mylibrary.bib"))
(setq! ews-bibtex-directory "/home/jw/Dokument/Böcker")
;; (after! citar
;;   (progn
;;     (setq! citar-bibliography '("/home/jw/Dokument/Böcker/my-library.bib"))
;;     ))

(use-package! citar-denote
  :after (citar denote)
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))

(after! denote
  (load "/home/jw/projects/emacs/emacs-writing-studio/ews.el"))

(after! (bibtex denote)
  (progn
    (setq bibtex-user-optional-fields
          '(("keywords" "Keywords to describe the entry" "")
            ("file"     "Relative or absolute path to attachments" "" )))
    (ews-bibtex-register)
    (global-set-key (kbd "C-c w b r") 'ews-bibtex-register)
    (global-set-key (kbd "C-c w b o") 'citar-open)
    (global-set-key (kbd "C-c w h") 'consult-org-heading)
    (global-set-key (kbd "C-c w g") 'consult-grep)
    (keymap-set org-mode-map "C-c w n" #'ews-org-insert-notes-drawer)
    (keymap-set org-mode-map "C-c w p" #'ews-org-insert-screenshot)
    (keymap-set org-mode-map "C-c w c" #'ews-org-count-words)
    ;; Export citations with Org Mode
    (require 'oc-natbib)
    (require 'oc-csl)
    ))

;; Biblio package for adding BibTeX records
(use-package! biblio
  :after (bibtex denote)
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Easy insertion of weblinks
(use-package! org-web-tools
  :after (bibtex denote)
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Open files with external applications
(use-package openwith
  :after denote
  :config
  (openwith-mode t)
  :custom
  (openwith-associations nil))

;; Writegood-Mode for weasel words, passive writing and repeated word detection
(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  ;; :hook
  ;; (text-mode . writegood-mode)
  )

(setopt epkg-repository "~/epkgs/")

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

(use-package! pcre2el
  :config
  (pcre-mode t))

(after! avy
  (setopt avy-all-windows t)
  (setq avy-single-candidate-jump nil)
  ;; Avoid collision with action keys
  (setopt avy-keys '(?a ?s ?d ?f ?g ?h ?j ?e ?l))
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
(after! evil
  (progn
    (define-key evil-insert-state-map (kbd "S-<right>") nil)
    (define-key evil-insert-state-map (kbd "S-<left>") nil)))
;; (global-set-key (kbd "<kp-4>") 'windmove-left)
;; (global-set-key (kbd "<kp-6>") 'windmove-right)
;; (global-set-key (kbd "<kp-8>") 'windmove-up)
;; (global-set-key (kbd "<kp-2>") 'windmove-down)

(setopt ibuffer-saved-filter-groups
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
(setopt ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(use-package! casual-ibuffer
  :ensure nil
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
  :after (ibuffer))

(use-package! casual-re-builder
  :ensure nil
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  :after (re-builder))

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

(after! dired
  (progn
    (defun my-dired-init ()
      "Bunch of stuff to run for dired, either immediately or when it's loaded."
      (bind-keys :map dired-mode-map
                 ("<delete>" . dired-unmark-backward)
                 ("<backspace>" . dired-up-directory))

      ;; (dired-filter-mode t)
      ;; (dired-filter-group-mode t)
      ;; (dired-collapse-mode 1)
      (visual-line-mode -1)
      (toggle-truncate-lines 1))
    (add-hook 'dired-mode-hook 'my-dired-init)))

(use-package! casual-suite
  :config
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu))

(use-package! casual-dired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
              ("C-c C-o" . casual-dired-tmenu)
              ("C-c s" . #'casual-dired-sort-by-tmenu)
              ("C-c /" . #'casual-dired-search-replace-tmenu)))

;; (use-package! bookmark+
;;   :after dired
;;   ;;:defer t
;;   :config
;;   (setopt bmkp-last-as-first-bookmark-file nil))

(use-package! casual-bookmarks
  :ensure nil
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after dired)

(setq eshell-prompt-regexp "^.* λ ") ;; modify according to your prompt

(add-hook 'eshell-mode-hook 'outline-minor-mode)
(add-hook 'eshell-mode-hook (lambda () (setq-local outline-regexp (concat eshell-prompt-regexp ".*"))))
(add-hook 'eshell-mode-hook (lambda ()
           (setq-local imenu-generic-expression `(("Prompt" ,(concat eshell-prompt-regexp "\\(.*\\)") 1)))))

(use-package! rainbow-delimiters
  ;; :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package! casual-editkit
  :ensure nil
  :defer t
  :bind (("C-o" . casual-editkit-main-tmenu)))

(use-package! casual-calendar
  :ensure nil
  :defer t
  :bind (:map calendar-mode-map
              ("C-o" . casual-calendar)))

(use-package! w3m
  :defer t
  :config
  (setopt w3m-key-binding 'info)
   (define-key w3m-mode-map [up] 'previous-line)
   (define-key w3m-mode-map [down] 'next-line)
   (define-key w3m-mode-map [left] 'backward-char)
   (define-key w3m-mode-map [right] 'forward-char)
  (setopt w3m-default-display-inline-images t)
  (setopt w3m-make-new-session t)
  (setopt w3m-use-cookies t)
  (setopt w3m-default-save-directory "~/Downloads/")
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
;; (setopt browse-url-browser-function 'w3m-browse-url)
(setopt browse-url-new-window-flag t)
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
  :hook
  (elfeed-show-mode . paw-annotation-mode)
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
  ;; (add-hook 'elfeed-show-mode-hook #'org-indent-mode)
  (add-hook 'elfeed-show-mode-hook #'eldoc-mode)
  (add-hook 'elfeed-show-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'elfeed-show-mode-hook #'shrface-wallabag-setup)

  (advice-add 'elfeed-insert-html :around #'shrface-elfeed-advice)

  (require 'shrface)

  (defun shrface-wallabag-setup ()
    (unless shrface-toggle-bullets
      (shrface-regexp)
      (setq-local imenu-create-index-function #'shrface-imenu-get-tree))
    (if (string-equal system-type "android")
        (setq-local touch-screen-enable-hscroll nil))
    ;; (add-function :before-until (local 'eldoc-documentation-function) #'paw-get-eldoc-note)
    )
  (defun shrface-elfeed-advice (orig-fun &rest args)
    (require 'eww)
    (let ((shrface-org nil)
          (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
          ;; make it large enough, it would not fill the column
          ;; I uses visual-line-mode, writeroom-mode for improving the reading experience instead
          (shr-width 7000)
          (shr-indentation 3)
          (shr-table-vertical-line "|")
          (shr-external-rendering-functions
           (append '((title . eww-tag-title)
                     (form . eww-tag-form)
                     (input . eww-tag-input)
                     (button . eww-form-submit)
                     (textarea . eww-tag-textarea)
                     (select . eww-tag-select)
                     (link . eww-tag-link)
                     (meta . eww-tag-meta)
                     ;; (a . eww-tag-a)
                     (code . shrface-tag-code)
                     (pre . shrface-shr-tag-pre-highlight))
                   shrface-supported-faces-alist))
          (shrface-toggle-bullets nil)
          (shrface-href-versatile t)
          (shr-use-fonts nil))
      (apply orig-fun args)
      (with-current-buffer "*elfeed-entry*"
        (when (bound-and-true-p paw-annotation-mode)
          (paw-clear-annotation-overlay)
          (paw-show-all-annotations)
          (if paw-annotation-show-wordlists-words-p
              (paw-focus-find-words :wordlist t))
          (if paw-annotation-show-unknown-words-p
              (paw-focus-find-words))) )))

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star)))

(use-package! elfeed-org
  :after elfeed
  :init
  (setopt rmh-elfeed-org-files (list "~/.config/doom/elfeed.org"))
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
  ;; :hook
  ;; (nov-mode . paw-annotation-mode)
  :bind
  (:map nov-mode-map
        ("<home>" . move-beginning-of-line)
        ("<end>" . move-end-of-line))
  :config
  (setq nov-text-width 80)
  ;; (setq visual-fill-column-center-text t)
  ;; (add-hook 'nov-mode-hook 'visual-line-mode)
  ;; (add-hook 'nov-mode-hook 'visual-fill-column-mode)

  ;; (add-hook 'nov-mode-hook #'eldoc-mode)
  ;; ;; (add-hook 'nov-mode-hook #'org-indent-mode)
  ;; (add-hook 'nov-mode-hook #'eldoc-box-hover-mode)
  ;; (add-hook 'nov-mode-hook #'shrface-nov-setup)
  ;; (require 'shrface)
  ;; ;; (define-key nov-mode-map (kbd "C-c C-l") 'shrface-links-consult)
  ;; ;; (define-key nov-mode-map (kbd "C-c C-h") 'shrface-headline-consult)
  ;; (setopt nov-render-html-function #'my-nov-render-html)
  ;; ;; (advice-add 'my-nov-visit-relative-file :override #'nov-visit-relative-file)
  ;; (advice-add 'shr--remove-blank-lines-at-the-end :override #'my-shr--remove-blank-lines-at-the-end)
  )

;; (defun my-nov-render-html ()
;;   (require 'eww)
;;   (let ((shrface-org nil)
;;         (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
;;         (shr-table-vertical-line "|")
;;         (shr-width 7000) ;; make it large enough, it would not fill the column (use visual-line-mode/writeroom-mode instead)
;;         (shr-indentation 3) ;; remove all unnecessary indentation
;;         (tab-width 8)
;;         (shr-external-rendering-functions
;;          (append '((img . nov-render-img)
;;                    (svg . nov-render-svg)
;;                    (title . nov-render-title)
;;                    (pre . shrface-shr-tag-pre-highlight)
;;                    (code . shrface-tag-code)
;;                    (form . eww-tag-form)
;;                    (input . eww-tag-input)
;;                    (button . eww-form-submit)
;;                    (textarea . eww-tag-textarea)
;;                    (select . eww-tag-select)
;;                    (link . eww-tag-link)
;;                    (meta . eww-tag-meta))
;;                  shrface-supported-faces-alist))
;;         (shrface-toggle-bullets nil)
;;         (shrface-href-versatile t)
;;         (shr-use-fonts nil)           ; nil to use default font
;;         (shr-map nov-mode-map))

;;     ;; HACK: `shr-external-rendering-functions' doesn't cover
;;     ;; every usage of `shr-tag-img'
;;     (cl-letf (((symbol-function 'shr-tag-img) 'nov-render-img))
;;       (shr-render-region (point-min) (point-max)))

;;     ;; workaround, show annotations when document updates
;;     (when (bound-and-true-p paw-annotation-mode)
;;       (paw-clear-annotation-overlay)
;;       (paw-show-all-annotations)
;;       (if paw-annotation-show-wordlists-words-p
;;           (paw-focus-find-words :wordlist t))
;;       (if paw-annotation-show-unknown-words-p
;;           (paw-focus-find-words)))))

;; (defun my-shr--remove-blank-lines-at-the-end (start end)
;;   "A fix for `shr--remove-blank-lines-at-the-end' which will remove image at the end of the document."
;;   (save-restriction
;;     (save-excursion
;;       (narrow-to-region start end)
;;       (goto-char end)
;;       (when (and (re-search-backward "[^ \n]" nil t)
;;                  (not (eobp)))
;;         (forward-line 1)
;;         (delete-region (point) (min (1+ (point)) (point-max)))))))

;; (defun shrface-nov-setup ()
;;   (unless shrface-toggle-bullets
;;     (shrface-regexp))
;;   (set-visited-file-name nil t)
;;   (setq tab-width 8)
;;   (if (string-equal system-type "android")
;;       (setq-local touch-screen-enable-hscroll nil))
;;   ;; (add-function :before-until (local 'eldoc-documentation-function) #'paw-get-eldoc-note)
;;   )

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
  (advice-add 'calibredb-show-entry :around #'shrface-calibredb-advice)
  (defun shrface-calibredb-advice (orig-fun &rest args)
    (require 'eww)
    (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-width 60)
        (shr-indentation 3)
        (shr-table-vertical-line "|")
        (shr-external-rendering-functions
         (append '((title . eww-tag-title)
                   (form . eww-tag-form)
                   (input . eww-tag-input)
                   (button . eww-form-submit)
                   (textarea . eww-tag-textarea)
                   (select . eww-tag-select)
                   (link . eww-tag-link)
                   (meta . eww-tag-meta)
                   ;; (a . eww-tag-a)
                   (code . shrface-tag-code)
                   (pre . shrface-shr-tag-pre-highlight))
                 shrface-supported-faces-alist))
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil))
    (apply orig-fun args)))
  (setopt sql-sqlite-program "/usr/bin/sqlite3")
  (setopt calibredb-program "/usr/bin/calibredb")
  (setopt calibredb-root-dir (expand-file-name "~/calibre_library"))
  (setopt calibredb-db-dir (concat calibredb-root-dir "/metadata.db"))
  ;; (setopt calibredb-library-alist '(("~/calibre_library")))
  (setopt calibredb-date-width 0)
  (setopt calibredb-download-dir (expand-file-name "~/Downloads"))
  (setq calibredb-virtual-library-alist '(("1. cpp" . "cpp")
                                          ("2. fantasy" . "fantasy")
                                          ("3. fiction" . "lit-fiction")
                                          ("4. manual" . "manual")
                                          ("5. physics" . "physics")
                                          ("6. tensor" . "tensor-calc")
                                          ("7. science fiction" . "scifi")))
  (setopt calibredb-library-alist '(("/home/jw/calibre_library" (name . "Calibre"))
                                  ("https://bookserver.archive.org/catalog/" (name . "archive"))
                                  ("http://arxiv.maplepop.com/catalog/" (name . "arxiv"))
                                  ("https://m.gutenberg.org/ebooks.opds/" (name . "Gutenberg"))
                                  )))

(after! eww
  (progn
    (require 'shrface)
    ;; (define-key eww-mode-map (kbd "C-c C-l") 'shrface-links-consult)
    ;; (define-key eww-mode-map (kbd "C-c C-h") 'shrface-headline-consult)
    (advice-add 'eww-display-html :around #'shrface-eww-advice)
    ;; (add-hook 'eww-after-render-hook #'org-indent-mode)
    (add-hook 'eww-after-render-hook #'eldoc-mode)
    (add-hook 'eww-after-render-hook #'eldoc-box-hover-mode)
    (add-hook 'eww-after-render-hook #'shrface-eww-setup)
    (defun shrface-eww-setup ()
      (unless shrface-toggle-bullets
        (shrface-regexp)
        (setq-local imenu-create-index-function #'shrface-imenu-get-tree))
      ;; (add-function :before-until (local 'eldoc-documentation-function) #'paw-get-eldoc-note)
      ;; workaround to show annotations in eww
      (when (bound-and-true-p paw-annotation-mode)
        (paw-clear-annotation-overlay)
        (paw-show-all-annotations)
        (if paw-annotation-show-wordlists-words-p
            (paw-focus-find-words :wordlist t))
        (if paw-annotation-show-unknown-words-p
          (paw-focus-find-words))))

    (defun shrface-eww-advice (orig-fun &rest args)
      (require 'eww)
      (let ((shrface-org nil)
            (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
            (shr-table-vertical-line "|")
            (shr-width 65)
            (shr-indentation 3)
            (shr-external-rendering-functions
             (append '((title . eww-tag-title)
                       (form . eww-tag-form)
                       (input . eww-tag-input)
                       (button . eww-form-submit)
                       (textarea . eww-tag-textarea)
                       (select . eww-tag-select)
                       (link . eww-tag-link)
                       (meta . eww-tag-meta)
                       ;; (a . eww-tag-a)
                       (code . shrface-tag-code)
                       (pre . shrface-shr-tag-pre-highlight))
                     shrface-supported-faces-alist))
            (shrface-toggle-bullets nil)
            (shrface-href-versatile t)
            (shr-use-fonts nil))
        (apply orig-fun args)))))

(use-package! mixed-pitch)

(use-package! hyperbole
  :defer t
  :config
  ;; (require 'hyperbole)
  (hyperbole-mode 1)
  (setopt hsys-org-enable-smart-keys t)
  ;; (global-set-key (kbd "S-s-<return>") 'hkey-either)
  ;; (global-set-key (kbd "s-S") 'assist-key)
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

(use-package! major-mode-hydra
  ;; :defer t
  :bind
  ("M-SPC" . major-mode-hydra))

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(pretty-hydra-define medusa/denote
  (:color blue :quit-key "<escape>" :title "Denote")
  ("Create" (
    ("n" denote "_n_ew note" )
    ("t" denote-type "other _t_ype" )
    ("d" denote-date  "other _d_ate" )
    ("s" denote-subdirectory  "other _s_ubdir" )
    ("T" denote-template  "with _T_emplate" )
    ("S" denote-signature  "with _S_ignature" ))
   "Link" (
    ("l" denote-link-or-create "_l_ink" )
    ("L" denote-link-or-create-with-command "_L_ink with command" )
    ("h" denote-org-link-to-heading  "specific _h_eader" )
    ("r" denote-add-links "by _r_egexp" )
    ("d" denote-add-links "by _d_ired" )
    ("b" denote-backlinks "_b_acklinks" ))
   "Rename" (
    ("RF" denote-rename-file "Rename File")
    ("FT" denote-change-file-type-and-front-matter  "only FileType")
    ("UF" denote-rename-file-using-front-matter "use Frontmatter"))
   "Dyn. Block" (
    ("DL" denote-org-dblock-insert-links "dyn. Links" )
    ("DB" denote-org-dblock-insert-backlinks "dyn. Backlinks" ))
   "Convert links" (
    ("CF" denote-org-convert-links-to-file-type "to File Type" )
    ("CD" denote-org-convert-links-to-denote-type "to Denote Type" ))
  "Other" (
     ("?" (info "denote") "Help")
     ("M-SPC" major-mode-hydra "Major Mode Hydra"))))

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
    ;; (global-set-key (kbd "M-i") nil)
    ;; (global-set-key (kbd "M-i") #'help/hydra/left/describe/body)

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

(defun jw/skatt3 (fore skatt-procent)
  "Given before tax calculate payment and tax, assuming skatt-procent tax"
  (interactive)
  (list fore (* fore (- 1.0 skatt-procent)) (* fore skatt-procent)))

(defun jw/copy-file-content-to-clipboard () ; From https://emacs.stackexchange.com/questions/64982/copy-a-file-content-to-clipboard-with-dired/64993#64993
  "Copy content of file selected in dired, to clipboard"
  (interactive)
  (let ((buf  (find-file-noselect (dired-get-file-for-visit))))
    (with-current-buffer buf
      (kill-new (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer buf)))

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
  ;; (add-hook 'hledger-mode-hook
  ;;           (lambda ()
  ;;             (make-local-variable 'company-backends)
  ;;             (add-to-list 'company-backends 'hledger-company)))
  ;; Provide the path to you journal file.
  ;; The default location is too opinionated.
  (setopt hledger-jfile "/home/jw/Dokument/hledger/pension/pension_2023.journal"))

(set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))

;; Note: This uses Company completion, so <F1> will display the candidates documentation.

(load "/home/jw/.roswell/lisp/quicklisp/clhs-use-local.el")
(load "/home/jw/.roswell/helper.el")
;; (setopt common-lisp-hyperspec-root
;;       ;; “http://www.lispworks.com/reference/HyperSpec/&#8221;)
;;       "file:///home/jw/lisp/HyperSpec/")
;; (setopt browse-url-browser-function ‘eww-browse-url)
;; (setopt common-lisp-hyperspec-symbol-table "/home/jw/lisp/HyperSpec/Data/Map_Sym.txt")
;; block images in EWW browser
;; (setq-default shr-inhibit-images t)
;; (setopt inferior-lisp-program "sbcl")
(setopt sly-default-lisp 'roswell)
(setopt ros-config "/home/jw/.roswell/ros-conf.lisp")
(setopt sly-lisp-implementations
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

(use-package! persid
  :defer t
  )

(defun load-emacs-with-nyxt ()
  (interactive)
  (load "/home/jw/.config/doom/emacs-with-nyxt.el"))

;; if you built from source
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  ;; (setopt mu4e-compose-signature "Johan Widén, tel: +46705367346\nRisvägen 5 A, 192 73 Sollentuna, SWEDEN")
(setq mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail"     ;; folder for sent messages
      mu4e-drafts-folder "/gmail/[Gmail]/Drafts"        ;; unfinished messages
      mu4e-trash-folder  "/gmail/[Gmail]/Bin" ;; trashed messages
      mu4e-refile-folder "/gmail/[Gmail]/All Mail")     ;; saved messages
(setopt mu4e-maildir-shortcuts
        '((:maildir "/gmail/INBOX"                 :key ?i)
          (:maildir "/gmail/[Gmail]/Sent Mail"     :key ?s)
          (:maildir "/gmail/[Gmail]/Bin" :key ?t)
          (:maildir "/gmail/[Gmail]/Drafts"        :key ?d)
          (:maildir "/gmail/[Gmail]/All Mail"      :key ?a)))
(after! mu4e
  (progn
    (setopt sendmail-program (executable-find "msmtp")
            send-mail-function #'smtpmail-send-it
            smtpmail-smtp-server "smtp.google.com"
            message-sendmail-f-is-evil t
            message-sendmail-extra-arguments '("--read-envelope-from")
            message-send-mail-function #'message-send-mail-with-sendmail)
    ;; (define-key mu4e-view-mode-map (kbd "C-c C-l") 'shrface-links-consult)
    ;; (define-key mu4e-view-mode-map (kbd "C-c C-h") 'shrface-headline-consult)
    (advice-add 'mu4e-shr2text :around #'shrface-mu4e-advice)
    (defun shrface-mu4e-advice (orig-fun &rest args)
      (require 'eww)
      (let ((shrface-org nil)
            (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
            (shr-table-vertical-line "")
            (shr-width 90)
            (shr-indentation 3)
            (shr-external-rendering-functions
             (append '((title . eww-tag-title)
                       (form . eww-tag-form)
                       (input . eww-tag-input)
                       (button . eww-form-submit)
                       (textarea . eww-tag-textarea)
                       (select . eww-tag-select)
                       (link . eww-tag-link)
                       (meta . eww-tag-meta)
                       ;; (a . eww-tag-a)
                       (code . shrface-tag-code)
                       (pre . shrface-shr-tag-pre-highlight))
                     shrface-supported-faces-alist))
            (shrface-toggle-bullets nil)
            (shrface-href-versatile t)
            (shr-use-fonts nil))
        (apply orig-fun args)))))

(after! which-key
  (setopt which-key-side-window-location 'right)
  (setopt which-key-side-window-max-height 0.5)
  (setopt which-key-side-window-max-width 0.5)
  ;; (setopt which-key-allow-imprecise-window-fit nil)
  (which-key-add-key-based-replacements
    "C-c w"   "Emacs Writing Studio"
    "C-c w b" "Bibliographic"
    "C-c w d" "Denote"
    "C-c w m" "Multimedia"
    "C-c w s" "Spelling and Grammar"
    "C-c w t" "Themes"
    "C-c w x" "Explore")
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
  (setopt vertico-multiform-categories
         '((file my/vertico-grid-mode reverse)
           (jinx grid (vertico-grid-annotate . 20))
           (project-file my/vertico-grid-mode reverse)
           (imenu buffer)
           (consult-location buffer)
           (consult-grep buffer)
           (embark-keybinding grid)
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
  ;;  (setopt vertico-multiform-commands
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
  (setopt vertico-grid-separator "    ")
  (setopt vertico-grid-lookahead 50))

(use-package! vertico-reverse
  ;; :disabled
  :after vertico)

(use-package! vertico-buffer
  :after vertico
  ;; :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (setopt vertico-buffer-display-action 'display-buffer-reuse-window))

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

  (defun show-narrow-help (&rest _)
    (cl-letf (((symbol-function #'minibuffer-message) #'message))
      (consult-narrow-help)))
  (advice-add #'consult--narrow-setup :after #'show-narrow-help)

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
    (consult-info "orderless" "embark" "company"))

  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))

  (defalias 'consult-line-thing-at-point 'consult-line)

  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol)))

(use-package! wgrep
  :defer t)

(use-package! denote
  :after consult
  ;; :defer t
  :config
  ;; Remember to check the doc strings of those variables.
  ;; (setopt denote-directory (expand-file-name "~/org/roam/notes/"))
  (setopt denote-directory (expand-file-name "~/Sync/notes/"))
  (setq denote-save-buffer-after-creation nil)
  (setopt denote-known-keywords '("async" "bestpractices" "emacs" "consult" "gravity" "inertia" "life" "llm" "notes" "orgroam" "philosophy" "physics" "politics" "template" "economics"))
  (setopt denote-infer-keywords t)
  (setopt denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setopt denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setopt denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (setopt denote-org-store-link-to-heading 'id)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setopt denote-date-prompt-use-org-read-date t)

  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.

  (setq denote-date-format nil) ; read doc string

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'text-mode-hook #'denote-fontify-links-mode)
  ;; denote 3.0.0
  ;; (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

  ;; We use different ways to specify a path for demo purposes.
  (setopt denote-dired-directories
          (list denote-directory
                (thread-last denote-directory (expand-file-name "attachments"))
                (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (let ((map global-map))
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n t") #'denote-template)
    (define-key map (kbd "C-c w d b") #'denote-find-backlink)
    (define-key map (kbd "C-c w d d") #'denote-date)
    (define-key map (kbd "C-c w d l") #'denote-find-link)
    (define-key map (kbd "C-c w d i") #'denote-link-or-create)
    (define-key map (kbd "C-c w d k") #'denote-rename-file-keywords)
    (define-key map (kbd "C-c w d n") #'denote)
    (define-key map (kbd "C-c w d r") #'denote-rename-file)
    (define-key map (kbd "C-c w d R") #'denote-rename-file-using-front-matter)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-add-links)
    (define-key map (kbd "C-c n b") #'denote-backlinks)
    (define-key map (kbd "C-c n f f") #'denote-find-link)
    (define-key map (kbd "C-c n f b") #'denote-find-backlink)
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
    (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
    (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

  (with-eval-after-load 'org-capture
    (setopt denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well.


  ;; If you want to have Denote commands available via a right click
  ;; context menu, use the following and then enable
  ;; `context-menu-mode'.
  (add-hook 'context-menu-functions #'denote-context-menu)

  ;; Transient setup for Denote
  ;; https://gist.github.com/ashton314/1de93821d255412cdadfbcf98cd30cad
  (transient-define-prefix denote-transient ()
    "Denote dispatch"
    [["Note creation (d)"
      ("dd" "new note" denote)
      ("dj" "new or existing journal entry" denote-journal-new-or-existing-entry)
      ("dn" "open or new" denote-open-or-create)
      ("dt" "new specifying date and time" denote-date)
      ("ds" "create in subdirectory " denote-subdirectory)]
     ["Folgezettel (f)"
      ("fc" "create parent/child/sibling" denote-sequence)
      ("ff" "find parent/child/sibling notes" denote-sequence-find)
      ("fr" "reparent (adopt) current note into another sequence" denote-sequence-reparent)
      ("fp" "find previous sibling" denote-sequence-find-previous-sibling :transient t)
      ("fn" "find next sibling" denote-sequence-find-next-sibling :transient t)]]
    [["Bookkeeping (b)"
      ("br" "prompt and rename" denote-rename-file)
      ("bf" "rename with frontmatter" denote-rename-file-using-front-matter)
      ("bk" "modify keywords" denote-rename-file-keywords)]
     ["Linking (l)"
      ("li" "insert link" denote-link)
      ("lh" "insert link to org heading" denote-org-link-to-heading)
      ("lb" "show backlinks" denote-backlinks)
      ("lg" "visit backlink" denote-find-backlink)
      ("lo" "org backlink block" denote-org-dblock-insert-backlinks)]]
    [["Searching (s)"
      ("sd" "deft" deft)
      ("sn" "consult-notes" consult-notes)
      ("ss" "consult-notes search" consult-notes-search-in-all-notes)]]))

(use-package! denote-org
  :after denote
  ;; :defer t
  :bind
  (("C-c w d h" . denote-org-link-to-heading)))

(define-minor-mode denote-mode
  "Denote is a simple note-taking tool for Emacs."
  :lighter " Note"
  :keymap (let ((map
                 (make-sparse-keymap)))
            (define-key map (kbd "C-l") 'denote-link-or-create)
            (define-key map (kbd "C-n") 'denote-link-after-creating)
            (define-key map (kbd "<f6>")(lambda () (interactive) (find-file "~/Sync/notes")))
            map))

(use-package! consult-denote
  :after denote
  ;; :defer t
  :config
  (consult-denote-mode 1))

(use-package! consult-notes
  :after denote
  ;; :defer t
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :bind
  (("C-c w d f" . consult-notes)
   ("C-c w d g" . consult-notes-search-in-all-notes))
  :config
  (setopt consult-notes-file-dir-sources `(("Denote"  ?d  ,(denote-directory))
                                         ("Org"     ?o  "~/org/roam"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  ;; (setopt consult-notes-org-headings-files '("~/path/to/file1.org"
  ;;                                          "~/path/to/file2.org"))
  ;; (consult-notes-org-roam-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; ;; search only for text files in denote dir
  ;; (setopt consult-notes-denote-files-function (function denote-directory-text-only-files))
  )

(use-package! denote-menu
  :after denote
  ;; :defer t
  :commands (list-denotes
             denote-menu-clear-filters
             denote-menu-filter
             denote-menu-filter-by-keyword
             denote-menu-filter-out-keyword
             denote-menu-export-to-dired)
  :config
  (global-set-key (kbd "C-c z") #'list-denotes)

  (define-key denote-menu-mode-map (kbd "c")   #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "e")   #'denote-menu-export-to-dired))

(use-package denote-sequence
  ;; :defer t
  ;; :bind
  ;; ( :map global-map
  ;;   ;; Here we make "C-c n s" a prefix for all "[n]otes with [s]equence".
  ;;   ;; This is just for demonstration purposes: use the key bindings
  ;;   ;; that work for you.  Also check the commands:
  ;;   ;;
  ;;   ;; - `denote-sequence-new-parent'
  ;;   ;; - `denote-sequence-new-sibling'
  ;;   ;; - `denote-sequence-new-child'
  ;;   ;; - `denote-sequence-new-child-of-current'
  ;;   ;; - `denote-sequence-new-sibling-of-current'
  ;;   ("C-c n s s" . denote-sequence)
  ;;   ("C-c n s f" . denote-sequence-find)
  ;;   ("C-c n s l" . denote-sequence-link)
  ;;   ("C-c n s d" . denote-sequence-dired)
  ;;   ("C-c n s r" . denote-sequence-reparent)
  ;;   ("C-c n s c" . denote-sequence-convert))
  :config
  ;; The default sequence scheme is `numeric'.
  (setq denote-sequence-scheme 'alphanumeric))

(use-package denote-explore
  ;; :defer t
  :after denote
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-barchart-keywords)
   ("C-c w x e" . denote-explore-barchart-filetypes)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ("C-c w x x" . denote-explore-random-regex)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-barchart-degree)))

(use-package! consult-gh
  ;; :after (consult projectile)
  :defer t
  :custom
  (consult-gh-show-preview t)
  (consult-gh-preview-key "M-o")
  (consult-gh-preview-buffer-mode 'org-mode)
  ;; (consult-gh-default-clone-directory "~/projects/consult-gh")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-issue-action #'consult-gh--issue-view-action)
  (consult-gh-pr-action #'consult-gh--pr-view-action)
  (consult-gh-code-action #'consult-gh--code-view-action)
  (consult-gh-file-action #'consult-gh--files-view-action)
  ;; (consult-gh-large-file-warning-threshold 2500000)
  ;; (consult-gh-prioritize-local-folder 'suggest)
  :config
  ;;add your main GitHub account (replace "armindarvish" with your user or org)
  (add-to-list 'consult-gh-default-orgs-list "johanwiden")
  (setopt consult-gh-default-orgs-list (append consult-gh-default-orgs-list '("alphapapa" "systemcrafters")))
  (require 'consult-gh-embark)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list) ;;keep record of searched orgs
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)) ;;keep record of searched repos

(use-package! consult-mu
  :after (consult mu4e)
  ;; :defer t
  :custom
  ;; minimal config
  ;;maximum number of results shown in minibuffer
  (consult-mu-maxnum 200)
  ;;show preview when pressing any keys
  (consult-mu-preview-key 'any)
  ;;do not mark email as read when previewed
  (consult-mu-mark-previewed-as-read nil)
  ;;do not amrk email as read when selected. This is a good starting point to ensure you would not miss important emails marked as read by mistake especially when trying this package out. Later you can change this to t.
  (consult-mu-mark-viewed-as-read nil)
  ;; open the message in mu4e-view-buffer when selected.
  (consult-mu-action #'consult-mu--view-action))

(use-package! consult-web
  :after (consult consult-notes)
  ;; :defer t
  :custom
  (consult-web-default-browse-function 'browse-url)
  (consult-web-alternate-browse-function 'eww-browse-url)
  (consult-web-default-preview-function #'browse-url)
  ;; (consult-web-default-preview-function #'xwidget-webkit-browse-url)
  (consult-web-show-preview t)
  (consult-web-preview-key "M-O")
  (consult-web-highlight-matches t) ;;; highlight matches in minibuffer
  (consult-web-default-count 5) ;;; set default count
  (consult-web-default-page 0) ;;; set the default page (default is 0 for the first page)

  ;;; optionally change the consult-web debounce, throttle and delay.
  ;;; Adjust these (e.g. increase to avoid hiting a source (e.g. an API) too frequently)
  (consult-web-dynamic-input-debounce 0.8)
  (consult-web-dynamic-input-throttle 1.6)
  (consult-web-dynamic-refresh-delay 0.8)

  :config
  ;; Add sources and configure them
  ;;; load the example sources provided by default
  ;; (require 'consult-web-bing)
  ;; (require 'consult-web-brave)
  ;; (require 'consult-web-brave-autosuggest)
  ;; (require 'consult-web-doi)
  ;; (require 'consult-web-wikipedia)
  ;; (require 'consult-web-stackoverflow)
  ;; (require 'consult-web-chatgpt)
  ;; (require 'consult-web-gptel)
  ;; (require 'consult-web-buffer)
  ;; (require 'consult-web-line-multi)
  ;; (require 'consult-web-notes)
  ;; (setopt consult-web-sources-modules-to-load '(consult-web-brave
  ;;                                             consult-web-brave-autosuggest
  ;;                                             consult-web-doi
  ;;                                             consult-web-stackoverflow
  ;;                                             consult-web-chatgpt
  ;;                                             consult-web-gptel
  ;;                                             consult-web-buffer
  ;;                                             consult-web-line-multi
  ;;                                             consult-web-notes
  ;;                                             consult-web-wikipedia))
  (require 'consult-web-sources)
  (require 'consult-web-embark)

  ;;; set multiple sources for consult-web-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-web-sources-alist'.
  ;; (setopt consult-web-multi-sources (list "Brave" "Wikipedia" "chatGPT")) ;; consult-web-multi
  (setopt consult-web-multi-sources (list "Brave" "Wikipedia" "chatGPT")) ;; consult-web-multi
  ;; (setopt consult-web-dynamic-sources (list "gptel" "Brave" "StackOverflow" )) ;; consult-web-dynamic
  (setopt consult-web-dynamic-sources (list "gptel" "Brave" "StackOverflow" "Wikipedia")) ;; consult-web-dynamic
  ;; (setopt consult-web-scholar-sources (list "PubMed")) ;; consult-web-scholar
  ;; (setopt consult-web-omni-sources (list "elfeed" "Brave" "Wikipedia" "gptel" "YouTube" 'consult-buffer-sources 'consult-notes-all-sources)) ;;consult-web-omni
  ;; (setopt consult-web-omni-sources (list "Brave" "Wikipedia" "gptel" 'consult-buffer-sources 'consult-notes-all-sources)) ;;consult-web-omni
  (setopt consult-web-omni-sources (list "Brave" "Wikipedia" "gptel" "YouTube" 'consult-buffer-sources 'consult-notes-all-sources)) ;;consult-web-omni
  ;; (setopt consult-web-dynamic-omni-sources (list "Known Project" "File" "Bookmark" "Buffer" "Reference Roam Nodes" "Zettel Roam Nodes" "Line Multi" "elfeed" "Brave" "Wikipedia" "gptel" "Youtube")) ;;consult-web-dynamic-omni
  ;; (setopt consult-web-dynamic-omni-sources (list "File" "Bookmark" "Buffer" "Reference Roam Nodes" "Zettel Roam Nodes" "Brave" "Wikipedia" "gptel")) ;;consult-web-dynamic-omni
  (setopt consult-web-dynamic-omni-sources (list "File" "Bookmark" "Reference Roam Nodes" "Zettel Roam Nodes" "Line Multi" "Brave" "Wikipedia" "gptel" "YouTube")) ;;consult-web-dynamic-omni

  ;; Per source customization
  ;;; Pick you favorite autosuggest command.
  (setopt consult-web-default-autosuggest-command #'consult-web-dynamic-brave-autosuggest)

  ;;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  (setopt consult-web-bing-api-key (secrets-get-secret "Login" "Password for 'BING_SEARCH_V7_SUBSCRIPTION_KEY' on 'apikey'"))
  ;; (add-to-list 'consult-web-dynamic-sources "Bing")
  (setopt consult-web-brave-api-key (secrets-get-secret "Login" "Password for 'BRAVE_SEARCH_API_KEY' on 'apikey'"))
  ;; (add-to-list 'consult-web-dynamic-sources "Brave")
  (setopt consult-web-brave-autosuggest-api-key
        (secrets-get-secret "Login" "Password for 'BRAVE_AUTOSUGGEST_API_KEY' on 'apikey'"))
  ;; (add-to-list 'consult-web-dynamic-sources "Wikipedia")
  (setopt consult-web-stackexchange-api-key
        (secrets-get-secret "Login" "Password for 'STACKEXCHANGE_API_KEY' on 'apikey'"))
  ;; (add-to-list 'consult-web-dynamic-sources "StackOverflow")
  (setopt consult-web-openai-api-key
        (secrets-get-secret "Login" "Password for 'OPENAI_API_KEY' on 'apikey'"))
  (setopt consult-web-google-customsearch-key
        (secrets-get-secret "Login" "Password for 'YOUTUBE_V3_API_KEY' on 'apikey'"))
  ;; (add-to-list 'consult-web-dynamic-sources "chatGPT")
  ;; (add-to-list consult-web-dynamic-sources "gptel")
  ;; (add-to-list 'consult-web-dynamic-omni "Buffer")
  ;; (add-to-list 'consult-web-dynamic-omni "Line Multi")
  )

(use-package! consult-omni
  :after (consult consult-notes)
  :custom
   ;; General settings that apply to all sources
  (consult-omni-show-preview t) ;;; show previews
  (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
  (consult-omni-default-browse-function 'browse-url)
  (consult-omni-alternate-browse-function 'eww-browse-url)
  (consult-omni-default-preview-function #'browse-url)
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)

  ;; Either load all source modules or a selected list

  ;;; Select a list of modules you want to aload, otherwise all sources all laoded
  (setopt consult-omni-sources-modules-to-load (list 'consult-omni-apps
                                                   'consult-omni-brave
                                                   'consult-omni-brave-autosuggest
                                                   'consult-omni-buffer
                                                   'consult-omni-calc
                                                   'consult-omni-consult-notes
                                                   'consult-omni-dict
                                                   'consult-omni-fd
                                                   'consult-omni-find
                                                   'consult-omni-gh
                                                   'consult-omni-git-grep
                                                   'consult-omni-gptel
                                                   'consult-omni-grep
                                                   'consult-omni-invidious
                                                   'consult-omni-line-multi
                                                   'consult-omni-locate
                                                   'consult-omni-man
                                                   'consult-omni-mu4e
                                                   'consult-omni-numi
                                                   'consult-omni-wkipedia
                                                   'consult-omni-notes
                                                   'consult-omni-ripgrep
                                                   'consult-omni-ripgrep-all
                                                   'consult-omni-stackoverflow
                                                   'consult-omni-youtube))
  (consult-omni-sources-load-modules)
  ;;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
  (setopt consult-omni-multi-sources '("calc"
                                     ;; "File"
                                     ;; "Buffer"
                                     ;; "Bookmark"
                                     "Apps"
                                     ;; "gptel"
                                     "Brave"
                                     "Dictionary"
                                     ;; "Google"
                                     "Wikipedia"
                                     ;; "elfeed"
                                     ;; "mu4e"
                                     ;; "buffers text search"
                                     "Notes Search"
                                     ;; "Org Agenda"
                                     "GitHub"
                                     ;; "YouTube"
                                     "Invidious"
                                     ))

;; Per source customization

  ;;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  (setopt consult-omni-google-customsearch-key
        (secrets-get-secret "Login" "Password for 'YOUTUBE_V3_API_KEY' on 'apikey'"))
  ;; (setopt consult-omni-google-customsearch-key "YOUR-GOOGLE-API-KEY-OR-FUNCTION")
  ;; (setopt consult-omni-google-customsearch-cx "YOUR-GOOGLE-CX-NUMBER-OR-FUNCTION")
  (setopt consult-omni-brave-api-key (secrets-get-secret "Login" "Password for 'BRAVE_SEARCH_API_KEY' on 'apikey'"))
  (setopt consult-omni-brave-autosuggest-api-key
        (secrets-get-secret "Login" "Password for 'BRAVE_AUTOSUGGEST_API_KEY' on 'apikey'"))
  (setopt consult-omni-stackexchange-api-key
        (secrets-get-secret "Login" "Password for 'STACKEXCHANGE_API_KEY' on 'apikey'"))
  ;; (setopt consult-omni-pubmed-api-key "YOUR-PUBMED-API-KEY-OR-FUNCTION")
  (setopt consult-omni-openai-api-key
        (secrets-get-secret "Login" "Password for 'OPENAI_API_KEY' on 'apikey'"))

;;; Pick you favorite autosuggest command.
  (setopt consult-omni-default-autosuggest-command #'consult-omni-dynamic-brave-autosuggest) ;;or any other autosuggest source you define

 ;;; Set your shorthand favorite interactive command
  (setopt consult-omni-default-interactive-command #'consult-omni-multi))

(after! consult
 (defun consult--get-completion-options-from-help (exec)
    "Generate exec options table vai `exec' -h."
    (when (executable-find exec)
      (let* ((-h (shell-command-to-string (concat exec  " --help")))
             (-h-list (string-split -h "\\(\\.\\|:\\)\n"))
             (doc-left-pad 30))
        (mapcan (lambda (h)
                  (let ((l (string-replace "\n" "" h)))
                    (when (string-match (rx-to-string
                                         '(: bol (* space)
                                           (group "-" (? "-") (+ (or alnum "-")))
                                           (? ", ") (? (group "-" (? "-") (+ (or alnum "-"))))
                                           (? "=" (+ (or "_" "-" alnum)))
                                           (+ space)
                                           (group (* any)) eol))
                                        l)
                      (let* ((short (match-string 1 l))
                             (long (match-string 2 l))
                             (doc (match-string 3 l))
                             (s-pad (- doc-left-pad (length short)))
                             (l-pad (when long (- doc-left-pad (length long))))
                             (s-doc (concat (make-string s-pad ?\s) doc))
                             (l-doc (when long (concat (make-string l-pad ?\s) doc))))
                        (if long
                            (list `(,short . ,s-doc)
                                  `(,long . ,l-doc))
                          (list `(,short . ,s-doc)))))))
                -h-list))))

 (defmacro def-consult-help (command exec)
   (let ((options-fun (intern (format "consult-%s-get-completion-options" exec)))
         (options-alist (intern (format "consult-%s-completion-options-alist" exec)))
         (annotion (intern (format "consult-%s-completion-annotation" exec)))
         (table (intern (format "consult-%s-completion-table" exec)))
         (capf (intern (format "consult-%s-completion-at-point" exec)))
         (adv (intern (format "consult-%s-with-completion-at-point" exec))))
     `(progn
        (defun ,options-fun ()
          "Generate options table vai -h."
          (consult--get-completion-options-from-help ,exec))

        (defcustom ,options-alist
          (,options-fun)
          ,(format "%s options alist." exec))

        (defun ,annotion (candidate)
          "Annotation for rg option."
          (cdr (assoc candidate ,options-alist)))

        (defun ,table ()
          "List all option for rg."
          (mapcar #'car ,options-alist))

        (defun ,capf ()
          "Completion option.
This is the function to be used for the hook `completion-at-point-functions'."
          (interactive)
          (let* ((bds (bounds-of-thing-at-point 'symbol))
                 (start (car bds))
                 (end (cdr bds)))
            (list start end (,table) :annotation-function #',annotion)))

        (defun ,adv (orign &rest args)
          (minibuffer-with-setup-hook
              (:append
               (lambda ()
                 (add-hook 'completion-at-point-functions
                           #',capf nil t)))
            (apply orign args)))

        (advice-add ,command :around ',adv))))

 (def-consult-help 'consult-ripgrep "rg")
 (def-consult-help 'consult-fd "fd"))

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

(setopt browse-url-generic-program
      (or
       (executable-find (or (getenv "BROWSER") ""))
       (when (executable-find "xdg-mime")
         (let ((desktop-browser (ambrevar/call-process-to-string "xdg-mime" "query" "default" "text/html")))
           (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
       (executable-find browse-url-chrome-program)))

(use-package! mistty
  :defer t
  :config
  (setopt explicit-shell-file-name "/usr/bin/fish")
  ;; (setopt explicit-shell-file-name "/usr/bin/bash")
  )

(use-package! igist
  :defer t
  :config
  (setopt igist-current-user-name "johanwiden")
  (setopt igist-auth-marker 'igist))

(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)

(use-package! casual-calc
  :ensure nil
  :after calc
  :bind
    (:map
     calc-mode-map
     ("C-o" . casual-calc-tmenu)
     :map
     calc-alg-map
     ("C-o" . casual-calc-tmenu)))

(use-package! casual-isearch
  :ensure nil
  :defer t
  ;; :bind
  ;; (:map isearch-mode-map ((kbd "<f2>") . casual-isearch-tmenu))
  :config
  (define-key isearch-mode-map (kbd "C-o") #'casual-isearch-tmenu))

(use-package! visual-replace
  :ensure nil
  :defer t
  :bind (("C-c r" . visual-replace)
          :map isearch-mode-map
          ("C-c r" . visual-replace-from-isearch)))

(setq! gptel-api-key (secrets-get-secret "Login" "Password for 'OPENAI_API_KEY' on 'apikey'"))
;; (use-package! gptel
;;   :defer t
;;   :config
;;   (setq gptel-model "gpt-4o")
;;   (setq! gptel-api-key (secrets-get-secret "Login" "Password for 'OPENAI_API_KEY' on 'apikey'")))

(use-package! evil-matchit
  ;; :defer t
  :config
  (global-evil-matchit-mode 1))

(use-package! activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setopt edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package! bufferlo
  ;; :defer t
  :init
  (setopt bufferlo-bookmark-tab-replace-policy 'new)
  (setopt bufferlo-bookmark-restore-tab-groups t)
  (bufferlo-mode))

(use-package! tab-bar-groups
  :init
  (setq tab-bar-groups-colors
        '("yellow" "green" "orange" "brown" "magenta" "cyan" "goldenrod" "red"))
  :config
  (tab-bar-groups-activate))

;; (defun jw/tab-bar-group (bookmark-name effective-bookmark-name new-frame-p frame)
;;   "For each tab in tab-bar-tabs, conditionally add tab to a tab-group"
;;   ;; (interactive)
;;   (cl-loop for tab in (tab-bar-tabs)
;;            do (let ((current-tab-name (cdr (assq 'name tab))))
;;                 (cond
;;                  ((string-match-p (rx "partition") current-tab-name)
;;                   (tab-bar-groups-assign-group "thermo" tab))
;;                  ((string-match-p (rx "thermodyna") current-tab-name)
;;                   (tab-bar-groups-assign-group "thermo" tab))
;;                  ((string-match-p (rx "Gibbs free") current-tab-name)
;;                   (tab-bar-groups-assign-group "thermo" tab))
;;                  ((string-match-p (rx "internal ene") current-tab-name)
;;                   (tab-bar-groups-assign-group "thermo" tab))
;;                  ((string-match-p (rx "state func") current-tab-name)
;;                   (tab-bar-groups-assign-group "thermo" tab))
;;                  ((string-match-p (rx "generalized") current-tab-name)
;;                   (tab-bar-groups-assign-group "anal" tab))
;;                  ((string-match-p (rx "total deriva") current-tab-name)
;;                   (tab-bar-groups-assign-group "anal" tab))
;;                  (t t)))))

;; (add-hook 'bufferlo-bookmark-frame-handler-functions 'jw/tab-bar-group)

;; (defun jw/tab-bar-group (tab)
;;   "Conditionally add current tab to a tab group"
;;   (let ((current-tab-name (cdr (assq 'name tab))))
;;   (cond
;;    ((string-match-p (rx "partition") current-tab-name)
;;     (tab-bar-groups-assign-group "anal" tab))
;;    ((string-match-p (rx "eneralized") current-tab-name)
;;     (tab-bar-groups-assign-group "thermo" tab))
;;    (t (message (format "jw A%sA" current-tab-name))))))

;; (add-hook 'tab-bar-tab-post-open-functions 'jw/tab-bar-group)

;; (defun my/tab-bar-tab-post-open-function (tab)
;;   (my/tab-bar-set-id nil tab)
;;   (my/tab-bar-tab-set-project-and-group tab 'maybe-sort))
;; (add-hook 'tab-bar-tab-post-open-functions #'my/tab-bar-tab-post-open-function)

;;     (when (eq tab-bar-new-tab-choice 'clone)
;;       (let* ((tabs (funcall tab-bar-tabs-function))
;;              (orig-tab (nth (tab-bar--tab-index-recent 1) tabs)))

(defun ace-window-one-command ()
  (interactive)
  (let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((command (key-binding
                         (read-key-sequence
                          (format "Run in %s..." (buffer-name)))))
               (this-command command))
          (call-interactively command))))))

(keymap-global-set "C-x O" 'ace-window-one-command)

(defun ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(keymap-global-set "C-x 4 o" 'ace-window-prefix)

(use-package! jinx
  :defer t
  :init
  (add-hook 'doom-init-ui-hook #'global-jinx-mode)
  ;; :hook (conf-mode text-mode prog-mode org-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (after! ispell
    (global-set-key [remap ispell-word] #'jinx-correct))
  (after! evil-commands
    (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
    (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous)))

(use-package! casual-avy
  :ensure nil
  :defer t
  ;; :after avy
  :bind ("C-c g" . casual-avy-tmenu))

(use-package! casual-eshell
  :ensure nil
  :defer t
  :bind (:map eshell-mode-map
              ("C-o" . casual-eshell-tmenu)))

(use-package! casual-compile
  :ensure nil
  :defer t
  :bind (:map compilation-mode-map
              ("C-o" . casual-compile-tmenu))
        (:map grep-mode-map
              ("C-o" . casual-compile-tmenu)))

(use-package! casual-elisp
  :ensure nil
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("M-m" . casual-elisp-tmenu)))

(use-package! casual-info
  :ensure nil
  :defer t
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu))
  :config
  ;; Use web-browser history navigation bindings
  (keymap-set Info-mode-map "M-[" #'Info-history-back)
  (keymap-set Info-mode-map "M-]" #'Info-history-forward)
  ;; Bind p and n to paragraph navigation
  (keymap-set Info-mode-map "p" #'casual-info-browse-backward-paragraph)
  (keymap-set Info-mode-map "n" #'casual-info-browse-forward-paragraph)
  ;; Bind h and l to navigate to previous and next nodes
  ;; Bind j and k to navigate to next and previous references
  (keymap-set Info-mode-map "h" #'Info-prev)
  (keymap-set Info-mode-map "j" #'Info-next-reference)
  (keymap-set Info-mode-map "k" #'Info-prev-reference)
  (keymap-set Info-mode-map "l" #'Info-next)
  ;; Bind / to search
  (keymap-set Info-mode-map "/" #'Info-search)
  ;; Set Bookmark
  (keymap-set Info-mode-map "B" #'bookmark-set)

  (add-hook 'Info-mode-hook #'hl-line-mode)
  (add-hook 'Info-mode-hook #'scroll-lock-mode))

(use-package! casual-man
  :ensure nil
  :defer t
  :bind (:map Man-mode-map ("C-o" . casual-man-tmenu))
  :config
  ;; The following keybindings are recommended to support consistent behavior between Man-mode and casual-man-tmenu.
  (keymap-set Man-mode-map "n" #'casual-lib-browse-forward-paragraph)
  (keymap-set Man-mode-map "p" #'casual-lib-browse-backward-paragraph)
  (keymap-set Man-mode-map "[" #'Man-previous-section)
  (keymap-set Man-mode-map "]" #'Man-next-section)
  (keymap-set Man-mode-map "j" #'next-line)
  (keymap-set Man-mode-map "k" #'previous-line)
  (keymap-set Man-mode-map "K" #'Man-kill)
  (keymap-set Man-mode-map "o" #'casual-man-occur-options))

(use-package! casual-help
  :ensure nil
  :defer t
  :bind (:map help-mode-map ("C-o" . casual-help-tmenu))
  :config
  ;; The following keybindings are recommended to support consistent behavior between help-mode and casual-help-tmenu.
  (keymap-set help-mode-map "M-[" #'help-go-back)
  (keymap-set help-mode-map "M-]" #'help-go-forward)
  (keymap-set help-mode-map "p" #'casual-lib-browse-backward-paragraph)
  (keymap-set help-mode-map "n" #'casual-lib-browse-forward-paragraph)
  (keymap-set help-mode-map "P" #'help-goto-previous-page)
  (keymap-set help-mode-map "N" #'help-goto-next-page)
  (keymap-set help-mode-map "j" #'forward-button)
  (keymap-set help-mode-map "k" #'backward-button))

(use-package! casual-bibtex
  :ensure nil
  :defer t
  :bind (:map bibtex-mode-map ("M-m" . casual-bibtex-tmenu))
  :config
  ;; The following keybindings are recommended to support consistent behavior between bibtex-mode-map and casual-bibtex-tmenu.
  (add-hook 'bibtex-mode-hook 'hl-line-mode)

  (keymap-set bibtex-mode-map "<TAB>" #'bibtex-next-field)
  (keymap-set bibtex-mode-map "<backtab>" #'previous-line)

  (keymap-set bibtex-mode-map "C-n" #'bibtex-next-field)
  (keymap-set bibtex-mode-map "M-n" #'bibtex-next-entry)
  (keymap-set bibtex-mode-map "M-p" #'bibtex-previous-entry)

  (keymap-set bibtex-mode-map "<prior>" #'bibtex-previous-entry)
  (keymap-set bibtex-mode-map "<next>" #'bibtex-next-entry)

  (keymap-set bibtex-mode-map "C-c C-o" #'bibtex-url)
  (keymap-set bibtex-mode-map "C-c C-c" #'casual-bibtex-fill-and-clean)

  (keymap-set bibtex-mode-map "<clear>" #'bibtex-empty-field)
  (keymap-set bibtex-mode-map "M-<clear>" #'bibtex-kill-field)
  (keymap-set bibtex-mode-map "M-DEL" #'bibtex-kill-field))

(use-package! casual-csv
  :ensure nil
  :defer t
  :bind (:map csv-mode-map ("M-m" . casual-csv-tmenu))
  :config
  ;; disable line wrap
  (add-hook 'csv-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (toggle-truncate-lines 1)))

  ;; auto detect separator
  (add-hook 'csv-mode-hook #'csv-guess-set-separator)
  ;; turn on field alignment
  (add-hook 'csv-mode-hook #'csv-align-mode))

(use-package! eldoc-box
  ;; :defer t
)

(use-package! immersive-translate
  :defer t
  :config
  (setopt immersive-translate-backend 'chatgpt
        immersive-translate-chatgpt-host "api.openai.com"))

(use-package! cc-cedict
  ;; :defer t
  :config
  (setq cc-cedict-file "/home/jw/.stardict/dic/org-cc-cedict/cedict_1_0_ts_utf-8_mdbg.txt"))

(use-package! pinyin-convert
  ;; :defer t
)

(use-package! jieba
  ;; :defer t
)

(use-package! zh-utils
  ;; :defer t
)

(use-package! pangu-spacing
  :defer t)

(use-package! wallabag
  ;; :defer t
  ;; :load-path "~/.config/emacs/.local/straight/repos/wallabag/"
  :hook
  (wallabag-entry-mode . paw-annotation-mode)
  :config
  (setopt wallabag-host "https://app.wallabag.it") ;; wallabag server host name
  (setopt wallabag-username "johanwiden") ;; username
  ;; (setopt wallabag-username "johan") ;; username
  ;; (setopt wallabag-password (auth-source-pick-first-password :host "<wallabag-host>")
  ;;       wallabag-secret (auth-source-pick-first-password :host "<wallabag-client>"))
  (setopt wallabag-password (secrets-get-secret "Login" "Password for 'WALLABAG_PASSWORD' on 'apikey'")
        wallabag-secret (secrets-get-secret "Login" "Password for 'WALLABAG_SECRET' on 'apikey'"))
  ;; (setopt wallabag-password "pengalosa") ;; password
  (setopt wallabag-clientid "28948_5xfysd48cv0g0ksgc84wwgwskc8s0s8wg84k0koo8k8sk0ksgk") ;; created with API clients management
  ;; (setopt wallabag-clientid "1_28hh2e86jqkgckkogkcscwcwggcwcks408cwskcw8g40s88kgs") ;; created with API clients management
  ;; (setopt wallabag-secret "1y9pknyk85twccggg0cswkc088cc04css0c4oww8840wgsggoc") ;; created with API clients management
  (setopt wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date")) ;; control what content should be show in *wallabag-search*
  (setopt wallabag-search-page-max-rows 32) ;; how many items shown in one page
  (setopt wallabag-db-file "~/org/wallabag.sqlite") ;; optional, default is saved to ~/.emacs.d/.cache/wallabag.sqlite
  ;; (run-with-timer 0 3540 'wallabag-request-token) ;; optional, auto refresh token, token should refresh every hour
  ;; (add-hook 'wallabag-entry-mode-hook #'org-indent-mode)
  (add-hook 'wallabag-entry-mode-hook #'eldoc-mode)
  (add-hook 'wallabag-entry-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'wallabag-entry-mode-hook #'shrface-wallabag-setup)

  (advice-add 'wallabag-entry-quit :after #'(lambda (&rest args)
                                              (interactive)
                                              (if (get-buffer "*Ilist*")
                                                  (kill-buffer "*Ilist*"))))

  (require 'shrface)

  (defun shrface-wallabag-setup ()
    (unless shrface-toggle-bullets
      (shrface-regexp)
      (setq-local imenu-create-index-function #'shrface-imenu-get-tree))
    (if (string-equal system-type "android")
        (setq-local touch-screen-enable-hscroll nil))
    ;; (add-function :before-until (local 'eldoc-documentation-function) #'paw-get-eldoc-note)
    )

  (setopt wallabag-render-html-function #'my-wallabag-render-html)
  (defun my-wallabag-render-html (begin end)
    (require 'eww)
    (let ((shrface-org nil)
          (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
          ;; make it large enough, it would not fill the column
          ;; I uses visual-line-mode, writeroom-mode for improving the reading experience instead
          (shr-width 7000)
          (shr-indentation (if (string-equal system-type "android") 0 0))
          (shr-table-vertical-line "|")
          (shr-external-rendering-functions
           (append '((title . eww-tag-title)
                     (form . eww-tag-form)
                     (input . eww-tag-input)
                     (button . eww-form-submit)
                     (textarea . eww-tag-textarea)
                     (select . eww-tag-select)
                     (link . eww-tag-link)
                     (meta . eww-tag-meta)
                     ;; (a . eww-tag-a)
                     (code . shrface-tag-code)
                     (pre . shrface-shr-tag-pre-highlight))
                   shrface-supported-faces-alist))
          (shrface-toggle-bullets nil)
          (shrface-href-versatile t)
          (shr-use-fonts nil))
      (shr-render-region begin end))
    ;; workaround, show annotations when document updates
    (when (bound-and-true-p paw-annotation-mode)
      (paw-clear-annotation-overlay)
      (paw-show-all-annotations)
      (if paw-annotation-show-wordlists-words-p
          (paw-focus-find-words :wordlist t))
      (if paw-annotation-show-unknown-words-p
        (paw-focus-find-words)))))

(use-package! paw
  :defer t
  :hook
  ;; my personal configs
  (paw-view-note-mode . paw-view-note-setup)
  (paw-annotation-mode . paw-annotation-setup)
  :init
  (require 'paw-hsk)
  (require 'paw-jieba)
  (paw-hsk-update-word-lists)
  (setopt paw-note-dir (expand-file-name "paw" org-directory))
  (setopt paw-db-file (expand-file-name "paw.sqlite" paw-note-dir))
  ;; Set SilverDict as the external dictionary function
  (setq paw-external-dictionary-function 'paw-silverdict-search-details)
  ;; Configure host and port (defaults shown below)
  (setq paw-silverdict-host "localhost")  ; or your server IP
  (setq paw-silverdict-port "2628")       ; default SilverDict port
  ;; (setq paw-silverdict-query-path "/api/query/Chinese/")
  ;; ecdict dictionary
  (setopt paw-ecdict-db (expand-file-name "stardict.db" paw-note-dir))
  (setq paw-say-word-functions '(paw-edge-tts-say-word))
  ;; setup ECDICT before using it, and create the files manually if not exist
  ;; (setopt paw-ecdict-wordlist-files `(
  ;;                                   ;; ,(expand-file-name "美国当代英语语料库.csv" paw-note-dir) ;; https://www.eapfoundation.com/vocab/academic/other/mawl/
  ;;                                   ,(expand-file-name "mawl.csv" paw-note-dir) ;; https://www.eapfoundation.com/vocab/academic/other/mawl/
  ;;                                   ,(expand-file-name "opal.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "5000.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "极品GRE红宝书.csv" paw-note-dir)
  ;;                                   ,(expand-file-name "gre.txt" paw-note-dir)
  ;;                                   ,(expand-file-name "托福绿宝书.csv" paw-note-dir)
  ;;                                   ,(expand-file-name "2021_Teachers_AcademicCollocationList.csv" paw-note-dir) ;; https://www.pearsonpte.com/teachers/academic-collocation
  ;;                                   ,(expand-file-name "The Unofficial Harry Potter Vocabulary Builder.csv" paw-note-dir)
  ;;                                   ,(expand-file-name "Illustrated Everyday Expressions with Stories.csv" paw-note-dir)
  ;;                                   ,(expand-file-name "Essential Idioms in English.csv" paw-note-dir)
  ;;                                   ,(expand-file-name "IELTS_word_lists.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "Cambridge_word_lists_-_Advanced.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "Cambridge_word_lists_-_Intermediate.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "Cambridge_word_lists_-_Beginner.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "idioms.txt" paw-note-dir)
  ;;                                   ,(expand-file-name "phrase-list.csv" paw-note-dir) ;; https://www.oxfordlearnersdictionaries.com/wordlists/
  ;;                                   ,(expand-file-name "英语生词本.csv" paw-note-dir)
  ;;                                   ))
  ;; setup ECDICT before using it, and create the files manually if not exist
  (setq paw-ecdict-known-words-files `(,(expand-file-name "eudic.csv" paw-note-dir)
                                       ,(expand-file-name "english.txt" paw-note-dir)))
  ;; setup ECDICT before using it, and create the file manually if not exists
  (setopt paw-ecdict-default-known-words-file (expand-file-name "english.txt" paw-note-dir))

  ;; jlpt dictionary
  ;; (setopt paw-jlpt-db (expand-file-name "japanese.db" paw-note-dir))
  ;; setup jlpt before using it, and create the files manually if not exist
  ;; (setopt paw-jlpt-wordlist-files `(,(expand-file-name "日语生词本.csv" paw-note-dir)
  ;;                                 ,(expand-file-name "日本语红宝书.csv" paw-note-dir)
  ;;                                 ,(expand-file-name "蓝宝书日语文法.csv" paw-note-dir)
  ;;                                 ,(expand-file-name "NEW-JLPT.csv" paw-note-dir)
  ;;                                   ))
  ;; setup jlpt before using it, and create the files manually if not exist
  ;; (setopt paw-jlpt-known-words-files `(,(expand-file-name "japanese.txt" paw-note-dir)))
  ;; setup jlpt before using it, and create the file manually if not exists
  ;; (setopt paw-jlpt-default-known-words-file (expand-file-name "japanese.txt" paw-note-dir))
  :custom
  (paw-non-ascii-word-separator "⁣") ;; Invisible separator character, for chinese
  (paw-non-ascii-language "zh") ;; For chinese
  (paw-go-translate-langs '(en zh)) ;; For chinese
  (paw-svg-enable t) ;; use svg-lib to generate icons
  ;; (paw-pbm-enable t) ;; use builtin pmb icons
  ;; Use all the icons icon on dashboard
  (paw-all-the-icons-icon-enable nil)
  ;; Use nerd icon on dashboard
  (paw-nerd-icons-icon-enable t)
  ;; Use all the icons button on non-android system
  ;; (paw-all-the-icons-button-enable (unless (eq system-type 'android) t))
  ;; you can use (face-attribute 'org-block :background) or other color
  (paw-view-note-background-color (face-attribute 'org-block :background))
  ;; For performance concerned, disable `paw-detect-language-p' and use
  ;; `paw-ascii-rate', `paw-default-language' and `paw-non-ascii-language'
  ;; instead.
  ;; (paw-detect-language-p t)
  (paw-detect-language-p nil)
  ;; (paw-python-program (if (string-equal system-type "android") "python3.10" "python3"))
  (paw-python-program "python3")
  ;; (paw-detect-language-program 'gcld3) ;; android can only install cld3
  (paw-detect-language-program
   (pcase system-type
     ('gnu/linux 'gcld3)
     ('windows-nt 'gcld3)
     ('darwin 'pycld2)
     ('android 'gcld3)))
  ;; Note for SER8: gcld3 is in a python venv, activated through shell command: source .venv/bin/activate
  ;; Note also that gcld3 is not an application, it is a python library.
  (paw-click-overlay-enable t)
  (paw-annotation-read-only-enable t)
  ;; (paw-annotation-show-wordlists-words-p t) ;; setup ECDICT before using it
  ;; (paw-annotation-show-unknown-words-p nil) ;; setup ECDICT before using it
  ;; (paw-ecdict-frq 3000) ;; setup ECDICT before using it
  ;; (paw-ecdict-bnc -1) ;; all possible words, 0 no bnc data
  ;; (paw-ecdict-tags "cet4 cet6 ielts toefl gre empty") ;; no easy words
  ;; (paw-ecdict-oxford 0) ;; no easy words
  ;; (paw-ecdict-collins-max-level 3) ;; no easy words
  ;; (paw-posframe-p (if (string-equal system-type "android") t))
  ;; For online words, you have to apply api on
  ;; https://my.eudic.net/OpenAPI/Authorization
  ;; (paw-authorization-keys (auth-source-pick-first-password :host "eudic-api-key"))
  ;; limit other languages web buttons number
  (paw-english-web-button-number (if (eq system-type 'android) 4 4))
  ;; limit japanese web buttons number
  (paw-japanese-web-button-number (if (eq system-type 'android) 3 4))
  ;; limit general web buttons number
  (paw-general-web-button-number (if (eq system-type 'android) 2 3))
  ;; (paw-default-say-word-function (if (eq system-type 'android) 'paw-android-say-word 'paw-say-word))
  (paw-tts-zh-cn-voice "zh-CN-YunjianNeural") ; zh-CN-XiaoxiaoNeural, zh-CN-YunyangNeural
  ;; (paw-sdcv-dictionary-list '("简明英汉字典增强版"))
  (paw-sdcv-dictionary-list '("Org CC-Cedict"))
  ;; add online word by default for add button
  ;; (paw-add-button-online-p t)
  ;; show the note both in minibuffer or/and *paw-view-note*
  ;; To use this, you need to setup ECDICT (English) or JLPT (Japanese) before
  ;; use this, otherwise, use the 'buffer instead
  ;; (paw-view-note-show-type (if (eq system-type 'android) 'buffer 'all))
  (paw-view-note-show-type 'buffer)
  ;; must be one of the studylist name in `paw-studylist', please run `paw-get-all-studylist' to get the available studylists.
  ;; (paw-default-online-studylist "ID: 133584289117482484, Language: en, Name: Business")
  ;; (paw-offline-studylist '(("English Studylist" ;; studylist name when choosing offline studylist
  ;;                           (id . "1") ;; random id for internal use, but it could not be the same as any id in online study list defined in `paw-studylist'
  ;;                           (language . "en") ;; language of the studylist
  ;;                           (name . "English")) ;; name of the studylist
  ;;                          ("Japanese Studylist"
  ;;                           (id . "2")
  ;;                           (language . "ja")
  ;;                           (name . "Japanese"))))
  (paw-offline-studylist '(("Chinese Studylist" ;; studylist name when choosing offline studylist
			      (id . "1") ;; random id for internal use, but it could not be the same as any id in online study list defined in `paw-studylist'
			      (language . "zh") ;; language of the studylist
			      (name . "Chinese")) ;; name of the studylist
			     ))
  ;; (paw-search-function #'paw-chinese-search-function)
  (paw-chinese-sdcv-exact-match t)
  (paw-hsk-levels-to-highlight "hsk1 hsk2 hsk3 hsk4 hsk5 hsk6 hsk7-to-9")
  (paw-view-note-meaning-src-lang "org")
  ;; must be one of the studylist name in `paw-offline-studylist'
  ;; (paw-default-offline-studylist "English Studylist")
  (paw-default-offline-studylist "Chinese Studylist")
  (paw-search-page-max-rows (pcase system-type
                              ('gnu/linux 31)
                              ('windows-nt 31)
                              ('darwin 31)
                              ('android 31)))
  ;; be careful if using auto adjust, paw-search-page-max-rows will be ignored, it may be unstable
  (paw-search-page-max-rows-auto-adjust t)
  (paw-add-offline-word-without-asking t)
  (paw-add-online-word-without-asking t)
  ;; ;; Servers to add online words. It could be eudic, anki, or both.
  ;; (paw-online-word-servers '(eudic anki))
  ;; ;; The default Anki deck to use.
  ;; (paw-anki-deck "English")
  (paw-view-note-click-enable t)
  (paw-view-note-back-to-original-buffer-supported-modes
   '("pdf-viewer" paw-search-mode nov-mode org-mode wallabag-entry-mode))
  :config
  ;; if the file was moved to other places after adding annotations, we can add
  ;; the parent path of the file for paw to search. This is necessary for
  ;; multiple clients (PC/Mobile/Pad) to use the same database but file location
  ;; is different.
  (setq paw-annotation-search-paths '("~/calibre_library"
                                      "~/Data/Books/"
                                      "/storage/emulated/0/Books/"
                                      ;; "/storage/emulated/0/Download/"
                                      ;; "/storage/emulated/0/Download/Telegram"
                                      ;; "/storage/emulated/0/Org/web/"
                                      "~/org/notes/web/"
                                      "~/org/roam"
                                      "~/Sync/notes"
                                      ))

  ;; show image annotation in *paw-view-note*
  (add-hook 'paw-view-note-after-render-hook #'org-display-inline-images)
  (add-hook 'context-menu-functions #'paw-annotation-context-menu)

  (unless (string-equal system-type "android")
    (let ((func (cond
                 ((fboundp 'eaf-open-browser-other-window)
                  'eaf-open-browser-other-window)
                 (t 'browse-url))))
      (setopt paw-dictionary-browse-function func)
      (setopt paw-mdict-dictionary-function func)))

  (setopt paw-sdcv-env-lang (getenv "LANG"))
  (setopt paw-sdcv-dictionary-data-dir (expand-file-name "dict" doom-private-dir))

  (unless (string-equal system-type "android")
    (let ((func (cond
                 ((fboundp 'eaf-open-browser-other-window)
                  'eaf-open-browser-other-window)
                 (t 'browse-url))))
      (setq paw-dictionary-browse-function func)
      (setq paw-mdict-dictionary-function func)))

  (after! wallabag
    (paw-server)))

(defun paw-view-note-setup ()
   ;; (org-writeroom-setup)
  (when (fboundp 'visual-line-mode)
    (visual-line-mode))
  ;; (org-modern-mode)
  (when (fboundp 'pangu-spacing-mode)
    (pangu-spacing-mode))
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1)))

(defun paw-annotation-setup()
  ;; TODO need manual enable later
  (when (bound-and-true-p flyspell-mode)
    (flyspell-mode -1)))

;; (defun paw-silverdict-search-details (&optional word en)
;;   "Search WORD with SilverDict web server.
;; The SilverDict server must be running on paw-silverdict-host:paw-silverdict-port.
;; If WORD is not provided, uses the word at point via `paw-get-word'.
;; EN parameter is ignored but kept for API compatibility."
;;   (interactive)
;;   (let* ((word (or word (paw-get-word)))
;;          (url (format "http://%s:%s%s%s"
;;                       paw-silverdict-host
;;                       paw-silverdict-port
;;                       paw-silverdict-query-path
;;                       (url-hexify-string word))))
;;     (browse-url url)))

;; (after! paw
;; ;; override the original gt-init, to remove the Processing message
;; (cl-defmethod gt-init :around ((render gt-render) translator)
;;   (gt-log-funcall "init (%s %s)" render translator)
;;   (condition-case err
;;       (progn (cl-call-next-method render translator)
;;              (gt-update translator))
;;     (error (gt-log 'render (format "%s initialize failed, abort" (eieio-object-class render)))
;;            (user-error (format "[output init error] %s" err)))))
;; (cl-defmethod gt-output ((render paw-gt-translate-render) translator)
;;   (deactivate-mark)
;;   (when (= (oref translator state) 3)
;;     (let* ((ret (gt-extract-data render translator))
;;            (buffer (get-buffer (oref render buffer-name)))
;;            (section (oref render section)))
;;       (if-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
;;           (setq paw-go-translate-running-p nil)
;;         ;; (error "%s" (plist-get err :result))
;;         ;; (error "Translation Error")
;;         (if (buffer-live-p buffer)
;;             (with-current-buffer buffer
;;               (save-excursion
;;                 (let* ((buffer-read-only nil)
;;                        (translation (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n")))
;;                   (unless (string-match-p section (org-no-properties (org-get-heading t t t t)))
;;                     (goto-char (point-min))
;;                     (search-forward (format "** %s" section) nil t))
;;                   (org-end-of-subtree t t)
;;                   ;; (forward-line)
;;                   ;; (delete-region (region-beginning) (region-end))
;;                   (let ((bg-color paw-view-note-background-color))
;;                     (paw-insert-and-make-overlay
;;                      translation
;;                      'face `(:background ,bg-color :extend t))
;;                     (insert "\n"))
;;                   (if (or paw-ask-ai-p paw-ai-translate-p paw-ai-translate-context-p)
;;                       (insert "\n"))
;;                   (beginning-of-line)
;;                   ;; (message "Translation completed")
;;                   ;; (message "Translation completed %s" translation)
;;                   ) )
;;               (deactivate-mark))))))
;;   (setq paw-go-translate-running-p nil))
;;   )

(use-package! sdcv
  :defer t
  :config
  (setopt sdcv-env-lang "en_US.UTF-8")
  (setopt sdcv-program "sdcv")
  (setopt sdcv-only-data-dir nil) ;; sdcv --only-data-dir ...
  (setopt sdcv-dictionary-data-dir nil) ;; sdcv --data-dir ...
  (setq sdcv-dictionary-simple-list
        '("Merriam-Webster's Advanced Learner's Dictionary (En-En)"))
  (setq sdcv-dictionary-complete-list
        '("Merriam-Webster's Advanced Learner's Dictionary (En-En)"))
  )

(use-package! org-download
  :defer t
  :init
  (add-hook 'dired-mode-hook 'org-download-enable)
  :config
  (setopt org-download-image-dir "~/Pictures/org-download"))

(use-package! gt
  :defer t
  :config
  (setopt gt-langs '(en fr))
  (setopt gt-default-translator (gt-translator :engines (gt-google-engine))))

(use-package! svg-lib
  :defer t)

(use-package! tldr
  :defer t)

(use-package! transient-showcase
  ;; :defer t
  )

(use-package! shr-tag-pre-highlight
  :defer t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package! shrface
  :defer t
  :config
  (setq shr-cookie-policy nil)
  (if (string-equal system-type "android")
      (setopt shrface-bullets-bullet-list
        '("▼"
          "▽"
          "▿"
          "▾"))
    (setopt shrface-bullets-bullet-list
          '("▼"
            "▽"
            "▿"
            "▾"
            )
          )
    )

  (add-hook 'outline-view-change-hook 'shrface-outline-visibility-changed)

  (require 'shr-tag-pre-highlight)
  (setq shr-tag-pre-highlight-lang-modes
        '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
          ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
          ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
          ("screen" . shell-script) ("shell" . sh) ("bash" . sh)
          ("rust" . rustic)
          ("rust" . rustic)
          ("awk" . bash)
          ("json" . "js")
          ;; Used by language-detection.el
          ("emacslisp" . emacs-lisp)
          ;; Used by Google Code Prettify
          ("el" . emacs-lisp)))

  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       ;; (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       ;; (propertize "#+END_SRC" 'face 'org-block-end-line )
       )
      (shr-ensure-newline)
      (setq end (point))
      (pcase (frame-parameter nil 'background-mode)
        ('light
         (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
        ('dark
         (add-face-text-property start end '(:background "#292b2e" :extend t))))
      (shr-ensure-newline)
      (insert "\n"))))

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
 ;; (autoload 'maxima-mode "maxima" "Maxima mode" t)
 ;; (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
 ;; (autoload 'maxima "maxima" "Maxima interaction" t)
 ;; (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
 ;; (setopt imaxima-use-maxima-mode-flag t)
 ;; (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
(use-package! maxima
  :defer t
  :init
  (add-hook 'maxima-mode-hook #'maxima-hook-function)
  (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
  (setq
	 org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
	 maxima-display-maxima-buffer nil)
  :mode ("\\.mac\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :bind (:map maxima-mode-map
              ;; Main motions commands, this can be used inside the maxima-mode buffer.
              ("M-C-a" . maxima-goto-beginning-of-form)
              ("M-C-e" . maxima-goto-end-of-form)
              ("M-C-b" . maxima-goto-beginning-of-list)
              ("M-C-f" . maxima-goto-end-of-list)
              ("M-h"   . maxima-mark-form)
              ("C-c )"   . maxima-check-parens-region)
              ("C-c C-)" . maxima-check-form-parens)
              ;; Completions command
              ("M-TAB"       . maxima-complete)
              ;; Help with the symbol under point, use (“d” for describe)
              ("C-c C-d d"   . maxima-completion-help)
              ("C-c C-d C-d" . maxima-completion-help)
              ;; Eldoc-like information
              ("C-c C-d s"   . maxima-symbol-doc)
              ;; Apropos
              ("C-c C-d a"   . maxima-apropos)
              ("C-c C-d C-a" . maxima-apropos)
              ;; To get apropos with the symbol under point, use
              ("C-c C-d p"   . maxima-apropos-help)
              ("C-c C-d C-p" . maxima-apropos-help)
              ;; Maxima info manual, use
              ("C-c C-d m"   . maxima-info)
              ("C-c C-d C-m" . maxima-info)
              ("C-c C-d i"   . maxima-info)
              ("C-c C-d C-i" . maxima-info)
              ;; Interaction with the Maxima process
              ("C-c C-r"   . maxima-send-region)
              ("C-c C-b"   . maxima-send-buffer)
              ("C-c C-c"   . maxima-send-line)
              ("C-c C-e"   . maxima-send-previous-form)
              ;; Send the smallest set of lines which contains the cursor
              ;; and contains no incomplete forms, and go to the next form.
              ("C-RET"     . maxima-send-full-line-and-goto-next-form)
              ;; As above, but with the region instead of the current line.
              ("M-RET"     . maxima-send-completed-region-and-goto-next-form)
              ;; Prompt for a file name to load into Maxima
              ("C-c C-l"   . maxima-load-file))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((maxima . t))))

(after! julia-repl
  (progn
    (julia-repl-set-terminal-backend 'vterm)))

(use-package! pulsar
  ;; :defer t
  :config
  ;; Check the default value of `pulsar-pulse-functions'.  That is where
  ;; you add more commands that should cause a pulse after they are
  ;; invoked

  (setopt pulsar-pulse t)
  (setopt pulsar-delay 0.055)
  (setopt pulsar-iterations 10)
  (setopt pulsar-face 'pulsar-magenta)
  (setopt pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ;; OR use the local mode for select mode hooks

  ;; (dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
  ;;   (add-hook hook #'pulsar-mode))

  ;; pulsar does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  ;;
  ;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
  ;; `pulsar-highlight-line'.
  ;;
  ;; You can replace `pulsar-highlight-line' with the command
  ;; `pulsar-highlight-dwim'.
  (let ((map global-map))
    (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
    (define-key map (kbd "C-c h h") #'pulsar-highlight-line));; Check the default value of `pulsar-pulse-functions'.  That is where
;; you add more commands that should cause a pulse after they are
;; invoked

(setopt pulsar-pulse t)
(setopt pulsar-delay 0.055)
(setopt pulsar-iterations 10)
(setopt pulsar-face 'pulsar-magenta)
(setopt pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

;; OR use the local mode for select mode hooks

(dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'pulsar-mode))

;; pulsar does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
;;
;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
;; `pulsar-highlight-line'.
;;
;; You can replace `pulsar-highlight-line' with the command
;; `pulsar-highlight-dwim'.
(let ((map global-map))
  (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
  (define-key map (kbd "C-c h h") #'pulsar-highlight-line))
;; Use pulsar with next-error
(add-hook 'next-error-hook #'pulsar-pulse-line)
;; Recenter and then pulse current line
;; integration with the `consult' package:
(add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
(add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

;; integration with the built-in `imenu':
(add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
(add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))

(use-package! reader
  :defer t)

(after! pdf-tools
  (add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode))

(use-package! math-preview
  :defer t
  :config
  (setopt math-preview-command "/home/jw/projects/html/math-preview/math-preview.js"))

(use-package! typst-ts-mode
  ;; :defer t
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu)
  )

;;; A Help Transient on C-S-h
(transient-define-prefix hrm-help-transient ()
  "Help commands that I use. A subset of C-h with others thrown in."
  ["Help Commands"
   ["Mode &amp; Bindings"
    ("m" "Mode" describe-mode)
    ("M" "Minor Modes" consult-minor-mode-menu)
    ("b" "Major Bindings" which-key-show-full-major-mode)
    ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
    ("d" "Descbinds" describe-bindings) ; or embark-bindings
    ("t" "Top Bindings  " which-key-show-top-level)
    ]
   ["Describe"
    ("C" "Command" helpful-command)
    ("f" "Function" helpful-callable)
    ("v" "Variable " helpful-variable)
    ("k" "Key" helpful-key)
    ("s" "Symbol" helpful-symbol)
    ("l" "Library" apropos-library)
    ]
   ["Info on"
    ("C-c" "Command" Info-goto-emacs-command-node)
    ("C-f" "Function" info-lookup-symbol)
    ("C-v" "Variable" info-lookup-symbol) ; fails if transient-detect-key-conflicts
    ("C-k" "Key" Info-goto-emacs-key-command-node)
    ("C-s" "Symbol" info-lookup-symbol)
    ]
   ["Goto Source"
    ""
    ("F" "Function" find-function-other-frame)
    ("V" "Variable" find-variable-other-frame)
    ("K" "Key" find-function-on-key-other-frame)
    ""
    ("L" "Library" find-library-other-frame)
    ]
   ["Apropos"
    ("ac" "Command" apropos-command)
    ("af" "Function" apropos-function)
    ("av" "Variable" apropos-variable)
    ("aV" "Value" apropos-value)
    ("aL" "Local Value" apropos-local-value)
    ("ad" "Documentation" apropos-documentation)
    ]
   ]
  [
   ["Internals"
    ("I" "Input Method" describe-input-method)
    ("G" "Language Env" describe-language-environment)
    ("S" "Syntax" describe-syntax)
    ("T" "Categories" describe-categories)
    ("O" "Coding System" describe-coding-system)
    ("o" "Coding Briefly" describe-current-coding-system-briefly)
    ("T" "Display Table" describe-current-display-table)
    ("e" "Echo Messages" view-echo-area-messages)
    ("H" "Lossage" view-lossage)
    ]
   ["Describe"
    ("." "At Point" helpful-at-point)
    ("c" "Key Short" describe-key-briefly)
    ("p" "Key Map" describe-keymap)
    ("A" "Face" describe-face)
    ("i" "Icon" describe-icon)
    ("w" "Where Is" where-is)
    ("=" "Position" what-cursor-position)
    ("g" "Shortdoc" shortdoc-display-group)
    ]
   ["Info Manuals"
    ("C-i" "Info" info)
    ("C-4" "Other Window" info-other-window)
    ("C-e" "Emacs" info-emacs-manual)
    ("C-l" "Elisp" info-elisp-manual)
    ("C-r" "Pick Manual" info-display-manual)
    ]
   ["External"
    ("N" "Man" consult-man)
    ("W" "Dictionary" lookup-word-at-point)
    ("D" "Dash" dash-at-point)
    ]
   ]
  )
(global-set-key (kbd "C-c C-S-h") 'hrm-help-transient)
