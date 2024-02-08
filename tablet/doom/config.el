;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Johan Widén"
      user-mail-address "j.e.widen@gmail.com")

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
 )
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(after! recentf
  (progn(setq recentf-max-saved-items 10000)
        ;; (run-at-time nil (* 5 60)
        ;;      (lambda ()
        ;;        (let ((save-silently t))
        ;;          (recentf-save-list))))
        ))
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

(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode 1)
(tooltip-mode 1)

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

(setq display-line-numbers-type nil)

(pcre-mode t)

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
    :after (org dired)
    :config
    ;; diredp requires dired-actual-switches to be a string, not nil, but
    ;; this variable is only non nil in dired buffers
    (setq dired-actual-switches "-al")
    ;; (setq diredp-image-preview-in-tooltip 300)
    )
;; (after! dired
;;   (load "/home/jw/Downloads/dired+.el"))

(use-package! bookmark+
  :after (org dired)
  ;;:defer t
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
    ;; (global-set-key (kbd "M-i") nil)
    ;; (global-set-key (kbd "M-i") #'help/hydra/left/describe/body)

(after! parent-mode
  (defun help/parent-mode-display ()
    "Display this buffer's mode hierarchy."
    (interactive)
    (let ((ls (parent-mode-list major-mode)))
      (princ ls))))
    )

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
