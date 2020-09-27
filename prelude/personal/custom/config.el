(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)

(setq load-prefer-newer t)
;; (require 'use-package)
(use-package auto-compile
 :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))
(add-to-list 'load-path "~/.emacs.d/personal/custom")
;; Get rid of pesky warnings about bytecomp interactive-p
;; (setq warning-minimum-level :error)
(setq gc-cons-threshold 100000000)

(setq user-full-name "Johan Widén")
(setq user-mail-address "j.e.widen@gmail.com")

(defvar jw/paradox-github-token nil)

(let ((secret.el (expand-file-name ".secret.el" "/home/jw")))
  (when (file-exists-p secret.el)
    (load secret.el)))

(setq prelude-guru nil)
(setq prelude-whitespace nil)
(set-fringe-style 8)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always, always, prefer UTF-8, anything else is insanity
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package modus-vivendi-theme
 :ensure t)
(setq prelude-theme 'modus-vivendi)
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))
(load-theme 'modus-vivendi t) ;; Dark theme

(add-to-list 'package-archives
 '("org" . "https://orgmode.org/elpa/")
 t)

(setq paradox-github-token jw/paradox-github-token)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; (setq default-frame-alist
;;  (quote ((width . 80)
;;          (height . 56))))
(server-start)
;; (run-at-time nil (* 30 60) 'recentf-save-list)
(run-with-idle-timer 600 t (lambda ()
                            (let ((save-silently t))
                             (recentf-save-list))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq image-dired-thumb-size 300)
(setq x-gtk-use-system-tooltips nil) ;; gtk tooltips do not work for me in i3
(setq confirm-kill-processes nil)

(setq-default
 help-window-select t             ; Focus new help windows when opened
 debug-on-error t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t
 sentence-end-double-space nil    ; End a sentence after a dot and a space
 window-combination-resize t      ; Resize windows proportionally
 x-stretch-cursor t               ; Stretch cursor to the glyph width
 history-delete-duplicates t
 )

(add-hook 'focus-out-hook #'garbage-collect)

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )

(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.  Also, commands that normally delete
;; just one character will delete the entire selection instead.
(delete-selection-mode)
(bind-key "RET" 'newline-and-indent)
(global-set-key (kbd "<dead-circumflex>") "^")
(global-set-key (kbd "<dead-tilde>") "~")
(global-set-key (kbd "M-+") 'xref-find-references) ; M-? is taken by smartparens

;; (use-package anzu
;;   :init
;;   (global-anzu-mode)
;;   (bind-key "M-%" 'anzu-query-replace)
;;   (bind-key "C-M-%" 'anzu-query-replace-regexp))

(use-package clean-aindent-mode
  :init
 (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package comment-dwim-2
 :init
 (global-set-key (kbd "M-;") 'comment-dwim-2)
 :config
 ;; (setq cd2/region-command 'cd2/comment-or-uncomment-lines)
 (setq cd2/region-command 'cd2/comment-or-uncomment-region)
)

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(python-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; (use-package iedit
;;   :bind (("C-;" . iedit-mode))
;;   :init
;;   (setq iedit-toggle-key-default nil))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1)
  )

;; (global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap mark-sexp] 'easy-mark)

(use-package mark-thing-at
 :ensure t
 :config
 (mark-thing-at-mode))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))

(use-package flycheck-pos-tip
  :config
  (custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  )

(use-package bind-key)

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

(use-package pcre2el
  :straight t
  :diminish (pcre-mode . "")
  :config
  (pcre-mode t))

(use-package visual-regexp
  :straight t
  :bind (;; Replace the regular query replace with the regexp query
         ;; replace provided by this package.
         ("M-%" . vr/query-replace)))

(use-package visual-regexp-steroids
  :straight t
  :after visual-regexp
  :config
  ;; Use Perl-style regular expressions by default.
  (setq vr/engine 'pcre2el))

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config
  (avy-setup-default)
)

(use-package ivy
  :straight t
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key (kbd "C-s") 'swiper)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    (define-key shell-mode-map (kbd "C-r") 'counsel-shell-history)))
(use-package ivy-hydra
  :straight t
)

(use-package counsel
  :straight t
  :bind
  (;;("M-x" . counsel-M-x)
   ("C-t" . counsel-M-x)
   ;;("M-y" . counsel-yank-pop)
   ;;("C-c r" . counsel-recentf)
   ;;("C-x C-f" . counsel-find-file)
   ;; ("<f1> f" . counsel-describe-function)
   ;; ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ;;("C-h f" . counsel-describe-function)
   ;;("C-h v" . counsel-describe-variable)
   ;;("C-h l" . counsel-load-library)
))

(use-package projectile
  :straight t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;; (use-package counsel-projectile
;;   :ensure t
;; )

;; (use-package company
;;   :init
;;   (use-package company-c-headers)
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode)

;;   ;; (add-hook 'haskell-mode-hook 'company-mode)
;;   ;; (add-to-list 'company-backends 'company-ghc)
;;   (add-to-list 'company-backends 'company-c-headers)
;;   (add-to-list 'company-c-headers-path-system "/usr/include/c++/6/")
;;   ;; (custom-set-variables '(company-ghc-show-info t))
;;   (custom-set-variables '(company-show-numbers t))
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   (define-key c-mode-map  [(control tab)] 'company-complete)
;;   (define-key c++-mode-map  [(control tab)] 'company-complete)
;;   )
;; ;; (define-key c-mode-map  [(tab)] 'company-complete)
;; ;; (define-key c++-mode-map  [(tab)] 'company-complete)

(use-package company-auctex
  :straight t
  :config
  (company-auctex-init))

(use-package company-quickhelp
  :straight t
  :config
  (company-quickhelp-mode 1)
)

(use-package rainbow-blocks
  :straight t
)

;; (use-package info-colors
;;  :init
;;  (add-hook 'Info-selection-hook 'info-colors-fontify-node)
;; )

(defun th-activate-mark-init ()
  (setq cursor-type 'bar))
(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init ()
  (setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)

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

(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt
                        face minibuffer-prompt)))

(use-package mouse-copy
    :straight t
    :config
    (bind-key [C-down-mouse-1] 'mouse-drag-secondary-pasting)
    (bind-key [C-S-down-mouse-1] 'mouse-drag-secondary-moving)
  )

(global-set-key [remap mouse-drag-secondary] 'mouse-drag-region)
(global-set-key [remap mouse-set-secondary] 'mouse-set-region)
(global-set-key [remap mouse-start-secondary] 'mouse-set-point)
(global-set-key [remap mouse-yank-secondary] 'mouse-yank-primary)
(global-set-key [remap mouse-secondary-save-then-kill] 'mouse-save-then-kill)

;; show unncessary whitespace that can mess up your diff
(add-hook 'diff-mode-hook
          (lambda ()
            (setq-local whitespace-style
                        '(face
                          tabs
                          tab-mark
                          spaces
                          space-mark
                          trailing
                          indentation::space
                          indentation::tab
                          newline
                          newline-mark))
            (whitespace-mode 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local whitespace-style
                        '(tab-mark
                          trailing))
            (whitespace-mode 1)))

(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq tab-width 4)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Toggle whitespace-mode to view all whitespace characters
(bind-key "C-c w" 'whitespace-mode)

(use-package hungry-delete
  :straight t
  :config
  (global-hungry-delete-mode))

(use-package ws-butler
  :straight t
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

;(set-face-attribute 'default nil :font
; "-unknown-Liberation Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
;; (set-face-attribute 'default nil :font
;; "-*-anonymous pro-medium-r-normal-*-16-*-*-*-m-*-iso10646-1")
;(set-face-attribute 'default nil :font
; "-*-cousine-medium-r-normal-*-13-*-*-*-m-*-iso10646-1")
;; (set-face-attribute 'default nil :font
;; "-*-dejavu sans mono-medium-r-normal-*-13-*-*-*-m-*-iso10646-1")
;; (set-face-attribute 'default nil :font
;; "-*-droid sans mono-medium-r-normal-*-13-*-*-*-m-*-iso10646-1")
;(set-face-attribute 'default nil :font
; "-*-fira mono-regular-r-normal-*-13-*-*-*-m-*-iso10646-1")
;(set-face-attribute 'default nil :font
; "-*-hermit-medium-r-normal-*-13-*-*-*-m-*-iso10646-1")
;; (set-face-attribute 'default nil :font
;; "-*-inconsolata-medium-r-normal-*-16-*-*-*-m-*-iso10646-1")
;(set-face-attribute 'default nil :font
; "-*-oxygen mono-medium-r-normal-*-13-*-*-*-m-*-iso10646-1")
;(set-face-attribute 'default nil :font
; "-*-pt mono-regular-r-normal-*-13-*-*-*-m-*-iso10646-1")
;; (set-face-attribute 'default nil :font
;;  "-*-source code pro-regular-r-normal-*-13-*-*-*-m-*-iso10646-1")

;; (use-package mixed-pitch
;;   :ensure t
;;   :config
;;   ;; If you want it in all text modes:
;;   ;; (add-hook 'text-mode-hook #'mixed-pitch-mode)
;;   ;; (add-hook 'org-mode-hook #'mixed-pitch-mode)
;;   ;; Depending on your specific setup, you may want to adjust the height of
;;   ;; variable pitch fonts:
;;   (set-face-attribute 'variable-pitch nil :height 100)
;;   )

;; (defcustom org-variable-pitch-fixed-font "Hack"
;;   "Monospace font to use with ‘org-variable-pitch-minor-mode’."
;;   :group 'org-variable-pitch
;;   :type 'string
;;   :risky t)
;; (setq org-variable-pitch-fixed-faces
;;   '(org-block
;;     org-block-begin-line
;;     org-block-end-line
;;     org-code
;;     org-date
;;     org-document-info-keyword
;;     org-done
;;     org-formula
;;     org-link
;;     org-meta-line
;;     org-property-value
;;     org-special-keyword
;;     org-table
;;     org-tag
;;     org-todo
;;     org-verbatim))
;; (require 'org-variable-pitch)
;; (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

(use-package emacs
  :commands (prot/font-set-face-attribute
             prot/font-set-fonts
             prot/font-set-font-size-family
             prot/font-line-spacing
             prot/font-fonts-per-monitor)
  :config
  (setq x-underline-at-descent-line t)
  (setq underline-minimum-offset 1)

  (defconst prot/font-fontconfig-params
    "embeddedbitmap=false:autohint=false:hintstyle=hintslight"
    "Additional parameters for the given font family.
These are specific to the fontconfig backend for GNU/Linux systems.")

  (defvar prot/font-switch-fonts-hook nil
    "Hook that is called from `prot/font-set-fonts-completion'.")

  ;; The idea with this association list is to use font combinations
  ;; that are suitable to the given point size.  I find that at smaller
  ;; sizes the open and wide proportions of Hack+FiraGO work well, while
  ;; the more compact Iosevka+Source Sans Pro are better at larger point
  ;; sizes.  The "desktop" combo is ideal for use on a larger monitor at
  ;; a regular point size.
  ;;
  ;; Note that the "Hack" mentioned here is my patched version of it,
  ;; which uses some alternative glyphs, is built on top of the latest
  ;; dev branch, and is meant to improve both the Roman and Italic
  ;; variants (alt glyphs are part of the Hack project):
  ;; https://gitlab.com/protesilaos/hackfontmod
  (defconst prot/font-sizes-families-alist
    '(("laptop" . (10.5 "Hack" "FiraGO"))
	  ("desktop" . (12 "Ubuntu Mono" "FiraGO"))
	  ("presentation" . (19 "Iosevka SS08" "Source Sans Pro")))
    "Alist of desired point sizes and their typefaces.
Each association consists of a display type mapped to a point
size, followed by monospaced and proportionately-spaced font
names.

The monospaced typeface is meant to be applied to the `default'
and `fixed-pitch' faces.  The proportionately-space font is
intended for the `variable-pitch' face.")

  (defun prot/font-set-face-attribute (face family size &optional params)
    "Set FACE font to FAMILY at SIZE with optional PARAMS."
    (let ((params (if params
			          params
		            prot/font-fontconfig-params)))
	  (set-face-attribute
	   `,face nil :font
	   (format "%s-%s:%s" family (number-to-string size) params))))

  (defun prot/font-set-fonts (&optional points font-mono font-var)
    "Set default font size using presets.

POINTS is the font's point size, represented as either '10' or
'10.5'.  FONT-MONO should be a monospaced typeface, due to the
alignment requirements of the `fixed-pitch' face.  FONT-VAR could
be a proportionately-spaced typeface or even a monospaced one,
since the `variable-pitch' it applies to is not supposed to be
spacing-sensitive.  Both families must be represented as a string
holding the family's name."
    (interactive)
    (let* ((data prot/font-sizes-families-alist)
	       (displays (mapcar #'car data))
	       (choice (if points
                       points
		             (completing-read "Pick display size: " displays nil t)))
	       (size (if points
		             points
		           (nth 1 (assoc `,choice data))))
	       (mono (if font-mono
		             font-mono
		           (if (member choice displays)
                       (nth 2 (assoc `,choice data))
                     nil)))
	       (var (if font-var
		            font-var
                  (if (member choice displays)
                      (nth 3 (assoc `,choice data))
                    nil))))
	  (when window-system
        (dolist (face '(default fixed-pitch))
	      (prot/font-set-face-attribute `,face mono size))
	    ;; Increasing the size on this to account for the innate
	    ;; difference between the families I use.  Maybe there is some
	    ;; more flexible way to create visual harmony between typefaces
	    ;; with distinct inherent heights, without trying to query for
	    ;; the current family in use.
        ;;
        ;; TODO normalise font heights automatically?
	    (prot/font-set-face-attribute 'variable-pitch var (+ size 1))))
	(run-hooks 'prot/font-switch-fonts-hook))

  (defvar prot/font-monospaced-fonts-list
    '("Hack" "Iosevka SS08" "Iosevka Slab" "Source Code Pro"
      "Ubuntu Mono" "Fantasque Sans Mono" "DejaVu Sans Mono"
      "Fira Code" "Victor Mono" "Roboto Mono")
    "List of typefaces for coding.
See `prot/font-set-font-size-family' for how this is used.")

  (defun prot/font-set-font-size-family ()
    "Set point size and main typeface."
    (interactive)
    (let* ((fonts prot/font-monospaced-fonts-list)
           (font (completing-read "Select main font: " fonts nil t))
           (nums (list 13 14 15 16))
           (sizes (mapcar 'number-to-string nums))
           (size (completing-read "Select or insert number: " sizes nil)))
      (prot/font-set-fonts (string-to-number size) font)))

  (defun prot/font-fonts-dwim (&optional arg)
    "Set fonts interactively.
This is just a wrapper around `prot/font-set-fonts' and
`prot/font-set-font-size-family', whose sole purpose is to
economise on dedicated key bindings."
    (interactive "P")
    (if arg
        (prot/font-set-font-size-family)
      (prot/font-set-fonts)))

  (defvar prot/font-fonts-line-spacing-list
    '("Source Code Pro" "Ubuntu Mono")
    "Font families in need of extra `line-spacing'.
See `prot/font-line-spacing' for how this is used.")

  (defun prot/font-line-spacing ()
	"Determine desirable `line-spacing', based on font family.
Add this to `prot/font-switch-fonts-hook'."
	(let ((fonts prot/font-fonts-line-spacing-list))
	  (if (member (face-attribute 'default :family) fonts)
	      (setq-default line-spacing 1)
	    (setq-default line-spacing nil))))

  (defun prot/font-fonts-per-monitor ()
	"Use font settings based on screen size.
Meant to be used at some early initialisation stage, such as with
`after-init-hook'."
	(let* ((display (if (<= (display-pixel-width) 1366)
			            "laptop"
		              "desktop"))
	       (data prot/font-sizqes-families-alist)
           (size (cadr (assoc `,display data)))
	       (mono (nth 2 (assoc `,display data)))
	       (var (nth 3 (assoc `,display data))))
	  (prot/font-set-fonts size mono var)))

  :hook ((after-init-hook . prot/font-fonts-per-monitor)
	     (prot/font-switch-fonts-hook . prot/font-line-spacing))
  ;; Awkward key because I do not need it very often.  Maybe once a day.
  ;; The "C-c f" is used elsewhere.
  :bind ("C-c F" . prot/font-fonts-dwim))

(use-package face-remap
  :diminish buffer-face-mode            ; the actual mode
  :commands prot/variable-pitch-mode
  :config
  (define-minor-mode prot/variable-pitch-mode
    "Toggle `variable-pitch-mode', except for `prog-mode'."
    :init-value nil
    :global nil
    (if prot/variable-pitch-mode
        (unless (derived-mode-p 'prog-mode)
          (variable-pitch-mode 1))
      (variable-pitch-mode -1))))

(setq modus-vivendi-theme-proportional-fonts t)
(setq modus-vivendi-theme-rainbow-headings nil)
(setq modus-vivendi-theme-scale-headings t)
(setq modus-vivendi-theme-bold-constructs t)
(setq modus-vivendi-theme-visible-fringes t)
(setq modus-vivendi-theme-intense-paren-match nil)
(setq modus-vivendi-theme-variable-pitch-headings t)
(set-face-attribute 'default nil :font "Ubuntu Mono-12")
(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)

;; (dolist (face '(default fixed-pitch))
;;   (set-face-attribute `,face nil :font "Ubuntu Mono-12"))

;; (variable-pitch-mode)

(use-package unicode-fonts
  :straight t
  :config
    (unicode-fonts-setup))

(windmove-default-keybindings)
(global-set-key (kbd "<kp-4>") 'windmove-left)
(global-set-key (kbd "<kp-6>") 'windmove-right)
(global-set-key (kbd "<kp-8>") 'windmove-up)
(global-set-key (kbd "<kp-2>") 'windmove-down)

(global-set-key (kbd "C-x o") 'ace-window)

(use-package zygospore
  :straight t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq sml/theme 'dark)
;;   (setq sml/theme 'automatic)
;;   (setq sml/shorten-directory t)
;;   (setq sml/shorten-modes t)
;;   (setq sml/name-width 30)
;;   (setq sml/mode-width 'full)
;;   (setq rm-blacklist '(" all-the-icons-dired-mode" " Abbrev" " GitGutter" " EditorConfig" " Fly" " MRev" " company" " mate"
;;                        " h" " Helm" " ivy" " MPM" " Wrap" " Fill" " AC" " Pre"
;;                        " Prjl" " super-save" " Undo-Tree" " VHl" " WK" " WLR" " ws"))
;;   (sml/setup)
;;   (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
;;   (add-to-list 'sml/replacer-regexp-list
;;                '("^~/.*/lib/ruby/gems" ":GEMS" ))
;; )

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

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "~/org/roam/"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package org
  ;;:pin org
  :straight org-plus-contrib
  ;; :ensure org-plus-contrib
  :config
  (setq org-fontify-whole-heading-line t)
  (setq org-use-speed-commands t)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
)

(setq org-agenda-files (directory-files "~/org-files" 'absolute "[^#]*.org$" 'no-sort))
(setq jethro/org-agenda-directory "~/org-files/")

(use-package org-agenda
  :straight nil
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

(require 'org-protocol)
(require 'org-capture)

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
                            ("@school" . ?s)
                            (:newline)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c))))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))

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

(use-package org-clock-convenience
  :straight t
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

;; (add-hook 'org-mode-hook #'writeroom-mode)
;; (add-hook 'writeroom-mode-hook #'+word-wrap-mode)
;; (add-hook 'writeroom-mode-hook #'+org-pretty-mode)

(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))

(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which
do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      'my/org-add-ids-to-headlines-in-file nil 'local)))

(defun my/copy-id-to-clipboard() "Copy the ID property value to killring,
if no ID is there then create a new unique ID.
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
  (interactive)
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
  (kill-new mytmpid)
  (message "Copied %s to killring (clipboard)" mytmpid)
))

;; (global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)

(setq org-M-RET-may-split-line '((item . nil)))

(use-package org-pdfview
  :straight t
)
(eval-after-load "org"
  '(progn
     ;; (delete '("\\.pdf\\'" . default) org-file-apps)
     (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)  ; in my case /bin/bash
   (scheme . t)
   (python . t)
   (hledger . t)
   (ledger . t)
   (sed . t)
   (awk . t)
   (clojure . t)))

(use-package org-download
  :straight t
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package image+)
(eval-after-load 'image '(require 'image+))

(eval-after-load 'image+
  `(when (require 'hydra nil t)
     (defhydra imagex-sticky-binding (global-map "C-x C-l")
       "Manipulating Image"
       ("+" imagex-sticky-zoom-in "zoom in")
       ("-" imagex-sticky-zoom-out "zoom out")
       ("M" imagex-sticky-maximize "maximize")
       ("O" imagex-sticky-restore-original "restore original")
       ("S" imagex-sticky-save-image "save file")
       ("r" imagex-sticky-rotate-right "rotate right")
       ("l" imagex-sticky-rotate-left "rotate left"))))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(use-package org-pandoc-import
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))

(use-package org-make-toc
  :straight t
)

(setq python-shell-interpreter "python3")
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(use-package smartparens
  :straight t
  :config
  ;; Haskell
  (sp-with-modes '(
                   haskell-mode
                   inferior-haskell-mode
                   erlang-mode
                   )
    ;; math modes, yay.  The :actions are provided automatically if
    ;; these pairs do not have global definition.
    (sp-local-pair "`" "`"))
  (add-hook 'inferior-haskell-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'haskell-interactive-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'erlang-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'erlang-shell-mode-hook (lambda () (smartparens-mode 1)))
)

(use-package thingatpt+
  :straight (:host github
             :repo "emacsmirror/thingatpt-plus"))

(use-package hide-comnt
  :straight (:host github
             :repo "emacsmirror/hide-comnt"))

(use-package thing-cmds
  :straight (:host github
             :repo "emacsmirror/thing-cmds"))

(use-package hexrgb
  :straight (:host github
             :repo "emacsmirror/hexrgb"))

(use-package palette
  :straight (:host github
             :repo "emacsmirror/palette"))

(use-package facemenu+
  :straight (:host github
             :repo "emacsmirror/facemenu-plus"))

(use-package highlight
  :straight (:host github
             :repo "emacsmirror/highlight"))

(use-package mouse3
  :straight (:host github
             :repo "emacsmirror/mouse3"))

(setq dired-listing-switches "-laGh1v --group-directories-first")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-clean-up-buffers-too nil)
(setq dired-dwim-target t)
(defconst my-dired-media-files-extensions
 '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "wmv" "mkv" "mov" "wma")
  "Media file extensions that should launch in VLC.
Also used for highlighting.")

(use-package all-the-icons-dired)

(defvar-local +wdired-icons-enabled nil)
(defun +wdired-before-start-advice ()
  "Execute when switching from `dired' to `wdired'."
  (setq +wdired-icons-enabled (if (bound-and-true-p all-the-icons-dired-mode)
                                  1 0))
  (when (bound-and-true-p all-the-icons-dired-mode)
    (all-the-icons-dired-mode 0)))
(defun +wdired-after-finish-advice ()
  "Execute when switching from `wdired' to `dired'"
  (when (boundp 'all-the-icons-dired-mode)
    (all-the-icons-dired-mode +wdired-icons-enabled)))
(advice-add 'wdired-change-to-wdired-mode :before #'+wdired-before-start-advice)
(advice-add 'wdired-change-to-dired-mode :after #'+wdired-after-finish-advice)

(use-package dired-x
  :straight nil
  :config
  (progn
    (defun dired-virtual-revert (&optional _arg _noconfirm)
      "Enable revert for virtual direds."
      (let ((m (dired-file-name-at-point))
            (buffer-modified (buffer-modified-p)))
        (goto-char 1)
        (dired-next-subdir 1)
        (dired-do-redisplay nil t)
        (while (dired-next-subdir 1 t)
          (dired-do-redisplay nil t))
        (when m (dired-goto-file m))
        (set-buffer-modified-p buffer-modified)))

    (defun my-dired-jump ()
      (interactive)
      (if (eq major-mode 'dired-mode)
          (let ((file (dired-utils-get-filename)))
            (dired (f-parent file))
            (dired-utils-goto-line file))
        (dired-jump)))
    (bind-key "C-j" 'my-dired-jump ctl-x-map)

    (add-to-list 'dired-guess-shell-alist-user
                 (list (concat "\\."
                               (regexp-opt my-dired-media-files-extensions)
                               "\\'")
                       "vlc"))))

(use-package diredfl
    :straight t)
(add-hook 'dired-mode-hook 'diredfl-mode)
(define-key dired-mode-map "e"
  (lambda () (interactive)
    (eww-open-file (dired-get-file-for-visit))))

(use-package dired-subtree
  :straight t
  :init
  (bind-keys :map dired-mode-map
             :prefix "C-,"
             :prefix-map dired-subtree-map
             :prefix-docstring "Dired subtree map."
    ("C-i" . dired-subtree-insert)
    ("C-/" . dired-subtree-apply-filter)
    ("C-k" . dired-subtree-remove)
    ("C-n" . dired-subtree-next-sibling)
    ("C-p" . dired-subtree-previous-sibling)
    ("C-u" . dired-subtree-up)
    ("C-d" . dired-subtree-down)
    ("C-a" . dired-subtree-beginning)
    ("C-e" . dired-subtree-end)
    ("C-c" . dired-subtree-cycle)
    ("m" . dired-subtree-mark-subtree)
    ("u" . dired-subtree-unmark-subtree)
    ("C-o C-f" . dired-subtree-only-this-file)
    ("C-o C-d" . dired-subtree-only-this-directory)))

(use-package dired-avfs
  :straight t
)
;; (eval-after-load "dired-aux"
;;   '(add-to-list 'dired-compress-file-suffixes
;;                 '("\\.zip\\'" ".zip" "unzip")))

(bind-keys :map dired-mode-map
           ("ö" . dired-filter-map)
           ("ä" . dired-filter-mark-map))
(use-package dired-filter
  :straight t
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

(use-package dired-launch
  :straight (:host github
             :repo "thomp/dired-launch"))
(dired-launch-enable)

(use-package dired-ranger
  :straight t
  :config
  (setq dired-ranger-bookmark-LRU ?l)
  (bind-keys :map dired-mode-map
             :prefix "c"
             :prefix-map dired-ranger-map
             :prefix-docstring "Map for ranger operations."
    ("c" . dired-ranger-copy)
    ("p" . dired-ranger-paste)
    ("m" . dired-ranger-move))
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)
              ("'" . dired-ranger-bookmark)
              ("l" . dired-ranger-bookmark-visit))
)
(use-package ranger
  :straight t
)

(use-package dired-narrow
  :straight t
  :commands dired-narrow
  :init
  (bind-key "å" 'dired-narrow dired-mode-map))

(use-package peep-dired
  :straight t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(defun counsel-goto-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection :action 'dired)))

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  (bind-keys :map dired-mode-map
    ;; clean bullshit bindings so C-h e b shows us real info
    ("G") ("Z") ("#") (".")
    ("~") ("e") ("?")

    ("<insert>" . dired-mark)
    ("SPC" . dired-mark)
    ("<delete>" . dired-unmark-backward)
    ("<backspace>" . dired-up-directory))

  (dired-filter-mode t)
  (dired-filter-group-mode t)
  ;; (dired-collapse-mode 1)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))
(add-hook 'dired-mode-hook 'my-dired-init)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired+
  :straight (:host github
             :repo "emacsmirror/dired-plus")
  :config
  (setq diredp-image-preview-in-tooltip 300)
)

(use-package bookmark+
  :straight (:host github
             :repo "emacsmirror/bookmark-plus"))

(use-package info+
    :straight (:host github
               :repo "emacsmirror/info-plus")
    :config
    (setq Info-breadcrumbs-in-mode-line-mode t)
  )
(eval-after-load "info" '(require 'info+))

(use-package man
  :straight t
  :init (setq Man-notify-method 'aggressive))

(use-package neotree
  :straight t
  :bind (("<f8>" . neotree-toggle))
)

(use-package w3m
  :straight t
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

(use-package ace-link
  :straight t
  :config
  (ace-link-setup-default)
)

;; (setq browse-url-browser-function 'eww-browse-url
;;       browse-url-new-window-flag t)
(setq browse-url-mosaic-program nil)
(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(define-prefix-command 'web-map)
(define-key web-map (kbd "w") 'w3m-goto-url)
(define-key web-map (kbd "l") 'browse-url-at-point)
(define-key web-map (kbd "g") 'w3m-search)
(global-set-key (kbd "C-c w") 'web-map)
;; (global-set-key (kbd "C-c w w") 'w3m-goto-url)
;; (global-set-key (kbd "C-c w l") 'browse-url-at-point)
;; (global-set-key (kbd "C-c w g") 'w3m-search)
(autoload 'browse-url-interactive-arg "browse-url")

(defvar my-w3m-last-windows nil)

(defun my-store-pre-w3m-winconfig ()
  (interactive)
  (setq my-w3m-last-windows (current-window-configuration)))

(defun my-restore-pre-w3m-winconfig ()
  (interactive)
  (set-window-configuration my-w3m-last-windows))

;; (add-hook 'w3m-select-buffer-hook #'my-store-pre-w3m-winconfig)
;; (add-hook 'w3m-delete-buffer-hook #'my-restore-pre-w3m-winconfig)

(use-package helm-w3m
  :straight t
)

(use-package keyword-search
  :straight t
)

(use-package clang-format
  :commands clang-format-region)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp-mode)))
  :config
  (bind-keys :map c++-mode-map
             :prefix "C-ö"
             :prefix-map my-ccls-map
             :prefix-docstring "ccls map."
    ("C-h" . (lambda () (interactive) (ccls-navigate "U")))
    ("C-j" . (lambda () (interactive) (ccls-navigate "R")))
    ("C-k" . (lambda () (interactive) (ccls-navigate "L")))
    ("C-l" . (lambda () (interactive) (ccls-navigate "D")))
    ("m" . (lambda () (interactive) (ccls/references-not-call)))
    ("m" . (lambda () (interactive) (ccls/references-macro)))
    ("r" . (lambda () (interactive) (ccls/references-read)))
    ("w" . (lambda () (interactive) (ccls/references-write)))
    ("lp" . (lambda () (interactive) (ccls-preprocess-file)))
    ("lf" . (lambda () (interactive) (ccls-reload)))
    ("=" . (lambda () (interactive) (clang-format-region)))
  )
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq ccls-sem-highlight-method 'font-lock)
  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
  (ccls-use-default-rainbow-sem-highlight)
  (setq
   ccls-initialization-options
   `(:clang
     (:excludeArgs
      ;; Linux's gcc options. See ccls/wiki
      ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
       "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
       "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
      :extraArgs ["--gcc-toolchain=/usr"]
     )
     :completion
     (:include
      (:blacklist
       ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
        "^/usr/(local/)?include/c\\+\\+/v1/"
        ]))
     :index (:trackDependency 1)))

  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

  (defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))
)

;; (use-package lsp-mode
;;   :config
;;   (add-hook 'c++-mode-hook #'lsp)
;;   (add-hook 'python-mode-hook #'lsp)
;;   (add-hook 'rust-mode-hook #'lsp))

  ;; (use-package lsp-mode :commands lsp :ensure t)
  ;; (use-package lsp-ui :commands lsp-ui-mode :ensure t)
  ;; (use-package company-lsp
  ;;   :ensure t
  ;;   :commands company-lsp
  ;;   :config (push 'company-lsp company-backends)) ;; add company-lsp as a backend

    (use-package lsp-mode
      :straight t
      :commands lsp)

    (use-package lsp-ui
      :straight t
      :commands lsp-ui-mode
      :config
      (setq lsp-ui-sideline-enable t
            lsp-ui-sideline-show-symbol t
            lsp-ui-sideline-show-hover t
            lsp-ui-sideline-show-code-actions t
            lsp-ui-sideline-update-mode 'point
            lsp-ui-doc-enable t
            lsp-ui-peek-enable t
            lsp-ui-peek-always-show t)
      (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
      (define-key lsp-ui-mode-map (kbd "C-c C-m .") 'lsp-ui-peek-find-definitions)
      (define-key lsp-ui-mode-map (kbd "C-c C-m ?") 'lsp-ui-peek-find-references)
      (define-key lsp-ui-mode-map (kbd "C-c C-m r") 'lsp-rename)
      (define-key lsp-ui-mode-map (kbd "C-c C-m x") 'lsp-restart-workspace)
      (define-key lsp-ui-mode-map (kbd "C-c C-m w") 'lsp-ui-peek-find-workspace-symbol)
      (define-key lsp-ui-mode-map (kbd "C-c C-m i") 'lsp-ui-peek-find-implementation)
      (define-key lsp-ui-mode-map (kbd "C-c C-m d") 'lsp-describe-thing-at-point)
    )

    (use-package company-lsp
      :straight t
      :commands company-lsp)

    ;; (require 'lsp-mode)
    ;; (add-hook 'python-mode-hook #'lsp)
    ;; (require 'flymake-shell)
    ;; (add-hook 'sh-set-shell-hook 'flymake-shell-load)
    ;;(add-hook 'sh-mode-hook #'lsp-mode)
    ;;(add-hook 'c-mode-hook #'lsp)   ;; clangd, cquery
    ;;(add-hook 'c++-mode-hook #'lsp) ;; clangd, cquery
    ;; (use-package lsp-mode
    ;;   :ensure t
    ;;   :config

    ;;   ;; make sure we have lsp-imenu everywhere we have LSP
    ;;   ;; (require 'lsp-imenu)
    ;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    ;;   ;; get lsp-python-enable defined
    ;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
    ;;   ;;     or any other function that can be used to find the root directory of a project
    ;;   ;; (lsp-define-stdio-client lsp-python "python"
    ;;   ;;                          #'projectile-project-root
    ;;   ;;                          '("pyls"))

    ;;   ;; make sure this is activated when python-mode is activated
    ;;   ;; lsp-python-enable is created by macro above
    ;;   (add-hook 'python-mode-hook
    ;;             (lambda ()
    ;;               (lsp)))

    ;;   ;; lsp extras
    ;;   (use-package lsp-ui
    ;;     :ensure t
    ;;     :config
    ;;     (setq lsp-ui-sideline-ignore-duplicate t)
    ;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode))

    ;;   (use-package company-lsp
    ;;     :config
    ;;     (push 'company-lsp company-backends))

    ;;   ;; NB: only required if you prefer flake8 instead of the default
    ;;   ;; send pyls config via lsp-after-initialize-hook -- harmless for
    ;;   ;; other servers due to pyls key, but would prefer only sending this
    ;;   ;; when pyls gets initialised (:initialize function in
    ;;   ;; lsp-define-stdio-client is invoked too early (before server
    ;;   ;; start)) -- cpbotha
    ;;   (defun lsp-set-cfg ()
    ;;     (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
    ;;       ;; TODO: check lsp--cur-workspace here to decide per server / project
    ;;       (lsp--set-configuration lsp-cfg)))

    ;;   (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

;; (use-package erlang-start
;;   :config
;;   (setq erlang-root-dir "/usr/lib64/erlang")
;;   (add-to-list 'exec-path "/usr/lib64/erlang/bin")
;;   (setq erlang-man-root-dir "/usr/lib64/erlang/man")
;;   )

;; (custom-set-variables
;;  '(haskell-process-suggest-remove-import-lines t)
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t)
;;  '(haskell-process-type (quote cabal-repl)) ;; mui importante
;;  '(haskell-tags-on-save t)
;;  '(inferior-haskell-wait-and-jump t))

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; ;(add-hook 'haskell-mode-hook 'haskell-indent-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; ;(add-hook 'haskell-mode-hook 'haskell-doc-mode)

;; (eval-after-load 'haskell-mode '(progn
;;     (define-key haskell-mode-map (kbd "C-c C-h")
;;       (lambda ()
;;         (interactive)
;;         (keyword-search-at-point "hayoo")))
;;     (bind-keys :map haskell-mode-map
;;                ("C-c h" . 'haskell-hoogle)
;;                ("C-c C-l" . 'haskell-process-load-or-reload)
;;                ("C-c C-n C-t" . 'haskell-process-do-type)
;;                ("C-c C-n C-i" . 'haskell-process-do-info)
;;                ("C-c C-n C-c" . 'haskell-process-cabal-build)
;;                ("C-c C-n c" . 'haskell-process-cabal)
;;                ("C-c C-o" . 'haskell-compile)
;;                ("SPC" . 'haskell-mode-contextual-space))))

;; (eval-after-load 'haskell-cabal
;;   '(progn
;;      (bind-keys :map haskell-cabal-mode-map
;;                 ("C-c C-k" . 'haskell-interactive-mode-clear)
;;                 ("C-c C-c" . 'haskell-process-cabal-build)
;;                 ("C-c C-o" . 'haskell-compile)
;;                 ("C-c c" . 'haskell-process-cabal))))

(use-package json)

(use-package julia-mode)
(use-package julia-repl)
(use-package flycheck-julia)
(flycheck-julia-setup)

(use-package ein)

;; (setq lua-documentation-url "http://www.lua.org/manual/5.1/manual.html")
;; (setq lua-documentation-url "http://www.lua.org/manual/5.2/manual.html")
(setq lua-documentation-url "file:///usr/share/doc/lua5.2-doc/manual.html")

(use-package raku-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package eval-in-repl)

(use-package eval-in-repl-ielm
  :straight nil
  :bind (:map emacs-lisp-mode-map
         ("<C-return>" . eir-eval-in-ielm) ;; for .el files
         :map lisp-interaction-mode-map
         ("<C-return>" . eir-eval-in-ielm) ;; for *scratch*
         :map Info-mode-map
         ("<C-return>" . eir-eval-in-ielm) ;; for M-x info
         ))

;; (use-package eval-in-repl-cider
;;   :bind (:map clojure-mode-map
;;               ("<C-return>" . eir-eval-in-cider)
;;         )
;;   )

(use-package eval-in-repl-shell
  :straight nil
  :config
  (add-hook 'sh-mode-hook
            '(lambda()
               (local-set-key (kbd "C-<return>") 'eir-eval-in-shell))))

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/personal/custom/config.org"))
;; (global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Reloads ~/.emacs.d/personal/custom/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/personal/custom/config.org")))

(define-prefix-command 'launcher-map)
;; The manual recommends C-c for user keys, but C-x l is
;; always free and is natural to remember.
(define-key ctl-x-map "l" 'launcher-map)
;;(global-set-key (kbd "s-l") 'launcher-map)
(global-set-key (kbd "H-l") 'launcher-map)
;; Note: keys o and c are defined in link-hint
(define-key launcher-map "C" #'org-capture)
(define-key launcher-map "d" #'ediff-buffers)
(define-key launcher-map "e" #'er/expand-region)
(define-key launcher-map "E" #'er/contract-region)
(define-key launcher-map "f" #'find-dired)
(define-key launcher-map "g" #'lgrep)
(define-key launcher-map "G" #'rgrep)
(define-key launcher-map "h" #'man) ; Help
(define-key launcher-map "i" #'package-install-from-buffer)
(define-key launcher-map "j" #'org-journal-new-entry)
(define-key launcher-map "p" #'paradox-list-packages)
(define-key launcher-map "r" #'config-reload)
(define-key launcher-map "R" #'counsel-recentf)
(define-key launcher-map "s" #'sunrise)
(define-key launcher-map "S" #'sunrise-cd)
(define-key launcher-map "t" #'proced) ; top
(define-key launcher-map "u" #'my/copy-id-to-clipboard)
(define-key launcher-map "v" #'config-visit)
(define-key launcher-map "y" #'counsel-yank-pop)

(use-package link-hint
  :ensure t
  :config
  (define-key launcher-map "o" #'link-hint-open-link)
  (define-key launcher-map "c" #'link-hint-copy-link))

(defun sacha/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))

(defun sacha/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))

(global-set-key "\M-0" 'delete-window)
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-2" 'sacha/vsplit-last-buffer)
(global-set-key "\M-3" 'sacha/hsplit-last-buffer)
(global-set-key (kbd "M-4") #'er/expand-region)

(use-package hydra)
(use-package hydra-examples
 :straight nil)

(defhydra hydra-error ()
   "goto-error"
   ("h" first-error "first")
   ("j" next-error "next")
   ("k" previous-error "prev")
   ("v" recenter-top-bottom "recenter")
   ("q" nil "quit"))

(defhydra hydra-splitter ()
   "splitter"
   ("h" hydra-move-splitter-left)
   ("j" hydra-move-splitter-down)
   ("k" hydra-move-splitter-up)
   ("l" hydra-move-splitter-right))

(defhydra hydra-toggle (:color pink)
  "
            _a_ abbrev-mode:       %`abbrev-mode
            _d_ debug-on-error:    %`debug-on-error
            _f_ auto-fill-mode:    %`auto-fill-function
            _g_ golden-ratio-mode: %`golden-ratio-mode
            _t_ truncate-lines:    %`truncate-lines
            _w_ whitespace-mode:   %`whitespace-mode
            "
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("g" golden-ratio-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("q" nil "cancel"))

(defhydra hydra-window (:color red
                        :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
  ;("t" transpose-frame "'")
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" make-frame :exit t)
  ("s" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ;("i" ace-maximize-window "ace-one" :color blue)
  ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))
(global-set-key (kbd "C-M-o") 'hydra-window/body)

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" exchange-point-and-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))
(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

(defhydra hydra-buffer-menu (:color pink)
  "
  Mark               Unmark             Actions            Search
-------------------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete                           _g_: refresh       _O_: multi-occur
_D_: delete up                        _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark nil)
  ("u" Buffer-menu-unmark nil)
  ("U" Buffer-menu-backup-unmark nil)
  ("d" Buffer-menu-delete nil)
  ("D" Buffer-menu-delete-backwards nil)
  ("s" Buffer-menu-save nil)
  ("~" Buffer-menu-not-modified nil)
  ("x" Buffer-menu-execute nil)
  ("b" Buffer-menu-bury nil)
  ("g" revert-buffer nil)
  ("T" Buffer-menu-toggle-files-only nil)
  ("O" Buffer-menu-multi-occur nil :color blue)
  ("I" Buffer-menu-isearch-buffers nil :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp nil :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" xref-find-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-customize-apropos (:color blue)
  "Apropos (customize)"
  ("a" customize-apropos "apropos")
  ("f" customize-apropos-faces "faces")
  ("g" customize-apropos-groups "groups")
  ("o" customize-apropos-options "options"))

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("e" (progn
         (hot-expand "<s")
         (insert "emacs-lisp")
         (forward-line)))
  ("p" (progn
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("u" (progn
         (hot-expand "<s")
         (insert "plantuml :file CHANGE.png")
         (forward-line)))
  ("P" (progn
         (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

;; I bind it for myself like this:

;; (define-key org-mode-map "<"
;;   (lambda () (interactive)
;;      (if (looking-back "^")
;;          (hydra-org-template/body)
;;        (self-insert-command 1))))

(defun helm-persistent-delete-marked ()
  "Kill buffer without quitting helm."
  (interactive)
  (if (equal (cdr (assoc 'name (helm-get-current-source)))
             "Buffers")
      (with-helm-alive-p
        (helm-attrset 'kill-action
                      '(helm-persistent-kill-buffers . never-split))
        (helm-execute-persistent-action 'kill-action))
    (user-error "Only works for buffers")))

(defun helm-persistent-kill-buffers (_buffer)
  (unwind-protect
      (dolist (b (helm-marked-candidates))
        (helm-buffers-persistent-kill-1 b))
    (with-helm-buffer
      (setq helm-marked-candidates nil
            helm-visible-mark-overlays nil))
    (helm-force-update (helm-buffers--quote-truncated-buffer
                        (helm-get-selection)))))

(defhydra helm-like-unite (:hint nil
                           :color pink)
  "
Nav ^^^^^^^^^        Mark ^^          Other ^^       Quit
^^^^^^^^^^------------^^----------------^^----------------------
_K_ ^ ^ _k_ ^ ^     _m_ark           _v_iew         _i_: cancel
^↕^ _h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
_J_ ^ ^ _j_ ^ ^     _U_nmark all     _d_elete
^^^^^^^^^^                           _f_ollow: %(helm-attr 'follow)
"
  ;; arrows
  ("h" helm-beginning-of-buffer)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-end-of-buffer)
  ;; beginning/end
  ("g" helm-beginning-of-buffer)
  ("G" helm-end-of-buffer)
  ;; scroll
  ("K" helm-scroll-other-window-down)
  ("J" helm-scroll-other-window)
  ;; mark
  ("m" helm-toggle-visible-mark)
  ("t" helm-toggle-all-marks)
  ("U" helm-unmark-all)
  ;; exit
  ("<escape>" keyboard-escape-quit "" :exit t)
  ("o" keyboard-escape-quit :exit t)
  ("i" nil)
  ;; sources
  ("}" helm-next-source)
  ("{" helm-previous-source)
  ;; rest
  ("H" helm-help)
  ("v" helm-execute-persistent-action)
  ("d" helm-persistent-delete-marked)
  ("f" helm-follow-mode))

(define-key helm-map (kbd "<escape>") 'helm-like-unite/body)
(define-key helm-map (kbd "C-k") 'helm-like-unite/body)

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

(define-key helm-map (kbd "C-o") 'hydra-helm/body)

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

(defun ora-open-info (topic bname)
  "Open info on TOPIC in BNAME."
  (if (get-buffer bname)
      (progn
        (switch-to-buffer bname)
        (unless (string-match topic Info-current-file)
          (Info-goto-node (format "(%s)" topic))))
    (info topic bname)))

(defhydra hydra-info-to (:hint nil :color teal)
  "
_o_rg e_l_isp _e_macs _h_yperspec"
  ("o" (ora-open-info "org" "*org info*"))
  ("l" (ora-open-info "elisp" "*elisp info*"))
  ("e" (ora-open-info "emacs" "*emacs info*"))
  ("h" (ora-open-info "gcl" "*hyperspec*")))

(define-key Info-mode-map "t" 'hydra-info-to/body)

;; (push (expand-file-name "etc/info/" erc-user-emacs-directory)
;;       Info-directory-list)
;; (setq Info-additional-directory-list
;;       (list (expand-file-name "etc/info/" emacs-d)))

(defhydra hydra-info (:color blue
                      :hint nil)
      "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
      ("]"   Info-forward-node)
      ("["   Info-backward-node)
      ("n"   Info-next)
      ("p"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ("l"   Info-history-back)
      ("r"   Info-history-forward)
      ("H"   Info-history)
      ("t"   Info-top-node)
      ("<"   Info-top-node)
      (">"   Info-final-node)

      ("u"   Info-up)
      ("^"   Info-up)
      ("m"   Info-menu)
      ("g"   Info-goto-node)
      ("b"   beginning-of-buffer)
      ("e"   end-of-buffer)

      ("f"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("T"   Info-toc)
      ("d"   Info-directory)
      ("c"   Info-copy-current-node-name)
      ("C"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))
(define-key Info-mode-map (kbd "?") #'hydra-info/body)

(defhydra hydra-learn-sp (:hint nil)
  "
  _B_ backward-sexp            -----
  _F_ forward-sexp               _s_ splice-sexp
  _L_ backward-down-sexp         _df_ splice-sexp-killing-forward
  _H_ backward-up-sexp           _db_ splice-sexp-killing-backward
^^------                         _da_ splice-sexp-killing-around
  _k_ down-sexp                -----
  _j_ up-sexp                    _C-s_ select-next-thing-exchange
-^^-----                         _C-p_ select-previous-thing
  _n_ next-sexp                  _C-n_ select-next-thing
  _p_ previous-sexp            -----
  _a_ beginning-of-sexp          _C-f_ forward-symbol
  _z_ end-of-sexp                _C-b_ backward-symbol
--^^-                          -----
  _t_ transpose-sexp             _c_ convolute-sexp
-^^--                            _g_ absorb-sexp
  _x_ delete-char                _q_ emit-sexp
  _dw_ kill-word               -----
  _dd_ kill-sexp                 _,b_ extract-before-sexp
-^^--                            _,a_ extract-after-sexp
  _S_ unwrap-sexp              -----
-^^--                            _AP_ add-to-previous-sexp
  _C-h_ forward-slurp-sexp       _AN_ add-to-next-sexp
  _C-l_ forward-barf-sexp      -----
  _C-S-h_ backward-slurp-sexp    _ join-sexp
  _C-S-l_ backward-barf-sexp     _|_ split-sexp
"
  ;; TODO: Use () and [] - + * | <space>
  ("B" sp-backward-sexp );; similiar to VIM b
  ("F" sp-forward-sexp );; similar to VIM f
  ;;
  ("L" sp-backward-down-sexp )
  ("H" sp-backward-up-sexp )
  ;;
  ("k" sp-down-sexp ) ; root - towards the root
  ("j" sp-up-sexp )
  ;;
  ("n" sp-next-sexp )
  ("p" sp-previous-sexp )
  ;; a..z
  ("a" sp-beginning-of-sexp )
  ("z" sp-end-of-sexp )
  ;;
  ("t" sp-transpose-sexp )
  ;;
  ("x" sp-delete-char )
  ("dw" sp-kill-word )
  ;;("ds" sp-kill-symbol ) ;; Prefer kill-sexp
  ("dd" sp-kill-sexp )
  ;;("yy" sp-copy-sexp ) ;; Don't like it. Pref visual selection
  ;;
  ("S" sp-unwrap-sexp ) ;; Strip!
  ;;("wh" sp-backward-unwrap-sexp ) ;; Too similar to above
  ;;
  ("C-h" sp-forward-slurp-sexp )
  ("C-l" sp-forward-barf-sexp )
  ("C-S-h" sp-backward-slurp-sexp )
  ("C-S-l" sp-backward-barf-sexp )
  ;;
  ;;("C-[" (bind (sp-wrap-with-pair "[")) ) ;;FIXME
  ;;("C-(" (bind (sp-wrap-with-pair "(")) )
  ;;
  ("s" sp-splice-sexp )
  ("df" sp-splice-sexp-killing-forward )
  ("db" sp-splice-sexp-killing-backward )
  ("da" sp-splice-sexp-killing-around )
  ;;
  ("C-s" sp-select-next-thing-exchange )
  ("C-p" sp-select-previous-thing )
  ("C-n" sp-select-next-thing )
  ;;
  ("C-f" sp-forward-symbol )
  ("C-b" sp-backward-symbol )
  ;;
  ;;("C-t" sp-prefix-tag-object)
  ;;("H-p" sp-prefix-pair-object)
  ("c" sp-convolute-sexp )
  ("g" sp-absorb-sexp )
  ("q" sp-emit-sexp )
  ;;
  (",b" sp-extract-before-sexp )
  (",a" sp-extract-after-sexp )
  ;;
  ("AP" sp-add-to-previous-sexp );; Difference to slurp?
  ("AN" sp-add-to-next-sexp )
  ;;
  ("_" sp-join-sexp ) ;;Good
  ("|" sp-split-sexp ))

(use-package yasnippet
  :defer 1
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)

  (defhydra hydra-yasnippet (:color blue :hint nil)
    "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
    ("d" yas-load-directory)
    ("e" yas-activate-extra-mode)
    ("i" yas-insert-snippet)
    ("f" yas-visit-snippet-file :color blue)
    ("n" yas-new-snippet)
    ("t" yas-tryout-snippet)
    ("l" yas-describe-tables)
    ("g" yas-global-mode)
    ("m" yas-minor-mode)
    ("a" yas-reload-all))
  )

(use-package yasnippet-snippets
 :ensure t
 :after yasnippet
 :config
 (yasnippet-snippets-initialize))

(use-package helm-c-yasnippet
 :ensure t
 :after yasnippet-snippets
 :bind (("C-c C-y" . helm-yas-complete))
 :config
 (setq helm-yas-display-key-on-candidate t))

(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; Keeps focus on *Occur* window, even when when target is visited via RETURN key.
;; See hydra-occur-dwim for more options.
(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))

;; Focus on *Occur* window right away.
(add-hook 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
  (if (get-buffer "*Occur*")
    (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body) ))

;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
;; focus on the *Occur* window and hides upon request in case needed later.
(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "Start occur-dwim" :color red)
  ("j" occur-next "Next" :color red)
  ("k" occur-prev "Prev":color red)
  ("h" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red))

;; (global-set-key (kbd "C-x o") 'hydra-occur-dwim/body)

(global-set-key (kbd "C-c m")
                (defhydra hydra-transpose (:color red)
                  "Transpose"
                  ("c" transpose-chars "characters")
                  ("w" transpose-words "words")
                  ("o" org-transpose-words "Org mode words")
                  ("l" transpose-lines "lines")
                  ("s" transpose-sentences "sentences")
                  ("e" org-transpose-elements "Org mode elements")
                  ("p" transpose-paragraphs "paragraphs")
                  ("t" org-table-transpose-table-at-point "Org mode table")
                  ("q" nil "cancel" :color blue)))

(defhydra hydra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))

;; (setq ispell-dictionary "svenska")
(setq ispell-dictionary "english")

(use-package langtool
  :init
  (setq langtool-language-tool-jar
        "/home/jw/LanguageTool-3.0/languagetool-commandline.jar")
  :bind (("\C-x4w" . langtool-check)
         ("\C-x4W" . langtool-check-done)
         ("\C-x4l" . langtool-switch-default-language)
         ("\C-x44" . langtool-show-message-at-point)
         ("\C-x4c" . langtool-correct-buffer))
  :config
  ;; (setq langtool-default-language "en-US")
  (setq langtool-mother-tongue "en")
  )

(use-package ediff
  :config
  (defmacro csetq (variable value)
    `(funcall (or (get ',variable 'custom-set)
                  'set-default)
              ',variable ,value))
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-diff-options "-w")
  )

(defun bms/pdf-no-filter ()
 "View pdf without colour filter."
 (interactive)
 (pdf-view-midnight-minor-mode -1)
 )

;; change midnite mode colours functions
(defun bms/pdf-midnite-original ()
 "Set pdf-view-midnight-colors to original colours."
 (interactive)
 (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
 (pdf-view-midnight-minor-mode)
 )

(defun bms/pdf-midnite-amber ()
 "Set pdf-view-midnight-colors to amber on dark slate blue."
 (interactive)
 (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
 (pdf-view-midnight-minor-mode)
 )

(defun bms/pdf-midnite-green ()
 "Set pdf-view-midnight-colors to green on black."
 (interactive)
 (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
 (pdf-view-midnight-minor-mode)
 )

(defun bms/pdf-midnite-zenburn ()
  "Set pdf-view-midnight-colors to zenburn."
  (interactive)
  (setq pdf-view-midnight-colors '("#DCDCCC" . "#383838" ))
  (pdf-view-midnight-minor-mode)
  )

(defun bms/pdf-midnite-colour-schemes ()
 "Midnight mode colour schemes bound to keys"
 (local-set-key (kbd "!") (quote bms/pdf-no-filter))
 (local-set-key (kbd "@") (quote bms/pdf-midnite-amber))
 (local-set-key (kbd "#") (quote bms/pdf-midnite-zenburn))
 (local-set-key (kbd "$") (quote bms/pdf-midnite-original))
 )

(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; The following sets up a variety of colour-filter modes (good for
  ;; night-time viewing, or anytime really that you don't want your
  ;; eyeballs blasted with blazing white light):
  ;; (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; set the amber profile as default (see below)
  ;; (setq pdf-view-midnight-colors '("#DCDCCC" . "#383838" )) ; set the zenburn profile as default (see below)
  ;; (add-hook 'pdf-view-mode-hook (lambda ()
  ;;                                 ;; (add-to-list 'pdf-annot-default-markup-annotation-properties '(color . "#ff0000"))
  ;;                                 (pdf-view-midnight-minor-mode))) ; automatically turns on midnight-mode for pdfs
  ;; (add-hook 'pdf-annot-minor-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'pdf-annot-default-markup-annotation-properties
  ;;                         '(color . "#ff0000")))) ; automatically turns on midnight-mode for pdfs
  ;; This automatically sets pdf-tools to display using the midnight mode amber filter.
  ;; You can return to the original/no-filter with "!" (i.e. S-1); set
  ;; amber filter with "@" (i.e. S-2); set green filter with "#"
  ;; (i.e. S-3); set the bluish original midnight mode colours with "$"
  ;; (i.e. S-4).
  ;; (add-hook 'pdf-view-mode-hook 'bms/pdf-midnite-colour-schemes)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; For scanned pdfs, 'pdf-view-auto-slice-minor-mode can be useful
  ;; to turn on. You might bind it to something like s a. It auto
  ;; trims the borders for each page of the pdf as it encounters them.
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  )

(defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y"  pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-dark-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red))

(setq-default pdf-view-display-size 'fit-page)
(define-key pdf-view-mode-map "\\" 'hydra-pdftools/body)
(define-key pdf-view-mode-map "<s-spc>" 'pdf-view-scroll-down-or-next-page)
(define-key pdf-view-mode-map "g"  'pdf-view-first-page)
(define-key pdf-view-mode-map "G"  'pdf-view-last-page)
(define-key pdf-view-mode-map "l"  'image-forward-hscroll)
(define-key pdf-view-mode-map "h"  'image-backward-hscroll)
(define-key pdf-view-mode-map "j"  'pdf-view-next-page)
(define-key pdf-view-mode-map "k"  'pdf-view-previous-page)
(define-key pdf-view-mode-map "e"  'pdf-view-goto-page)
(define-key pdf-view-mode-map "u"  'pdf-view-revert-buffer)
(define-key pdf-view-mode-map "al" 'pdf-annot-list-annotations)
(define-key pdf-view-mode-map "ad" 'pdf-annot-delete)
(define-key pdf-view-mode-map "aa" 'pdf-annot-attachment-dired)
(define-key pdf-view-mode-map "am" 'pdf-annot-add-markup-annotation)
(define-key pdf-view-mode-map "at" 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map "y"  'pdf-view-kill-ring-save)
(define-key pdf-view-mode-map "i"  'pdf-misc-display-metadata)
(define-key pdf-view-mode-map "s"  'pdf-occur)
(define-key pdf-view-mode-map "b"  'pdf-view-set-slice-from-bounding-box)
(define-key pdf-view-mode-map "r"  'pdf-view-reset-slice)

(use-package pdf-view-restore
  :straight t
  :after pdf-tools
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
)

(use-package pdfgrep
  :straight t
)

(use-package bang
  :straight t
  :bind ("M-!" . bang))

(use-package epg
  :straight t
  :config
  (defun epg--list-keys-1 (context name mode)
    (let ((args (append (if (epg-context-home-directory context)
                            (list "--homedir"
                                  (epg-context-home-directory context)))
                        '("--with-colons" "--no-greeting" "--batch"
                          "--with-fingerprint" "--with-fingerprint")
                        (unless (eq (epg-context-protocol context) 'CMS)
                          '("--fixed-list-mode"))))
          (list-keys-option (if (memq mode '(t secret))
                                "--list-secret-keys"
                              (if (memq mode '(nil public))
                                  "--list-keys"
                                "--list-sigs")))
          (coding-system-for-read 'binary)
          keys string field index)
      (if name
          (progn
            (unless (listp name)
              (setq name (list name)))
            (while name
              (setq args (append args (list list-keys-option (car name)))
                    name (cdr name))))
        (setq args (append args (list list-keys-option))))
      (with-temp-buffer
        (apply #'call-process
               (epg-context-program context)
               nil (list t nil) nil args)
        (goto-char (point-min))
        (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
          (setq keys (cons (make-vector 15 nil) keys)
                string (match-string 0)
                index 0
                field 0)
          (while (and (< field (length (car keys)))
                      (eq index
                          (string-match "\\([^:]+\\)?:" string index)))
            (setq index (match-end 0))
            (aset (car keys) field (match-string 1 string))
            (setq field (1+ field))))
        (nreverse keys))))
)

(use-package helm
  :straight t
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
    ;; (global-unset-key (kbd "C-x c"))

    (global-set-key (kbd "C-x b") 'helm-mini)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (setq helm-google-suggest-use-curl-p nil ; t does not work
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
          helm-candidate-number-limit 150

          ;; you can customize helm-do-grep to execute ack-grep
          helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line t

          ;; helm-candidate-number-limit 500 ; limit the number of displayed candidates
          helm-ff-file-name-history-use-recentf t
          helm-ff-auto-update-initial-value t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-mode-fuzzy-match t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          ;; helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          ;; helm-display-header-line nil
  )

  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  (global-set-key (kbd "C-h b") 'helm-descbinds)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  ;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c r") 'helm-recentf)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)

  (global-set-key (kbd "C-c h x") 'helm-register)
  ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

  (define-key 'help-command (kbd "C-f") 'helm-apropos)
  (define-key 'help-command (kbd "r") 'helm-info-emacs)
  (define-key 'help-command (kbd "C-l") 'helm-locate-library)

  ;; use helm to list eshell history
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

  ;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; show minibuffer history with Helm
  (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
  (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

  (define-key global-map [remap find-tag] 'helm-etags-select)

  (define-key global-map [remap list-buffers] 'helm-buffers-list)

  (helm-mode 1)

  (use-package helm-projectile
    :straight t
    :init
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)
    (setq projectile-indexing-method 'alien))))

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :straight t
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    ;;(add-hook 'c-mode-hook 'helm-gtags-mode)
    ;;(add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(use-package helm-swoop
  :straight t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)
         )
  :config
      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows nil)

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t)

      ;; Go to the opposite side of line from the end or beginning of line
      (setq helm-swoop-move-to-line-cycle t)

      ;; Optional face for line numbers
      ;; Face name is `helm-swoop-line-number-face`
      (setq helm-swoop-use-line-number-face t)
)

(use-package helm-themes
  :straight t
)

(use-package helm-describe-modes
  :straight t
  :config
  (global-set-key [remap describe-mode] #'helm-describe-modes)
)

(use-package helm-proc
  :straight t
)

(use-package helm-tramp
  :straight t
)

(use-package helm-pydoc
  :straight t
)

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

(use-package helm-recoll)

(setf epg-pinentry-mode 'loopback)
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd
              (concat (replace-regexp-in-string "%22" "\""
                                                (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(use-package exwm)
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
(use-package windower)
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

(defun jw/xmodmap ()
  "Execute xmodmap"
  (progn
    (remove-hook 'exwm-manage-finish-hook 'jw/xmodmap)
    (ambrevar/call-process-to-string "/usr/bin/xmodmap" "/home/jw/.Xmodmap.exwm")))

(setq browse-url-generic-program
      (or
       (executable-find (or (getenv "BROWSER") ""))
       (when (executable-find "xdg-mime")
         (let ((desktop-browser (ambrevar/call-process-to-string "xdg-mime" "query" "default" "text/html")))
           (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
       (executable-find browse-url-chrome-program)))

(defun my-exwm-config-setup ()
  "My modified configuration for EXWM. Based on exwm-config.el"
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
            (,(kbd "s-o") . windower-toggle-single) ;; Toggle between multiple windows, and a single window
            (,(kbd "s-O") . windower-toggle-split)  ;; Toggle between vertical and horizontal split. Only works with exactly two windows.
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

(use-package telephone-line)
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

(use-package helm-exwm
  :straight t
  :config
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))

(use-package helpful
  :straight t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command)
)

(use-package cmake-mode)
  ;; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; helm-perldoc:setup takes long time on low power platform
(eval-after-load "cperl-mode"
  '(progn
    (helm-perldoc:setup)))

;; auto carton setup
;; (add-hook 'cperl-mode-hook 'helm-perldoc:carton-setup)

(use-package ansi-color
 :straight nil)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(use-package imenu
  :bind (("C-§" . helm-imenu)
         ("M-§" . helm-imenu)
         )
  )

;; (use-package guide-key
;;   :config
;;   (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
;;   (setq guide-key/recursive-key-sequence-flag t)
;;   (setq guide-key/idle-delay 0.7)
;;   (guide-key-mode 1)  ; Enable guide-key-mode
;;   )

(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

(use-package paperless
  :straight t
  :config
  (custom-set-variables
   '(paperless-capture-directory "/home/jw/Dokument/Scan")
   '(paperless-root-directory "/home/jw/Dokument"))
)

(use-package eterm-256color
  :straight t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode)
)

(setq bibtex-completion-bibliography "~/org/roam/biblio/references.bib"
      bibtex-completion-library-path "~/org/roam/pdfs"
      bibtex-completion-notes-path "~/org/roam/biblio/helm-bibtex-notes")
(setq bibtex-completion-pdf-open-function 'org-open-file)

(setq reftex-default-bibliography '("~/org/roam/biblio/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/org/roam/bibli/notes.org"
      org-ref-default-bibliography '("~/org/roam/biblio/references.bib")
      org-ref-pdf-directory "~/org/roam/pdfs/")

(use-package ox
 :straight nil)

(use-package ox-hugo
  :straight t
  :after ox)

(use-package org-ref)

(use-package org-ref-ox-hugo
  :after (org org-ref ox-hugo)
  :straight (:host github
             :repo "jethrokuan/org-ref-ox-hugo"
             :files ("*.el"))
  :config
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))

(use-package org-noter
  :straight t
  :config
  (setq org-noter-always-create-frame nil
        org-noter-notes-search-path '("~/org/roam/org-noter")))
(use-package org-roam-bibtex
  :straight t
  :hook (org-roam-mode . org-roam-bibtex-mode))
(use-package org-roam
  :straight t
  :after (org ox-hugo)
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#A08000"))))
  :init
  (setq org-roam-directory "~/org/roam/"
        org-roam-db-location "~/org/roam/org-roam.db"
        org-roam-graph-exclude-matcher "private")
  (setq org-roam-directory "~/org/roam/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n c" . org-roam-capture)
               ("C-c n g" . org-roam-show-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert)))
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:~/org/roam/hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:~/org/roam/hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}

- source :: ${ref}"
           :unnarrowed t)))
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n"
                             (file-relative-name (car it) org-roam-directory)
                             (org-roam--get-title-or-slug (car it))))
         "" (org-roam-db-query [:select [from]
                                   :from links
                                   :where (= to $s1)
                                   :and from :not :like $s2] file "%private%"))
      ""))
  (defun my/org-export-preprocessor (_backend)
    (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n" links))))))
  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)
  (defun jethro/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'jethro/conditional-hugo-enable)
  (setq org-roam-completion-system 'helm))

(use-package org-roam-server
  :straight t
)

(use-package el-patch
  :straight t
)
(eval-when-compile
  (require 'el-patch))
(use-package deft
  :after org
  :straight t
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam/")
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))

(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

(use-package perspective
  :straight t
  :config
  (persp-mode))

(use-package emms)
;; (use-package emms-setup
;;  :straight nil
;;  :config
;;  (emms-standard)
;;  (emms-default-players)
;;  (setq emms-source-file-default-directory "/home/jw/realpolish/100-DPS--61-70/mp3/")
;; )

(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-source-file-default-directory "/home/jw/realpolish/100-DPS--61-70/mp3/")

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
(use-package elfeed
  :straight t
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
(use-package elfeed-org
  :straight t
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

(setq
 ;; newsticker-heading-format "%t"
 ;; newsticker-item-format "%t"
 ;; newsticker-desc-format "%d\n%c"
 ;; newsticker-hide-old-items-in-newsticker-buffer t
 newsticker-html-renderer 'shr-render-region
 ;; newsticker-frontend 'newsticker-plainview
 ;; newsticker-use-full-width nil
 newsticker-treeview-treewindow-width 40
 newsticker-treeview-listwindow-height 20
 newsticker-url-list-defaults nil  ;remove default list (i.e. emacswiki)
 newsticker-retrieval-interval 0   ;don't fetch when I'm not reading RSS
 newsticker-automatically-mark-items-as-old nil
 newsticker-url-list '(
                       ("emacs-fu" "http://emacs-fu.blogspot.com/feeds/posts/default" nil nil nil)
                       ("abandonia" "http://www.abandonia.com/en/rss.xml" nil nil nil)
                       ("arch linux" "https://www.archlinux.org/feeds/news/" nil nil nil)
                       ("Planet Emacsen" "http://planet.emacsen.org/atom.xml" nil nil nil)
                       ("slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil)
                       ("Kmandla" "http://kmandla.wordpress.com/feed/" nil nil nil)
                       ("mojäng" "http://www.mojang.com/feed" nil nil nil)
                       ("SMBC" "http://www.smbc-comics.com/rss.php" nil nil nil)
                       ("xkcd" "https://www.xkcd.com/rss.xml" nil nil nil)
                       ;;("imdb" "http://rss.imdb.com/daily/poll" nil nil nil)
                       ;;("rotten" "https://www.rottentomatoes.com/syndication/rss/top_news.xml" nil nil nil)
                       ;;("BBC World" "http://feeds.bbci.co.uk/news/world/rss.xml" nil nil nil)
                       ("BBC Sci" "http://feeds.bbci.co.uk/news/science_and_environment/rss.xml" nil nil nil)
                       ;;("Coursera" "http://blog.coursera.org/rss" nil nil nil)
                       ;;("stallman" "http://www.stallman.org/rss/rss.xml" nil nil nil)
                       ("emacs rocks" "http://emacsrocks.com/atom.xml" nil nil nil)
                       ("endlessparentheses" "http://endlessparentheses.com/atom.xml" nil nil nil)
                       ("affärsvärlden" "http://www.affarsvarlden.se/?service=rss" nil nil nil)
                       ("börspodden" "http://borspodden.se/feed/" nil nil nil)
                       ("dividend mantra" "http://feeds.feedburner.com/DividendMantra/" nil nil nil)
                       ("avpixlat" "http://avpixlat.info/feed/" nil nil nil)
                       ("recepten" "http://www.recepten.se/feed/blog_rss2.xhtml" nil nil nil)
                       ;;("Hacker News" "http://news.ycombinator.com/rss" nil nil nil)
                       ("screen junkies" "https://www.youtube.com/feeds/videos.xml?user=screenjunkies" nil nil nil)
                       ("Matte Northice" "https://www.youtube.com/feeds/videos.xml?user=gurskit8" nil nil nil)
                       ("Simon Smith" "https://www.youtube.com/feeds/videos.xml?user=simonthedragon" nil nil nil)
                       ("failarmy" "https://www.youtube.com/feeds/videos.xml?user=failarmy" nil nil nil)
                       ("thuleanperspective" "https://www.youtube.com/feeds/videos.xml?user=ThuleanPerspective" nil nil nil)
                       ("danwood" "https://www.youtube.com/feeds/videos.xml?user=techguruuk" nil nil nil)
                       ("LGR" "https://www.youtube.com/feeds/videos.xml?user=phreakindee" nil nil nil)
                       ))

(use-package epkg
  :straight t
)

(use-package indium)
(add-hook 'js-mode-hook #'indium-interaction-mode)

;; (use-package borg)

;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (setq wg-prefix-key (kbd "C-c C-w"))
;;   (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;;   (workgroups-mode 1)
;;   )

(use-package nov
  :straight t
  :bind
  (:map nov-mode-map
        ("<home>" . move-beginning-of-line)
        ("<end>" . move-end-of-line)
        ))
(push '("\\.epub\\'" . nov-mode) auto-mode-alist)

(require 'url)

(use-package goto-addr
  :straight t
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package vterm
  :straight t
)

(use-package calibredb
  :straight t
  :config
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  (setq calibredb-root-dir (expand-file-name "~/calibre_library"))
  (setq calibredb-db-dir (concat calibredb-root-dir "/metadata.db"))
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-library-alist '(("~/calibre_library")))
)

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(custom-set-variables
 '(conda-anaconda-home "/home/jw/miniconda3/"))
(use-package conda)

(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode t)
(setq global-mode-string (append global-mode-string '(:exec conda-env-current-name)))

(use-package pyenv-mode)
(pyenv-mode)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(use-package esh-mode
  :straight nil
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")))
  (add-hook 'eshell-mode-hook (lambda ()
    ;; The 'ls' executable requires the Gnu version on the Mac
    (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                  "/usr/local/bin/gls"
                "/bin/ls")))
      (eshell/alias "ll" (concat ls " -AlohG --color=always")))))
  :config
  ;; used by other functions below
  (declare-function ffap-file-at-point "ffap.el")

  (defun prot/eshell-insert-file-at-point ()
    "Insert (cat) contents of file at point."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
          (progn
            (end-of-buffer)
            (insert (concat "cat " file))
            (eshell-send-input))
        (user-error "No file at point"))))

  (defun prot/eshell-kill-save-file-at-point ()
    "Add to kill-ring the absolute path of file at point."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
          (kill-new (concat (eshell/pwd) "/" file))
        (user-error "No file at point"))))

  (defun prot/eshell-find-file-at-point ()
    "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
          (find-file file)
        (user-error "No file at point"))))

  (defun prot/eshell-file-parent-dir ()
    "Open `dired' with the parent directory of file at point."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
          (dired (file-name-directory file))
        (user-error "No parent dir for file to jump to"))))

  (defun prot/eshell-mkcd (dir)         ; TODO define alias
    "Make a directory, or path, and switch to it."
    (interactive)
    (eshell/mkdir "-p" dir)
    (eshell/cd dir))

  (defun prot/eshell-put-last-output-to-buffer ()
    "Produce a buffer with output of last `eshell' command."
    (interactive)
    (let ((eshell-output (kill-region (eshell-beginning-of-output)
                                      (eshell-end-of-output))))
      (with-current-buffer (get-buffer-create  "*last-eshell-output*")
        (erase-buffer)
        (yank)           ; TODO do it with `insert' and `delete-region'?
        (switch-to-buffer-other-window (current-buffer)))))

  (defun prot/eshell-complete-redirect-to-buffer ()
    "Complete the syntax for appending to a buffer via `eshell'."
    (interactive)
    (insert
     (concat " >>> #<"
             (format "%s"
                     (read-buffer-to-switch "Switch to buffer: "))
             ">")))

  (defun prot/eshell-narrow-output-highlight-regexp ()
    (interactive)
    (let ((regexp (read-regexp "Regexp to highlight")))
      (narrow-to-region (eshell-beginning-of-output)
                        (eshell-end-of-output))
      (goto-char (point-min))
      (highlight-regexp regexp 'hi-yellow)))

  (defun pcomplete/eshell-mode/bmk ()
    "Completion for `bmk'"
    (pcomplete-here (bookmark-all-names)))

  (defun eshell/bmk (&rest args)
    "Integration between EShell and bookmarks.
For usage, execute without arguments."
    (setq args (eshell-flatten-list args))
    (let ((bookmark (car args))
          filename name)
      (cond
       ((eq nil args)
        (format "Usage:
- bmk BOOKMARK to
-- either change directory pointed to by BOOKMARK
-- or bookmark-jump to the BOOKMARK if it is not a directory.
- bmk . BOOKMARK to bookmark current directory in BOOKMARK.
Completion is available."))
       ((string= "." bookmark)
        ;; Store current path in EShell as a bookmark
        (if (setq name (car (cdr args)))
            (progn
              (bookmark-set name)
              (bookmark-set-filename name (eshell/pwd))
              (format "Saved current directory in bookmark %s" name))
          (error "You must enter a bookmark name")))
       (t
        ;; Check whether an existing bookmark has been specified
        (if (setq filename (bookmark-get-filename bookmark))
            ;; If it points to a directory, change to it.
            (if (file-directory-p filename)
                (eshell/cd filename)
              ;; otherwise, just jump to the bookmark
              (bookmark-jump bookmark))
          (error "%s is not a bookmark" bookmark))))))

(defun my-delete-window-on-eshell-exit ()
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'my-delete-window-on-eshell-exit)

:bind
  (:map eshell-mode-map
              ("M-k" . eshell-kill-input)
              ("C-c M-w" . prot/eshell-kill-save-file-at-point)
              ("C-c ä" . prot/eshell-insert-file-at-point)
              ("C-c f" . prot/eshell-find-file-at-point)
              ("C-c C-f" . prot/eshell-find-file-at-point)
              ("C-c ö" . prot/eshell-put-last-output-to-buffer)
              ("C-c C->" . prot/eshell-complete-redirect-to-buffer)
              ("C-c C-j" . prot/eshell-file-parent-dir)
              ("C-c h" . prot/eshell-narrow-output-highlight-regexp))
)

(use-package esh-module
  :straight nil
  :config
  (setq eshell-modules-list             ; Needs review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix)))

(use-package em-dirs
  :straight nil
  :after esh-mode
  :config
  (setq eshell-cd-on-directory t))

(use-package em-tramp
  :straight nil
  :after esh-mode
  :config
  (setq password-cache t)
  (setq password-cache-expiry 600))

(use-package em-hist
  :straight nil
  :after esh-mode
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)

  (defun prot/eshell-complete-history ()
    "Insert element from `eshell' history using completion."
    (interactive)
    (let ((hist (ring-elements eshell-history-ring)))
      (insert
       (completing-read "Input history: " hist nil t))))

  (defun prot/eshell-complete-recent-dir (&optional arg)
    "Switch to a recent `eshell' directory using completion.
With \\[universal-argument] also open the directory in a `dired'
buffer."
    (interactive "P")
    (let* ((dirs (ring-elements eshell-last-dir-ring))
           (dir (completing-read "Switch to recent dir: " dirs nil t)))
      (insert dir)                      ; Not good enough
      (eshell-send-input)               ; Should cd directly…
      (when arg
        (dired dir))))

  ;; `cl-remove-if' is used right below
  (declare-function cl-remove-if "cl-seq")

  (defun prot/eshell-find-subdirectory-recursive ()
    "Recursive `eshell/cd' to subdirectory.
This command has the potential for infinite recursion: use it
wisely or prepare to use `eshell-interrupt-process'."
    (interactive)
    (let* ((dir (eshell/pwd))
           (contents (directory-files-recursively dir ".*" t nil nil))
           ;; (contents (directory-files dir t))
           (find-directories (mapcar (lambda (x)
                                       (when (file-directory-p x)
                                         (abbreviate-file-name x)))
                                     contents))
           (subdirs (delete nil find-directories))
           (cands (cl-remove-if (lambda (x) (string-match-p "\\.git" x)) subdirs))
           (selection (completing-read "Find sub-directory: " cands nil t)))
      (insert selection)
      (eshell-send-input)))

  :bind (:map eshell-mode-map ;;  eshell-hist-mode-map
              ("M-s" . nil) ; I use this for lots of more useful commands
              ("M-r" . prot/eshell-complete-history) ; use this to find input history
              ("C-c d" . prot/eshell-find-subdirectory-recursive)
              ("C-c =" . prot/eshell-complete-recent-dir)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(use-package google-translate
  :straight t
  :config
  (setq google-translate-default-source-language "pl")
  (setq google-translate-default-target-language "en")
)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
(setq org-format-latex-options (plist-put org-format-latex-options :foreground 'default))
(setq org-format-latex-options (plist-put org-format-latex-options :background 'default))

(setq tramp-terminal-type "tramp")

(use-package rg
  :straight t
)

(use-package org-recoll
  :straight (:host github
             :repo "alraban/org-recoll")
)
