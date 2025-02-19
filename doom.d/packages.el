;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
(package! ace-link)
(package! activities)
(package! arxiv-mode)
(package! bookmark+
  :recipe (:host github :repo "emacsmirror/bookmark-plus"))
(package! calibredb)
(package! casual-suite)
;; (package! casual-calc)
;; (package! casual-avy)
;; (package! casual-dired)
;; (package! casual-info)
;; (package! casual-isearch)
;; (package! combobulate
;;   :recipe (:host github :repo "mickeynp/combobulate"))
(package! consult-gh
  :recipe (:host github :repo "armindarvish/consult-gh"))
(package! consult-gnome-search
  :recipe (:host github :repo "juergenhoetzel/consult-gnome-search"))
(package! consult-mu
  :recipe (:host github :repo "armindarvish/consult-mu" :files (:defaults "extrass/*.el")))
(package! consult-denote)
(package! consult-notes)
(package! consult-web
  :recipe (:host github :repo "armindarvish/consult-web" :files (:defaults "sources/*.el")))
(package! consult-omni
  :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
;; (package! consult-recoll)
(package! consult-recoll
  :recipe (:host github :repo "emacs-straight/consult-recoll"))
(package! denote)
(package! denote-menu)
;; (package! dired-filter)
;; (package! dired-hist)
;; (package! dired-launch)
;; (package! dired-narrow)
(package! dired+
  :recipe (:host github :repo "emacsmirror/dired-plus"))
;; (package! dropbox
;;   :recipe (:host github :repo "lorniu/emacs-dropbox"))
;; (package! dwim-shell-command)
;; (package! eat
;;   :recipe (:host codeberg :repo "akib/emacs-eat"
;;            :files ("*.el" ("term" "term/*.el") "*.texi"
;;                    "*.ti" ("terminfo/e" "terminfo/e/*")
;;                    ("terminfo/65" "terminfo/65/*")
;;                    ("integration" "integration/*")
;;                    (:exclude ".dir-locals.el" "*-tests.el"))))
(package! alert)
(package! eldoc-box)
(package! elfeed)
(package! elfeed-org)
;; (package! ement)
;; (package! engine-mode)
(package! epkg)
(package! evil-matchit)
(package! facemenu+
  :recipe (:host github :repo "emacsmirror/facemenu-plus"))
;; (package! fish-completion)
(package! focus)
(package! fontaine)
(package! ghub)
(package! go-translate)
(package! gptel)
(package! hexrgb
  :recipe (:host github :repo "emacsmirror/hexrgb"))
(package! hide-comnt
  :recipe (:host github :repo "emacsmirror/hide-comnt"))
(package! highlight
  :recipe (:host github :repo "emacsmirror/highlight"))
(package! hledger-mode)
;; (package! html-ts-mode
;;   :recipe (:host github :repo "mickeynp/html-ts-mode"))
(package! hungry-delete)
(package! hydra)
(package! hyperbole
  :recipe (:host github :repo "rswgnu/hyperbole"))
(package! igist)
;; (package! immersive-translate)
(package! immersive-translate
  :recipe (:host github :repo "Elilif/emacs-immersive-translate"))
(package! jieba
  :recipe (:host github :repo "kisaragi-hiu/emacs-jieba"))
(package! jinx)
(package! llama
  :recipe (:host github :repo "tarsius/llama"))
(package! lexic)
;; (package! load-theme-buffer-local)
(package! major-mode-hydra)
;; (package! mastodon)
(package! maxima)
(package! mistty)
(package! mixed-pitch)
(package! modus-themes
  :recipe (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(package! mouse3
  :recipe (:host github :repo "emacsmirror/mouse3"))
(package! nov)
(package! org-books
  :recipe (:host github :repo "goderich/org-books"))
(package! org-download)
(package! org-menu
  :recipe (:host github :repo "sheijk/org-menu"))
(package! org-protocol-capture-html
  :recipe (:host github :repo "alphapapa/org-protocol-capture-html"))
(package! org-ql)
(package! org-recoll
  :recipe (:host github :repo "alraban/org-recoll"))
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-similarity
  :recipe (:host github :repo "soldeace/org-similarity" :branch "main"))
(package! org-transclusion)
(package! palette
  :recipe (:host github :repo "emacsmirror/palette"))
(package! pangu-spacing)
(package! paw
  :recipe (:host github :repo "chenyanming/paw" :files ("*")))
;; (package! paw
;;   :recipe (:local-repo "paw"))
(package! pcre2el)
(package! pdftotext
  :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! persid
  :recipe (:host github :repo "rougier/persid"))
(package! posframe)
(package! request)
(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv" :branch "master"))
(package! shr-tag-pre-highlight)
(package! shrface
  :recipe (:host github :repo "chenyanming/shrface"))
(package! svg-lib)
(package! thingatpt+
  :recipe (:host github :repo "emacsmirror/thingatpt-plus"))
(package! thing-cmds
  :recipe (:host github :repo "emacsmirror/thing-cmds"))
(package! tldr)
(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"))
(package! unfill)
(package! visual-replace)
(package! w3m)
(package! wallabag
  :recipe (:host github :repo "chenyanming/wallabag.el" :files ("*.el" "*.alist" "*.css")))
(package! websocket)
(package! windower)
(package! xah-math-input
  :recipe (:host github :repo "DiamondBond/xah-math-input"))
(package! xah-wolfram-mode
  :recipe (:host github :repo "xahlee/xah-wolfram-mode"))
