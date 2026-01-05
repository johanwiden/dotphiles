;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! ace-link)
(package! activities)
(package! alert)
(package! arxiv-mode)

(package! biblio)
;; (package! bookmark+
;;   :recipe (:host github :repo "emacsmirror/bookmark-plus"))
;; (package! bufferlo)                     ;
;; (package! bufferlo
;;   :recipe (:host github :repo "florommel/bufferlo" :branch "fix-tab-group-restore"))
(package! bufferlo
  :recipe (:host github :repo "florommel/bufferlo"))

(package! calibredb)
(package! casual-suite)
;; (package! casual-eww
;;   :recipe (:host github :repo "dandrake/casual-eww"))
(package! cc-cedict
  :recipe (:host github :repo "xuchunyang/cc-cedict.el"))
(package! citar-denote)
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
(package! consult-recoll
  :recipe (:host github :repo "emacs-straight/consult-recoll"))

(package! denote)
(package! denote-explore)
(package! denote-org)
(package! denote-menu)
(package! denote-sequence)
;; (package! dired+
;;   :recipe (:host github :repo "emacsmirror/dired-plus"))

(package! eldoc-box)
(package! elfeed)
(package! elfeed-org)
(package! epkg)
(package! evil-matchit)

;; (package! facemenu+
;;   :recipe (:host github :repo "emacsmirror/facemenu-plus"))
(package! focus)
(package! fontaine)

(package! ghub)
(package! gt)
;; (package! gt
;;   :recipe (:host github :repo "lorniu/gt"))

;; (package! hexrgb
;;   :recipe (:host github :repo "emacsmirror/hexrgb"))
;; (package! hide-comnt
;;   :recipe (:host github :repo "emacsmirror/hide-comnt"))
;; (package! highlight
;;   :recipe (:host github :repo "emacsmirror/highlight"))
(package! hledger-mode)
(package! hungry-delete)
(package! hydra)
(package! hyperbole
  :recipe (:host github :repo "rswgnu/hyperbole"))

(package! igist)
(package! immersive-translate
  :recipe (:host github :repo "Elilif/emacs-immersive-translate"))

(package! jieba
  :recipe (:host github :repo "kisaragi-hiu/emacs-jieba"))
(package! jinx)

(package! llama
  :recipe (:host github :repo "tarsius/llama"))
(package! lexic)

(package! major-mode-hydra)
(package! math-preview)
(package! maxima)
(package! mistty)
(package! mixed-pitch)
(package! modus-themes
  :recipe (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(package! mouse3
  :recipe (:host github :repo "emacsmirror/mouse3"))

(package! nov)

(package! openwith)
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
(package! org-web-tools)

;; (package! palette
;;   :recipe (:host github :repo "emacsmirror/palette"))
(package! pangu-spacing)
(package! paw
  :recipe (:host github :repo "chenyanming/paw" :files ("*")))
(package! pcre2el)
(package! pdftotext
  :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! persid
  :recipe (:host github :repo "rougier/persid"))
(package! pinyin-convert
  :recipe (:host github :repo "tprost/pinyin-convert.el"))
(package! posframe)
(package! pulsar)

(package! rainbow-delimiters)
(package! reader
  :recipe (:type git :host codeberg :repo "divyaranjan/emacs-reader"
           :files ("*.el" "render-core.so")
           :pre-build ("make" "clean" "all")))
(package! request)
(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))

(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv" :branch "master"))
(package! shr-tag-pre-highlight)
(package! shrface
  :recipe (:host github :repo "chenyanming/shrface"))
(package! svg-lib)

(package! tab-bar-groups)
;; (package! tabspaces)
;; (package! thingatpt+
;;   :recipe (:host github :repo "emacsmirror/thingatpt-plus"))
;; (package! thing-cmds
;;   :recipe (:host github :repo "emacsmirror/thing-cmds"))
(package! tldr)
(package! transient-showcase
  :recipe (:host github :repo "positron-solutions/transient-showcase" :branch "master"))
(package! typst-ts-mode
  :recipe (:host codeberg :repo "meow_king/typst-ts-mode"))

(package! unfill)

(package! visual-replace)

(package! w3m)
(package! wallabag
  :recipe (:host github :repo "chenyanming/wallabag.el" :files ("*.el" "*.alist" "*.css")))
(package! websocket)
(package! windower)
(package! writegood-mode)

(package! xah-math-input
  :recipe (:host github :repo "DiamondBond/xah-math-input"))

(package! zh-utils
  :recipe (:host codeberg :repo "jiewawa/zh-utils"))
