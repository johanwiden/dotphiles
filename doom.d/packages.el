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
(unpin! straight)
(package! ace-link)
(package! arxiv-mode)
(package! bibtex-completion)
(package! bookmark+
  :recipe (:host github :repo "emacsmirror/bookmark-plus"))
(package! calibredb)
(package! consult-recoll)
(package! counsel)
(package! dired-filter)
(package! dired-launch
  :recipe (:host github :repo "thomp/dired-launch"))
(package! dired-narrow)
(package! dired+
  :recipe (:host github :repo "emacsmirror/dired-plus"))
(package! dwim-shell-command)
(package! elfeed)
(package! elfeed-org)
(package! engine-mode)
(package! epkg)
;; (package! exwm)
(package! exwm
  :recipe (:host github :repo "ch11ng/exwm"))
(package! xelb
  :recipe (:host github :repo "ch11ng/xelb"))
(package! facemenu+
  :recipe (:host github :repo "emacsmirror/facemenu-plus"))
(package! fish-completion)
(package! fontaine)
(package! git-link)
(package! good-scroll
  :recipe (:host github :repo "io12/good-scroll.el"))
(package! helm-bibtex)
(package! helm-browser :recipe (:local-repo "/home/jw/projects/emacs/burnthekernel/helm-browser"))
(package! helm-exwm)
(package! helm-ls-git)
(package! helm-org-ql)
(package! helm-org-rifle)
(package! helm-pydoc)
(package! helm-sly)
(package! helm-tramp)
(package! helm-w3m)
(package! helm-wikipedia)
(package! hexrgb
  :recipe (:host github :repo "emacsmirror/hexrgb"))
(package! hide-comnt
  :recipe (:host github :repo "emacsmirror/hide-comnt"))
(package! highlight
  :recipe (:host github :repo "emacsmirror/highlight"))
(package! hledger-mode)
(package! hungry-delete)
(package! hyperbole
  :recipe (:host github :repo "rswgnu/hyperbole"))
(package! lexic)
;; (package! lexic :recipe (:local-repo "lexic"))
;; (package! lexic
;;   :recipe (:host github :repo "tecosaur/lexic"))
(package! mixed-pitch)
(package! modus-themes
  :recipe (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(package! mouse3
  :recipe (:host github :repo "emacsmirror/mouse3"))
(package! nov)
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
(package! pdftotext
  :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! persid
  :recipe (:host github :repo "rougier/persid"))
(package! session)
;; Announcement on doom discord 221125 by gagbo, for upgrade problems
;; (package! straight :pin "3eca39d")
(package! telephone-line)
(package! thingatpt+
  :recipe (:host github :repo "emacsmirror/thingatpt-plus"))
(package! thing-cmds
  :recipe (:host github :repo "emacsmirror/thing-cmds"))
(package! unfill)
;; (package! visual-regexp)
;; (package! visual-regexp-steroids)
(package! w3m)
(package! websocket)
(package! windower)
(package! xah-math-input
  :recipe (:host github :repo "DiamondBond/xah-math-input"))
(package! xah-wolfram-mode
  :recipe (:host github :repo "xahlee/xah-wolfram-mode"))
(package! zoxide)
