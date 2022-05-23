;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
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
;; our package manager can't deal with; see raxod502/straight.el#279)
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
(package! ag)
(package! arxiv-mode)
(package! bibtex-completion)
(package! bookmark+
  :recipe (:host github :repo "emacsmirror/bookmark-plus"))
(package! calibredb)
;; (package! calibredb
;;   :recipe (:local-repo "calibredb"))
(package! deadgrep)
(package! dired-filter)
(package! dired-launch
  :recipe (:host github :repo "thomp/dired-launch"))
(package! dired-narrow)
(package! dired-ranger)
(package! dired+
  :recipe (:host github :repo "emacsmirror/dired-plus"))
;; (package! eis
;;   :recipe (:local-repo "eis"))
(package! eis
  :recipe (:host github :repo "bkaestner/eis.el"))
(package! elfeed)
(package! elfeed-org)
(package! embark)
(package! engine-mode)
(package! epkg)
(package! exwm)
(package! facemenu+
  :recipe (:host github :repo "emacsmirror/facemenu-plus"))
(package! fish-completion)
;; (package! flycheck-hledger)
(package! fontaine)
(package! gitconfig-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitconfig-mode.el")))
(package! gitignore-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitignore-mode.el")))
(package! good-scroll
  :recipe (:host github :repo "io12/good-scroll.el"))
(package! helm-bibtex)
(package! helm-exwm)
(package! helm-org-ql)
(package! helm-org-rifle)
(package! helm-proc)
(package! helm-projectile)
(package! helm-pydoc)
(package! helm-tramp)
(package! helm-w3m)
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
(package! marginalia)
(package! mixed-pitch)
(package! modus-themes
  :recipe (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(package! mouse3
  :recipe (:host github :repo "emacsmirror/mouse3"))
(package! nov)
(package! org-clock-convenience)
(package! org-menu
  :recipe (:host github :repo "sheijk/org-menu"))
(package! org-ql)
(package! org-recoll
  :recipe (:host github :repo "alraban/org-recoll"))
(package! org-ref-cite
  :recipe (:host github :repo "jkitchin/org-ref-cite"))
(package! org-ref-cite-core
  :recipe (:local-repo "org-ref-cite"))
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-similarity
  :recipe (:host github :repo "soldeace/org-similarity" :branch "main"))
(package! palette
  :recipe (:host github :repo "emacsmirror/palette"))
(package! pdftotext
  :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! session)
(package! telephone-line)
(package! thingatpt+
  :recipe (:host github :repo "emacsmirror/thingatpt-plus"))
(package! thing-cmds
  :recipe (:host github :repo "emacsmirror/thing-cmds"))
(package! unfill)
(package! visual-regexp)
(package! visual-regexp-steroids)
(package! w3m)
(package! windower)
(package! zoxide)
