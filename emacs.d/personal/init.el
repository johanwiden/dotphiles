;;; My personal emacs init file

;;; Commentary:

;;; Code:

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/personal/custom/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/personal/custom/config.org")))
