;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating hledger entries.
;;
;; Based on ob-hledger.el by Simon Michael.
;; Based on ob-ledger.el.
;; If the source block is empty, hledger will use the default journal file.
;; Include statements are processed to make filenames absolute, so that included
;; path names are effectively local to the containing org file, not the temp directory.

;; TODO: report errors in hledger execution.

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("hledger" . "journal"))

(defvar org-babel-default-header-args:hledger
  '((:results . "output") (:cmdline . "bal"))
  "Default arguments to use when evaluating a hledger source block.")

(defun org-babel-execute:hledger (body params)
  "Execute BODY, a block of hledger entries, with org-babel,qualified by PARAMS.
This function is called by `org-babel-execute-src-block'."
  (message "executing hledger source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
        (cmdline (cdr (assoc :cmdline params)))
        (in-file (org-babel-temp-file "hledger-"))
        (out-file (org-babel-temp-file "hledger-output-")))
    (with-temp-file in-file (insert (concat
                                     (replace-regexp-in-string ;make include filename absolute
                                      "include \\(.+\\)"
                                      #'(lambda (m) (concat "include " (expand-file-name (substring m 8))))
                                      body)
                                     "\n") ))
    (let ((cmd (concat "hledger"
                       (if (> (length body) 0)
                           (concat " -f " (org-babel-process-file-name in-file))
                         "")
                       " " cmdline)))
      (message "%s" cmd)
      (with-output-to-string
        (shell-command (concat cmd " > " (org-babel-process-file-name out-file)))) )
    (with-temp-buffer (insert-file-contents out-file) (buffer-string))))

(defun org-babel-prep-session:hledger (session params)
  (error "hledger does not support sessions"))

(provide 'ob-hledger)

;;; ob-hledger.el ends here
