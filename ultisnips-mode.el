;;; ultisnips-mode.el --- Emacs major mode for editing Ultisnips snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/ultisnips-mode.el
;; Keywords: languages
;; Package-Requires: ((emacs "26.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Emacs major mode for editing Ultisnips snippets.

;;; Code:

(defgroup ultisnips nil
  "Emacs major mode for editing Ultisnips snippets."
  :group 'ultisnips
  :prefix "ultisnips-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/ultisnips-mode.el"))

(defvar ultisnips-mode-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for `ultisnips-mode'.")

;;;###autoload
(define-derived-mode ultisnips-mode prog-mode "Ultisnips"
  "Major mode for editing *.snippets files."
  :syntax-table ultisnips-mode-syntax-table
  (setq-local font-lock-multiline t)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local indent-line-function #'ignore)

  ;; Override font-lock settings to remove inherited highlighting
  (setq-local
   font-lock-defaults
   '(( ;; snippet, endsnippet, priority as functions
      ("^\\(snippet\\|endsnippet\\|priority\\)\\b" . font-lock-function-name-face)
      ;; First word after snippet as a variable
      ("^snippet\\s-+\\(\\S-+\\)" 1 font-lock-keyword-face)
      ;; Matches ${1:var_name}
      ("\\${\\([0-9]+\\):[^}]*}" 0 font-lock-variable-name-face)
      ;; Matches $1, $2, etc.
      ("\\$[0-9]+" . font-lock-variable-name-face))
     t    ;; keywords-only
     nil  ;; case-fold
     nil  ;; syntax-alist
     nil  ;; syntax-begin
     )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[sS][nN][iI][pP][pP][eE][tT][sS]\\'" . ultisnips-mode))

(provide 'ultisnips-mode)
;;; ultisnips-mode.el ends here
