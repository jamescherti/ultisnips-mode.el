;;; ultisnips-mode.el --- Major mode for editing Ultisnips snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Version: 1.0.1
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
;; Major mode for editing Ultisnips snippets.

;;; Code:

(defgroup ultisnips nil
  "Emacs major mode for editing Ultisnips snippets."
  :group 'ultisnips
  :prefix "ultisnips-mode-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/ultisnips-mode.el"))

(defcustom ultisnips-mode-hook nil
  "Hooks called when Lua mode fires up."
  :type 'hook
  :options '(hs-minor-mode
             outline-minor-mode))

(defvar ultisnips-mode-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for `ultisnips-mode'.")

(defvar ultisnips-mode--sexp-alist
  '(("snippet" . "endsnippet")
    ("global"  . "endglobal")))

(defun ultisnips-mode--outline-level ()
  "Return the outline level for snippet blocks."
  (if (looking-at "^snippet")
      1
    2))

(defun ultisnips-mode--forward-sexp (&optional arg)
  "Move point forward across ARG blocks, ending at the block terminator."
  (interactive "p")
  (let ((count (or arg 1))
        (ends (mapcar #'cdr ultisnips-mode--sexp-alist)))
    (save-match-data
      (while (> count 0)
        (skip-chars-forward " \t\n")
        (beginning-of-line)

        (let ((done nil))
          ;; If already on an end keyword, just move past it
          (dolist (end ends)
            (when (looking-at (concat "^" end "\\_>"))
              (end-of-line)
              (setq done t)))

          ;; Otherwise search forward for the next end keyword
          (unless done
            (unless (re-search-forward
                     (concat "^" (regexp-opt ends 'words) "\\_>") nil t)
              (error "No further UltiSnips block end found"))
            (end-of-line)))

        (setq count (1- count))))
    (point)))

(defun ultisnips-mode--hs-forward-sexp (&optional arg)
  "Move point forward across ARG UltiSnips blocks.

ARG specifies how many block movements to perform. Each movement advances point
to the end of the current UltiSnips block, or, if point is not at a block start,
to the end of the next block. When ARG is nil, treat it as 1."
  (interactive "p")
  (let ((count (or arg 1)))
    (save-match-data
      (while (> count 0)
        ;; Normalize position
        (beginning-of-line)

        (cond
         ;; At snippet start: jump to its end
         ((looking-at "^snippet\\_>")
          (goto-char (match-end 0))
          (unless (re-search-forward "^endsnippet\\_>" nil t)
            (error "No matching endsnippet found"))
          (end-of-line))

         ;; At global start: jump to its end
         ((looking-at "^global\\_>")
          (goto-char (match-end 0))
          (unless (re-search-forward "^endglobal\\_>" nil t)
            (error "No matching endglobal found"))
          (end-of-line))

         ;; Otherwise, jump to next block start
         (t
          (unless (re-search-forward "^\\(snippet\\|global\\)\\_>" nil t)
            (error "No further UltiSnips block found"))
          (goto-char (match-beginning 0))))

        (setq count (1- count))))))

(defvar hs-block-start-regexp)
(defvar hs-block-end-regexp)

;;;###autoload
(define-derived-mode ultisnips-mode prog-mode "Ultisnips"
  "Major mode for editing *.snippets files."
  :syntax-table ultisnips-mode-syntax-table
  (setq-local font-lock-multiline t)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local indent-line-function #'ignore)

  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  ;; `outline-minor-mode'
  (setq-local outline-level #'ultisnips-mode--outline-level)
  (setq-local outline-regexp  "^snippet")

  ;; `hs-minor-mode'
  (setq-local hs-block-start-regexp "^\\(snippet\\|global\\)\\_>")
  (setq-local hs-block-end-regexp "^\\(endsnippet\\|endglobal\\)\\_>")

  (unless (assq 'ultisnips-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 `(ultisnips-mode
                   ;; Start
                   ,(regexp-opt (mapcar #'car ultisnips-mode--sexp-alist) 'words)
                   ;; End
                   ,(regexp-opt (mapcar #'cdr ultisnips-mode--sexp-alist) 'words)
                   nil
                   ultisnips-mode--hs-forward-sexp)))

  (setq-local forward-sexp-function #'ultisnips-mode--forward-sexp)

  ;; Font lock: Override font-lock settings to remove inherited highlighting
  (setq-local
   font-lock-defaults
   '(( ;; Comments
      ("^\\s-*#.*" . font-lock-comment-face)
      ;; snippet, endsnippet, priority as functions
      ("^\\(priority\\|extends\\)\\_>" .
       font-lock-function-name-face)
      ;; snippet [trigger] ["description"] [options]
      ("^\\(snippet\\|endsnippet\\)\\_>" .
       font-lock-function-name-face)
      ("^\\(global\\|endglobal\\)\\_>" .
       font-lock-function-name-face)
      ("^\\(post_jump\\)\\s-+\\(.*\\)$"
       (1 font-lock-function-name-face)
       (2 font-lock-string-face nil t))
      ("^snippet\\s-+\\(\\S-+\\)\\(.*\\)$"
       (1 font-lock-keyword-face)
       (2 font-lock-string-face nil t))
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
(add-to-list 'auto-mode-alist
             '("\\.[sS][nN][iI][pP][pP][eE][tT][sS]\\'" . ultisnips-mode))

(provide 'ultisnips-mode)
;;; ultisnips-mode.el ends here
