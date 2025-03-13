;;; ultisnips-mode.el --- Emacs major mode for editing Ultisnips snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/ultisnips-mode.el
;; Keywords: languages
;; Package-Requires: ((emacs "24.1"))
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

(defgroup ultisnips-mode nil
  "Emacs major mode for editing Ultisnips snippets"
  :group 'ultisnips-mode
  :prefix "ultisnips-mode-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/ultisnips-mode.el"))

(defcustom ultisnips-mode-verbose nil
  "Enable displaying messages (e.g., when files are compiled).
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'ultisnips-mode)

(defcustom ultisnips-mode-debug nil
  "Non-nil to display debug messages in the *ultisnips-mode:debug* buffer.
This displays a lot of messages."
  :type 'boolean
  :group 'ultisnips-mode)

(defun ultisnips-mode--message (&rest args)
  "Display a message with the same ARGS arguments as `message'."
  (apply #'message (concat "[ultisnips-mode] " (car args)) (cdr args)))

(defmacro ultisnips-mode--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when ultisnips-mode-debug
       (ultisnips-mode--debug-message ,(car args) ,@(cdr args)))
     (when ultisnips-mode-verbose
       (ultisnips-mode--message
        (concat "[ultisnips-mode] " ,(car args)) ,@(cdr args)))))

(defmacro ultisnips-mode--debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'.
The messages are displayed in the *ultisnips-mode* buffer."
  `(when ultisnips-mode-debug
     (ultisnips-mode--insert-message "*ultisnips-mode:debug*"
                                                    ,(car args) ,@(cdr args))))

;;;###autoload
(define-minor-mode ultisnips-mode-mode
  "Toggle `ultisnips-mode-mode'."
  :global t
  :lighter " ultisnips-mode"
  :group 'ultisnips-mode
  (if ultisnips-mode-mode
      t
    t))

(provide 'ultisnips-mode)
;;; ultisnips-mode.el ends here
