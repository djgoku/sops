;;; sops.el --- Edit SOPS-encrypted files transparently  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>

;; Author:  Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>
;; Keywords: convenience files tools sops encrypt decrypt
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/djgoku/sops

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Transparent decryption on find-file and encryption on save-buffer for
;; SOPS-encrypted files.  See README and the design spec at
;; docs/superpowers/specs/ for architecture and rationale.

;;; Code:

(require 'cl-lib)

(cl-defstruct (sops-state (:constructor sops-state-create)
                          (:conc-name sops-state-))
  "Buffer-local state for sops-mode."
  status        ; 'decrypted | 'creating
  last-error)   ; nil or string with most recent sops stderr

(defgroup sops nil
  "Edit SOPS-encrypted files transparently."
  :group 'convenience
  :prefix "sops-")

(defcustom sops-prefilter-regex
  "\\.\\(ya?ml\\|json\\|env\\|ini\\|txt\\)\\'"
  "Filename regex.  Files matching trigger a `sops filestatus' check on find-file.
Files not matching are never checked, so their open path is unaffected."
  :type 'regexp
  :group 'sops)

(defun sops--prefilter-p (filename)
  "Return non-nil if FILENAME should be checked by sops filestatus."
  (and filename (string-match-p sops-prefilter-regex filename)))

(defcustom sops-input-type-overrides nil
  "Alist of (REGEX . INPUT-TYPE) for files whose extension sops can't infer.
When the file path matches REGEX, INPUT-TYPE is passed as
\"--input-type INPUT-TYPE\" to sops filestatus, decrypt, and encrypt.

Each car is an Emacs regular expression matched against the file path
with `string-match-p'; remember to escape literal dots (\".envrc\" matches
any single char before \"envrc\"; use \"\\\\.envrc\" for a literal dot).
Each cdr is the parser name sops should use (\"yaml\", \"json\",
\"dotenv\", \"ini\", etc.).  Pairs are tried in list order; the first
match wins."
  :type '(alist :key-type regexp :value-type string)
  :group 'sops)

(defun sops--input-type-for (filename)
  "Return input-type string for FILENAME from `sops-input-type-overrides', or nil.
Pairs in the alist are tried in list order; the first match wins."
  (when filename
    (cdr (cl-find-if (lambda (pair) (string-match-p (car pair) filename))
                     sops-input-type-overrides))))

(provide 'sops)
;;; sops.el ends here
