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

(defun sops--parse-filestatus (json-string)
  "Parse JSON-STRING from `sops filestatus'.
Return t if and only if the parsed object contains the boolean true at
key `encrypted'.  Any other shape -- nil input, non-string input, parse
error, missing key, non-boolean value -- collapses to nil so callers can
treat a nil return as `not known to be encrypted' rather than `definitely
plaintext'.  Leading/trailing whitespace in JSON-STRING is trimmed."
  (condition-case nil
      (let* ((trimmed (string-trim json-string))
             (parsed (json-parse-string trimmed :object-type 'alist)))
        (eq t (cdr (assq 'encrypted parsed))))
    (error nil)))

(defcustom sops-executable "sops"
  "Path to the sops binary.  Looked up via `executable-find' if not absolute."
  :type 'string
  :group 'sops)

(defun sops--run (args &rest keys)
  "Run sops with ARGS.
Keyword args:
  :input STRING  -- pipe STRING to sops's stdin
  :filter FN     -- process filter (nil in v2.0; populated in v2.2)
Return plist (:exit-status N :stdout STR :stderr STR)."
  (let* ((input (plist-get keys :input))
         (filter (plist-get keys :filter))
         (stdout-buf (generate-new-buffer " *sops-stdout*" t))
         (stderr-buf (generate-new-buffer " *sops-stderr*" t))
         (done nil)
         (proc nil))
    (unwind-protect
        (let ((process-environment
               (cons "SOPS_DISABLE_VERSION_CHECK=true" process-environment)))
          (setq proc
                (make-process
                 :name "sops"
                 :buffer stdout-buf
                 :stderr stderr-buf
                 :command (cons sops-executable args)
                 :connection-type 'pipe
                 :filter filter
                 :sentinel (lambda (_p _event) (setq done t))))
          ;; Force utf-8-unix on both ends.  Sops emits text (YAML/JSON/ENV/INI
          ;; with base64-encoded ENC[...] strings); locking the coding system
          ;; prevents CRLF translation on Windows-built Emacs from corrupting
          ;; the encrypted blob round-trip.
          (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
          (when input
            (process-send-string proc input)
            (process-send-eof proc))
          ;; This loop blocks the Emacs main thread until sops exits.  C-g
          ;; works (accept-process-output respects quit-flag), but sops calls
          ;; that hang waiting for interactive input (yubikey touch, age PIN)
          ;; freeze the UI until C-g.  v2.2 adds a process :filter that
          ;; watches stderr for known prompts and responds via process-send-string;
          ;; that's the architectural fix.
          (while (not done)
            (accept-process-output proc 0.1))
          (list :exit-status (process-exit-status proc)
                :stdout (with-current-buffer stdout-buf (buffer-string))
                :stderr (with-current-buffer stderr-buf (buffer-string))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf)))))

(provide 'sops)
;;; sops.el ends here
