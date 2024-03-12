;;; sops-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jonathan Carroll Otsuka

;; Author:  Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Run the batch tests from root directory:
;; nix shell .github#emacsGit --quick --script .github/run-shim.el -- test
;; Test dependencies can be provided to the Emacsen declared inside the root
;; flake.nix.

;; For local development, dependencies should be installed by the user.  Tests
;; can be run from one of the project files using the `erk-ert-project'
;; command.

;;; Code:

(require 'ert)
(require 'sops)

(ert-deftest sops-test--sops--version-check-less-than-3-9 ()
  "Test that `sops--version-check' sets `sops--sops-version-greater-than-equal-to-3-9' to nil when sops version is less than 3.9"
  (setq sops-executable "sops")
  (should (equal (sops--version-check) nil))
  (should (equal sops--sops-version-greater-than-equal-to-3-9 nil)))

(ert-deftest sops-test--sops--version-check-greater-than-equal-to-3-9 ()
  "Test that `sops--version-check' sets `sops--sops-version-greater-than-equal-to-3-9' to t when sops version is greater than or equal to 3.9"
  (setq sops-executable "~/.golang/bin/sops")
  (sops--version-check)
  (should (equal (sops--version-check) t))
  (should (equal sops--sops-version-greater-than-equal-to-3-9 t)))

(ert-deftest sops-test--sops--is-sops-file-true ()
  "Test that `sops--is-sops-file' returns t when file is a sops encrypted file."
  (with-temp-buffer
    (insert "ENC[AES256_GCM" ?\n "ENC[AES256_GCM")
    (should (equal (sops--is-sops-file) t))))

(ert-deftest sops-test--sops--is-sops-file-nil ()
  "Test that `sops--is-sops-file' returns nil when file is not a sops encrypted file."
  (with-temp-buffer
    (insert "This is a great non-encrypted file")
    (should (equal (sops--is-sops-file) nil))))

(ert-deftest sops-test--sops-mode ()
  "Test that `sops-mode' sets the mode."
  (with-temp-buffer
    (should (equal (sops-mode) nil))
    (should (equal sops-mode nil)))
  (with-temp-buffer
    (setq-local sops--status "decrypted")
    (should (equal sops-mode nil))
    (should (equal (sops-mode) 1))
    (should (equal sops-mode 1)))
  (with-temp-buffer
    (insert "ENC[AES256_GCM" ?\n "ENC[AES256_GCM")
    (should (equal sops-mode nil))
    (should (equal (sops-mode) 1))
    (should (equal sops-mode 1)))
  (with-temp-buffer
    (should (equal sops-mode nil))
    (should (equal (sops-mode -1) nil))))

(ert-deftest sops-test--sops-edit-file-successfully-decrypt ()
  "Test we can open a sops encrypt file and decrypt it."
  ;; (global-sops-mode 1)
  (find-file "~/dev/github/getsops/sops/example.yaml")
  (sops-mode)
  (should (equal sops-mode 1))
  (should (equal sops--status nil))
  (sops-edit-file)
  (should (equal sops--original-buffer-file-name "/Users/dj_goku/dev/github/getsops/sops/example.yaml"))
  (should (equal sops--status "decrypted"))
  (should (equal (boundp 'sops--original-buffer-string) t))
  (should (equal (boundp 'sops--original-buffer-name) t))
  (should (equal (buffer-name) "*sops-mode-process*-/Users/dj_goku/dev/github/getsops/sops/example.yaml"))
  (should (equal (buffer-modified-p) nil)))

(ert-deftest sops-test--sops-edit-file-fails-decrypt ()
  "Test we can open a sops encrypt file and decrypt it."
  (find-file "~/dev/github/getsops/sops/README.rst")
  (sops-mode)
  (should (equal sops-mode 1))
  (should (equal sops--status nil))
  ;; :expected-result :failed
  (sops-edit-file)
  (should (equal (buffer-name) "*sops-mode-process-error*-/Users/dj_goku/dev/github/getsops/sops/README.rst"))
  (should (equal (>= (string-match "Error unmarshalling input json" (buffer-string)) 0) t)))

(provide 'sops-test)
;;; sops-test.el ends here.
