;;; sops.el --- SOPS encrypt and decrypt without leaving the editor -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>

;; Author:  Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>
;; Keywords: convenience, programming
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (s "1.13.0"))
;; Homepage: http://github.com/djgoku/sops
;; Keywords: convenience files tools sops encrypt decrypt

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

;; This package allows for easy sops encrypting and decrypting of riles.

;;; Code:

(require 's)

(defgroup sops nil "SOPS encrypt and decrypt without leaving the editor.." :prefix 'sops :group 'convenience)

(defcustom sops-executable "sops"
  "Path to the sops executable."
  :group 'sops
  :type 'string)

(defcustom sops-decrypt-args
  `("-d")
  "Decrypt arguments for sops."
  :group 'sops
  :type '(repeat string))

(defcustom sops-before-encrypt-decrypt-hook nil
  "Hook run before encrypting or decrypting a sops file."
  :group 'sops
  :type 'hook)

(defvar-local sops--status nil
  "Used to track state.")
(defvar-local sops--original-buffer-string nil)
(defvar-local sops--original-buffer-name nil)
(defvar-local sops--original-buffer-file-name nil)
(defvar sops--sops-version-greater-than-equal-to-3-9 -1
  "Used to signify sops executable version is greater than or equal to version 3.9.")

;;;###autoload
(define-minor-mode sops-mode
  "Managing SOPS encrypted files without leaving your favorite editor."
  :init-value nil
  :group 'sops
  :lighter " sops"

  (when (eq sops--sops-version-greater-than-equal-to-3-9 -1)
    (sops--version-check))

  (cond ((not sops-mode) (progn
                           (setq-local sops-mode nil)
                           (setq-local sops--status nil)))
        ((and (bound-and-true-p sops--status) (equal sops--status "decrypted")) (setq-local sops-mode 1))
        ((sops--is-sops-file) (setq-local sops-mode 1))
        ((not (sops--is-sops-file)) (progn
                                      (setq-local sops-mode nil)
                                      (setq-local sops--status nil)
                                      ;; (message "%s is not a sops encrypted file" (buffer-file-name))
                                      ))))

;;;###autoload
(define-globalized-minor-mode global-sops-mode sops-mode sops-turn-on-sops-mode)

;;;###autoload
(defun sops-turn-on-sops-mode ()
  "Turn on sops mode globally."
  (if (and global-sops-mode (executable-find sops-executable))
      (add-hook 'after-change-major-mode-hook #'sops-mode)
    (remove-hook 'after-change-major-mode-hook #'sops-mode)))

;;;###autoload
(defun sops-edit-file ()
  "Open a sops encrypted file and decrypt it."
  (interactive)
  
  (when sops-mode
    (run-hooks 'sops-before-encrypt-decrypt-hook)
    (sops--clean-up-buffers (buffer-file-name))
    (let* ((temp-file-error (make-temp-file "sops-mode-error"))
           (original-file-name (buffer-file-name))
           (sops-process-error-buffer (get-buffer-create (format "*sops-mode-process-error*-%s" original-file-name)))
           (sops-process-buffer (get-buffer-create (format "*sops-mode-process*-%s" original-file-name)))
           (buffer-string (buffer-string))
           (buffer-name (buffer-name))
           (original-major-mode (with-current-buffer (current-buffer) major-mode))
           (sops-save-file-key-description (key-description (where-is-internal 'sops-save-file nil t)))
           (sops-cancel-key-description (key-description (where-is-internal 'sops-cancel nil t))))
      (if (eq (apply #'call-process sops-executable nil `(,sops-process-buffer ,temp-file-error) nil (append sops-decrypt-args (list original-file-name))) 0)
          (progn
            (switch-to-buffer sops-process-buffer)
            (funcall original-major-mode)
            (setq-local sops--original-buffer-file-name original-file-name
                        sops--status "decrypted"
                        temp-file-modtime (visited-file-modtime)
                        sops--original-buffer-string buffer-string
                        sops--original-buffer-name buffer-name)
            (sops-mode 1)
            (set-buffer-modified-p nil)
            (message (format "%s to save modifications or %s to cancel modifications" sops-save-file-key-description sops-cancel-key-description)))
        (switch-to-buffer sops-process-error-buffer)
        (insert-file-contents temp-file-error)
        (read-only-mode)))))

;;;###autoload
(defun sops-save-file ()
  "Save modified sops file and close it."
  (interactive)
  (when (and (bound-and-true-p sops--status) sops-mode)
    (if (buffer-modified-p)
        (when (zerop (sops-encrypt-file))
          (if (bound-and-true-p sops--sops-version-greater-than-equal-to-3-9)
              (let ((encrypted-buffer-contents (buffer-string))
                    (original-file-name sops--original-buffer-file-name))
                (switch-to-buffer sops--original-buffer-name)
                (erase-buffer)
                (insert encrypted-buffer-contents)
                (save-buffer)
                (sops--clean-up-buffers original-file-name))
            (sops--clean-up-buffers (buffer-file-name))))
      (message "no changes were made to file, closing buffer")
      (kill-buffer (get-buffer "*sops-mode-process*"))
      (erase-buffer)
      (insert sops--original-buffer-string)
      (save-buffer))))

(defun sops-encrypt-file ()
  "Sops encrypt data."
  (run-hooks 'sops-before-encrypt-decrypt-hook)
  (if sops--sops-version-greater-than-equal-to-3-9
      (call-process-region (point-min) (point-max) sops-executable t t nil "--filename-override" sops--original-buffer-file-name "--encrypt" "/dev/stdin")
    (let ((decrypted-buffer-contents (buffer-string)))
      (switch-to-buffer sops--original-buffer-name)
      (erase-buffer)
      (insert decrypted-buffer-contents)
      (save-buffer)
      (call-process sops-executable nil nil nil "-e" "-i" (buffer-file-name)))))

(defun sops-cancel ()
  "Cancel saving sops encrypted data."
  (interactive)
  (when (and (bound-and-true-p sops--status) sops-mode)
    (sops--clean-up-buffers sops--original-buffer-file-name)))

(defun sops--is-sops-file ()
  "Check to see if a file is a sops encrypted file."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "ENC[AES256_GCM" nil t 2)
      t)))

(defun sops--clean-up-buffers (filename)
  "Clean up buffers we might have created using FILENAME as the suffix."
  (dolist (elt '("*sops-mode-process*-%s" "*sops-mode-process-error*-%s"))
    (when-let (buffer (get-buffer (format elt filename)))
      (kill-buffer buffer))))

(defun sops--version-check ()
  "Check that sops version is greater than or equal to 3.9."
  (with-temp-buffer
    (when (zerop (call-process sops-executable nil t nil "--version"))
      (goto-char (point-min))
      (re-search-forward (rx (+ digit) (+ (and "." (+ digit)))))
      (setq sops--sops-version-greater-than-equal-to-3-9
            (not (string-version-lessp (match-string 0) "3.9.0"))))))

(provide 'sops)
;;; sops.el ends here
