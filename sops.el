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

(provide 'sops)
;;; sops.el ends here
