;;; sops-test.el --- Tests for sops.el v2  -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'sops)

;; Compute fixture directory from this file's location
(defvar sops-test--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing this test file.")

(defvar sops-test--fixtures
  (expand-file-name "test-fixtures" sops-test--directory)
  "Directory where ephemeral test fixtures live.  Untracked by git.")

(defvar sops-test--fixtures-ready nil
  "Non-nil after `sops-test--ensure-fixtures' has run successfully this session.")

(defun sops-test--read-public-key (key-file)
  "Extract the age public key (age1...) from KEY-FILE."
  (with-temp-buffer
    (insert-file-contents key-file)
    (goto-char (point-min))
    (when (re-search-forward "^# public key: \\(age1[^[:space:]]+\\)" nil t)
      (match-string 1))))

(defun sops-test--encrypt-string (input override-name &optional input-type)
  "Run sops encrypt on INPUT (string), return ciphertext as string.
OVERRIDE-NAME is passed via --filename-override and must match the
fixture's `.sops.yaml' creation_rules path_regex (i.e. include `.enc.').
INPUT-TYPE is optional.  Returns binary bytes; the caller should write
with `coding-system-for-write' bound to `no-conversion' to round-trip
losslessly.  `default-directory' must contain the relevant `.sops.yaml'."
  (with-temp-buffer
    (let* ((output-buf (current-buffer))
           (stderr-file (make-temp-file "sops-stderr-"))
           (args (append (list "encrypt" "--filename-override" override-name)
                         (when input-type (list "--input-type" input-type))
                         (list "/dev/stdin"))))
      (unwind-protect
          (let ((exit (with-temp-buffer
                        (insert input)
                        (apply #'call-process-region (point-min) (point-max)
                               "sops" nil (list output-buf stderr-file) nil args))))
            (unless (zerop exit)
              (error "sops encrypt failed for %s (exit %s): %s"
                     override-name exit
                     (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (buffer-string))))
            (buffer-string))
        (when (file-exists-p stderr-file) (delete-file stderr-file))))))

(defun sops-test--ensure-fixtures ()
  "Generate test fixtures in `sops-test--fixtures' if not already done.
Idempotent within a session via `sops-test--fixtures-ready'.
Generates: age key, .sops.yaml, encrypted YAML/JSON/ENV/TXT samples,
plus a plaintext negative-test sample."
  (unless sops-test--fixtures-ready
    (make-directory sops-test--fixtures t)
    (let* ((key-file  (expand-file-name "age-test-key.txt" sops-test--fixtures))
           (sops-yaml (expand-file-name ".sops.yaml"       sops-test--fixtures)))
      ;; 1. Generate age key (requires age-keygen on PATH).
      (unless (file-exists-p key-file)
        (let ((exit (call-process "age-keygen" nil nil nil "-o" key-file)))
          (unless (zerop exit) (error "age-keygen failed (exit %d)" exit)))
        (set-file-modes key-file #o600))
      ;; 2. Create .sops.yaml pointing at the public key.  The path_regex
      ;;    matches the .enc. naming convention used by the fixtures so
      ;;    plain.yaml stays plaintext.
      (let ((pub (sops-test--read-public-key key-file)))
        (unless pub (error "Could not extract public key from %s" key-file))
        (with-temp-file sops-yaml
          (insert "creation_rules:\n"
                  "  - path_regex: \\.enc\\.(yaml|json|env|txt)$\n"
                  "    age: " pub "\n")))
      ;; 3. Encrypt fixtures.  Each --filename-override matches the on-disk
      ;;    name (and therefore the .sops.yaml path_regex) so sops can find
      ;;    the creation rule.  Set SOPS_AGE_KEY_FILE explicitly so the
      ;;    helper works without mise's env active.
      (let ((process-environment
             (cons (concat "SOPS_AGE_KEY_FILE=" key-file) process-environment))
            (default-directory sops-test--fixtures))
        (let ((write (lambda (path content)
                       (with-temp-file path
                         (let ((coding-system-for-write 'no-conversion))
                           (insert content))))))
          (funcall write (expand-file-name "secrets.enc.yaml" sops-test--fixtures)
                   (sops-test--encrypt-string
                    "database_password: super-secret-yaml\napi_key: abc-123-xyz\n"
                    "secrets.enc.yaml"))
          (funcall write (expand-file-name "config.enc.json" sops-test--fixtures)
                   (sops-test--encrypt-string
                    "{\"db_password\": \"super-secret-json\", \"api_key\": \"json-abc-123\"}\n"
                    "config.enc.json"))
          (funcall write (expand-file-name "vars.enc.env" sops-test--fixtures)
                   (sops-test--encrypt-string
                    "DB_PASSWORD=super-secret-env\nAPI_KEY=env-abc-123\n"
                    "vars.enc.env"))
          (funcall write (expand-file-name "notes.enc.txt" sops-test--fixtures)
                   (sops-test--encrypt-string
                    "secret_token: txt-fixture-token\n"
                    "notes.enc.txt" "yaml"))
          (funcall write (expand-file-name "plain.yaml" sops-test--fixtures)
                   "not: a-sops-file\njust: plain-yaml\n"))))
    (setq sops-test--fixtures-ready t)))

(defun sops-test--fixture (name)
  "Return absolute path to fixture NAME, generating fixtures on first call."
  (sops-test--ensure-fixtures)
  (expand-file-name name sops-test--fixtures))

(ert-deftest sops-test--state-struct-defaults ()
  "A new sops-state has nil status and nil last-error."
  (let ((s (sops-state-create)))
    (should (eq nil (sops-state-status s)))
    (should (eq nil (sops-state-last-error s)))))

(ert-deftest sops-test--state-struct-fields ()
  "A sops-state can be created with explicit fields."
  (let ((s (sops-state-create :status 'decrypted :last-error "boom")))
    (should (eq 'decrypted (sops-state-status s)))
    (should (equal "boom" (sops-state-last-error s)))))

(ert-deftest sops-test--prefilter-matches-yaml ()
  "Default prefilter matches yaml/yml/json/env/ini/txt.
Pin `case-fold-search' so the suite isn't sensitive to runner state."
  (let ((case-fold-search t))
    (dolist (name '("/tmp/x.yaml" "/tmp/x.yml" "/tmp/x.json"
                    "/tmp/x.env" "/tmp/x.ini" "/tmp/x.txt"))
      (should (sops--prefilter-p name)))))

(ert-deftest sops-test--prefilter-rejects-others ()
  "Default prefilter rejects non-target extensions."
  (let ((case-fold-search t))
    (dolist (name '("/tmp/x.png" "/tmp/x.exe" "/tmp/x.gz" "/tmp/x.gpg" "/tmp/x.el" nil))
      (should-not (sops--prefilter-p name)))))

(ert-deftest sops-test--prefilter-respects-custom-regex ()
  "Custom sops-prefilter-regex overrides the default."
  (let ((case-fold-search t)
        (sops-prefilter-regex "\\.secrets\\'"))
    (should (sops--prefilter-p "/tmp/x.secrets"))
    (should-not (sops--prefilter-p "/tmp/x.yaml"))))

(ert-deftest sops-test--input-type-for-no-override ()
  "Returns nil when no override matches."
  (let ((sops-input-type-overrides nil))
    (should (eq nil (sops--input-type-for "/tmp/x.yaml")))))

(ert-deftest sops-test--input-type-for-with-override ()
  "Returns the matching type string."
  (let ((sops-input-type-overrides '((".secrets\\'" . "yaml")
                                     (".envrc\\'" . "dotenv"))))
    (should (equal "yaml" (sops--input-type-for "/tmp/x.secrets")))
    (should (equal "dotenv" (sops--input-type-for "/tmp/.envrc")))
    (should (eq nil (sops--input-type-for "/tmp/x.yaml")))))

(ert-deftest sops-test--input-type-for-nil-filename ()
  "Returns nil for nil filename without erroring."
  (should (eq nil (sops--input-type-for nil))))

(provide 'sops-test)
;;; sops-test.el ends here
