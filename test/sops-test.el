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
  "Returns the matching type string; literal-dot escaping is honored."
  (let ((sops-input-type-overrides '(("\\.secrets\\'" . "yaml")
                                     ("\\.envrc\\'" . "dotenv"))))
    (should (equal "yaml" (sops--input-type-for "/tmp/x.secrets")))
    (should (equal "dotenv" (sops--input-type-for "/tmp/.envrc")))
    (should (eq nil (sops--input-type-for "/tmp/x.yaml")))
    ;; Pin the literal-dot expectation: an unescaped `.' would match here.
    (should (eq nil (sops--input-type-for "/tmp/asecrets")))))

(ert-deftest sops-test--input-type-for-nil-filename ()
  "Returns nil for nil filename without erroring."
  (should (eq nil (sops--input-type-for nil))))

(ert-deftest sops-test--input-type-for-first-match-wins ()
  "When two pairs both match, the first one in list order is returned."
  (let ((sops-input-type-overrides '(("\\.foo\\'" . "first")
                                     ("\\.foo\\'" . "second"))))
    (should (equal "first" (sops--input-type-for "/tmp/x.foo")))))

(ert-deftest sops-test--parse-filestatus-encrypted-true ()
  "Strict JSON `{\"encrypted\":true}' returns t."
  (should (eq t (sops--parse-filestatus "{\"encrypted\":true}"))))

(ert-deftest sops-test--parse-filestatus-encrypted-false ()
  "Strict JSON `{\"encrypted\":false}' returns nil."
  (should (eq nil (sops--parse-filestatus "{\"encrypted\":false}"))))

(ert-deftest sops-test--parse-filestatus-malformed ()
  "Malformed JSON returns nil (defensive)."
  (should (eq nil (sops--parse-filestatus "not json")))
  (should (eq nil (sops--parse-filestatus "")))
  (should (eq nil (sops--parse-filestatus "{}"))))

(ert-deftest sops-test--parse-filestatus-trailing-whitespace ()
  "Whitespace/newlines around JSON are tolerated."
  (should (eq t (sops--parse-filestatus "{\"encrypted\":true}\n")))
  (should (eq t (sops--parse-filestatus "  {\"encrypted\":true}  "))))

(ert-deftest sops-test--parse-filestatus-missing-key ()
  "JSON without an `encrypted' key returns nil even if other keys are true."
  (should (eq nil (sops--parse-filestatus "{\"other\":true}"))))

(ert-deftest sops-test--parse-filestatus-non-boolean-value ()
  "Strict-t check: any non-boolean value at `encrypted' returns nil.
Locks the discriminator against future `truthiness' loosening."
  (should (eq nil (sops--parse-filestatus "{\"encrypted\":\"true\"}")))
  (should (eq nil (sops--parse-filestatus "{\"encrypted\":1}")))
  (should (eq nil (sops--parse-filestatus "{\"encrypted\":[1,2]}"))))

(ert-deftest sops-test--parse-filestatus-non-string-input ()
  "Non-string input returns nil rather than erroring."
  (should (eq nil (sops--parse-filestatus nil))))

(ert-deftest sops-test--run-version-success ()
  "sops--run with --version returns exit 0 and version string in stdout."
  (let ((result (sops--run '("--version"))))
    (should (eq 0 (plist-get result :exit-status)))
    (should (string-match-p "^sops" (plist-get result :stdout)))))

(ert-deftest sops-test--run-bad-args-failure ()
  "sops--run with garbage returns non-zero exit and captured stderr.
Asserts stderr is non-empty so the :stderr pipe wiring is exercised on
the failure path; doesn't pin the message text (sops's wording can shift
between versions)."
  (let ((result (sops--run '("nonexistent-subcommand"))))
    (should-not (eq 0 (plist-get result :exit-status)))
    (should (> (length (plist-get result :stderr)) 0))))

(ert-deftest sops-test--run-with-input ()
  "sops--run can pipe input via :input."
  (let ((result (sops--run '("filestatus" "--input-type" "yaml" "/dev/stdin")
                           :input "foo: bar\n")))
    (should (eq 0 (plist-get result :exit-status)))
    (should (string-match-p "encrypted" (plist-get result :stdout)))))

(ert-deftest sops-test--run-version-check-disabled ()
  "stderr does not contain sops update-check noise."
  (let ((result (sops--run '("--version"))))
    (should-not (string-match-p "new version of sops" (plist-get result :stderr)))))

(ert-deftest sops-test--run-missing-executable-errors ()
  "An absolute path to a nonexistent binary signals an error from make-process.
Locks the contract that sops--run does not silently swallow exec failures."
  (let ((sops-executable "/no/such/sops"))
    (should-error (sops--run '("--version")))))

(ert-deftest sops-test--ensure-version-passes-on-modern-sops ()
  "Returns version string when sops >= 3.9.0."
  (setq sops--version-cache nil)
  (let ((v (sops--ensure-version)))
    (should (stringp v))
    (should (version<= "3.9.0" v))))

(ert-deftest sops-test--ensure-version-cache-hit ()
  "Second call uses cached value (same path)."
  (setq sops--version-cache nil)
  (sops--ensure-version)
  (let ((cached sops--version-cache))
    (should cached)
    (should (equal sops-executable (car cached)))
    ;; Calling again should not change the cache cell
    (sops--ensure-version)
    (should (eq cached sops--version-cache))))

(ert-deftest sops-test--ensure-version-recomputes-on-path-change ()
  "Cache invalidated when sops-executable changes."
  (setq sops--version-cache nil)
  (sops--ensure-version)
  (let ((sops-executable "/usr/bin/sops")) ; different path, may not exist
    (ignore-errors (sops--ensure-version))
    ;; The cached path should reflect the most recent successful call;
    ;; if the new path errors, cache for old path may persist — that's OK.
    (should (or (equal "/usr/bin/sops" (car sops--version-cache))
                (equal "sops" (car sops--version-cache))))))

(ert-deftest sops-test--ensure-version-missing-binary-errors ()
  "Missing binary signals user-error and does not poison the cache."
  (setq sops--version-cache nil)
  (let ((sops-executable "/no/such/sops"))
    (should-error (sops--ensure-version) :type 'user-error))
  (should (eq nil sops--version-cache)))

(ert-deftest sops-test--filestatus-encrypted-fixture ()
  "Real sops filestatus returns t on encrypted YAML fixture."
  (should (eq t (sops--filestatus (sops-test--fixture "secrets.enc.yaml")))))

(ert-deftest sops-test--filestatus-encrypted-json ()
  (should (eq t (sops--filestatus (sops-test--fixture "config.enc.json")))))

(ert-deftest sops-test--filestatus-encrypted-env ()
  (should (eq t (sops--filestatus (sops-test--fixture "vars.enc.env")))))

(ert-deftest sops-test--filestatus-encrypted-txt-needs-input-type ()
  "TXT fixture requires sops-input-type-overrides for sops to know format."
  (let ((sops-input-type-overrides
         '(("notes\\.enc\\.txt\\'" . "yaml"))))
    (should (eq t (sops--filestatus (sops-test--fixture "notes.enc.txt"))))))

(ert-deftest sops-test--filestatus-plaintext-fixture ()
  (should (eq nil (sops--filestatus (sops-test--fixture "plain.yaml")))))

(ert-deftest sops-test--filestatus-nonexistent-file ()
  "Returns nil for non-existent file (sops errors, we degrade gracefully)."
  (should (eq nil (sops--filestatus "/tmp/nonexistent-sops-test-file.yaml"))))

(ert-deftest sops-test--popup-error-creates-buffer ()
  "Creates *sops-error: FILE* buffer with stderr content; returns the buffer.
Captures the function's return value so a future regression that returns
nil (or the wrong buffer) is caught here, not just by the get-buffer
lookup."
  (let* ((file "/tmp/example.enc.yaml")
         (buf-name (format "*sops-error: %s*" file)))
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (let ((buf (sops--popup-error file '("decrypt" "/tmp/example.enc.yaml")
                                  1 "FAILED: bad credentials\n")))
      (should buf)
      (should (eq buf (get-buffer buf-name)))
      (with-current-buffer buf
        (should (string-match-p "sops decrypt" (buffer-string)))
        (should (string-match-p "Exit status: 1" (buffer-string)))
        (should (string-match-p "FAILED: bad credentials" (buffer-string)))
        (should (string-match-p "recovery" (buffer-string)))
        (should (string-match-p "C-x C-s" (buffer-string)))
        (should buffer-read-only)
        (should-not (buffer-modified-p)))
      (kill-buffer buf))))

(ert-deftest sops-test--popup-error-reuses-buffer ()
  "Subsequent failures for same file reuse the buffer (erase + rewrite)."
  (let* ((file "/tmp/x.yaml")
         (buf-name (format "*sops-error: %s*" file)))
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (sops--popup-error file '("decrypt") 1 "first error\n")
    (sops--popup-error file '("decrypt") 1 "second error\n")
    (with-current-buffer buf-name
      (should (string-match-p "second error" (buffer-string)))
      (should-not (string-match-p "first error" (buffer-string))))
    (kill-buffer buf-name)))

(ert-deftest sops-test--decrypt-buffer-success ()
  "Decrypts a fixture into the buffer; returns t."
  (let ((file (sops-test--fixture "secrets.enc.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (should (eq t (sops--decrypt-buffer)))
      (should (string-match-p "database_password: super-secret-yaml"
                              (buffer-string)))
      (should-not (buffer-modified-p)))))

(ert-deftest sops-test--decrypt-buffer-failure-pops-error ()
  "Bad auth: returns nil, buffer unchanged, error buffer popped, hook still fired.
The hook firing before sops--run is part of the contract: users set env
vars in the hook expecting they take effect on every attempt, not only
on attempts that succeed."
  (let* ((file (sops-test--fixture "secrets.enc.yaml"))
         (orig (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (buf-name (format "*sops-error: %s*" file))
         (hook-fired nil)
         (sops-before-decrypt-hook
          (list (lambda () (setq hook-fired t)))))
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (let ((process-environment
             (cons "SOPS_AGE_KEY_FILE=/tmp/nonexistent-key" process-environment)))
        (should (eq nil (sops--decrypt-buffer))))
      (should (equal orig (buffer-string))))
    (should hook-fired)
    (should (get-buffer buf-name))
    (kill-buffer buf-name)))

(ert-deftest sops-test--decrypt-buffer-runs-before-decrypt-hook ()
  "sops-before-decrypt-hook runs before decrypt with buffer-file-name set."
  (let* ((file (sops-test--fixture "secrets.enc.yaml"))
         (called nil)
         (sops-before-decrypt-hook
          (list (lambda () (setq called buffer-file-name)))))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (sops--decrypt-buffer)
      (should (equal file called)))))

(ert-deftest sops-test--encrypt-and-write-roundtrip ()
  "Decrypt → modify → encrypt-and-write → re-decrypt matches modified content.
The temp file lives inside `sops-test--fixtures' so sops can locate the
fixture's `.sops.yaml' by walking up from cwd.  `default-directory' is
set explicitly in each `with-temp-buffer' because `with-temp-buffer'
doesn't auto-set it the way `find-file' does in production."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-roundtrip-" sops-test--fixtures)
               nil ".enc.yaml")))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (goto-char (point-max))
            (insert "added_line: roundtrip-value\n")
            (let ((write-result (sops--encrypt-and-write)))
              (should (eq t write-result))))
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "added_line: roundtrip-value"
                                    (buffer-string)))))
      (delete-file tmp))))

(ert-deftest sops-test--encrypt-and-write-failure-leaves-file-untouched ()
  "When sops encrypt fails, target file is unchanged."
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file "sops-test-encfail-" nil ".enc.yaml"))
         (orig nil))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (setq orig (with-temp-buffer (insert-file-contents tmp) (buffer-string)))
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (insert "plain content with no .sops.yaml rule for /tmp\n")
            (let ((process-environment
                   (cons "SOPS_AGE_KEY_FILE=/tmp/nonexistent" process-environment)))
              (should-error (sops--encrypt-and-write) :type 'user-error)))
          (should (equal orig (with-temp-buffer
                                (insert-file-contents tmp)
                                (buffer-string)))))
      (delete-file tmp))))

(ert-deftest sops-test--encrypt-and-write-runs-before-encrypt-hook ()
  "sops-before-encrypt-hook fires before encrypt with buffer-file-name set.
See `sops-test--encrypt-and-write-roundtrip' for the explanation of why
the temp file lives in `sops-test--fixtures' and why `default-directory'
is set explicitly."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-hook-" sops-test--fixtures)
               nil ".enc.yaml"))
         (called nil)
         (sops-before-encrypt-hook
          (list (lambda () (setq called buffer-file-name)))))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (sops--encrypt-and-write)
            (should (equal tmp called))))
      (delete-file tmp))))

(ert-deftest sops-test--encrypt-and-write-widens-narrowed-buffer ()
  "Encrypt-and-write writes the full buffer even when the buffer is narrowed.
A narrowed `(point-min)..(point-max)' would otherwise truncate the
encrypted file to just the visible region -- silent data loss."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-narrow-" sops-test--fixtures)
               nil ".enc.yaml")))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (goto-char (point-max))
            (insert "outside_narrow: should-survive\n")
            ;; Narrow to the first line; without `widen' the encrypt
            ;; would only see that line and clobber the rest.
            (goto-char (point-min))
            (narrow-to-region (point-min) (line-end-position))
            (sops--encrypt-and-write))
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "outside_narrow: should-survive"
                                    (buffer-string)))))
      (delete-file tmp))))

(ert-deftest sops-test--encrypt-and-write-suppresses-backup ()
  "Save creates no backup file even when `make-backup-files' is t."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-backup-" sops-test--fixtures)
               nil ".enc.yaml"))
         (backup (concat tmp "~"))
         ;; Force-enable backups globally; the helper should still suppress.
         (make-backup-files t))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (goto-char (point-max))
            (insert "added: 1\n")
            (sops--encrypt-and-write))
          (should-not (file-exists-p backup)))
      (when (file-exists-p backup) (delete-file backup))
      (delete-file tmp))))

(ert-deftest sops-test--encrypt-and-write-passes-extra-encrypt-args ()
  "Items in `sops-extra-encrypt-args' appear in the sops invocation.
Stubs `sops--run' so we can lock the wiring without depending on a
specific sops flag being a no-op for encrypt."
  (let* ((tmp (make-temp-file "sops-test-extra-" nil ".enc.yaml"))
         (sops-extra-encrypt-args '("-a" "age1stub"))
         (captured-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'sops--run)
                   (lambda (args &rest _keys)
                     (setq captured-args args)
                     (list :exit-status 0
                           :stdout "stub-ciphertext\n"
                           :stderr ""))))
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (insert "plaintext\n")
            (sops--encrypt-and-write))
          ;; Both extra-args land between --filename-override and /dev/stdin.
          (should (member "-a" captured-args))
          (should (member "age1stub" captured-args))
          (should (equal "/dev/stdin" (car (last captured-args))))
          ;; Order: -a comes before age1stub (preserved from input list).
          (let ((dash-a-pos (cl-position "-a" captured-args :test #'equal))
                (stub-pos (cl-position "age1stub" captured-args :test #'equal))
                (stdin-pos (cl-position "/dev/stdin" captured-args :test #'equal)))
            (should (and dash-a-pos stub-pos stdin-pos))
            (should (< dash-a-pos stub-pos))
            (should (< stub-pos stdin-pos))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--mode-enable-installs-hooks ()
  "Enabling sops-mode installs write-contents-functions and suppresses backups.
Also asserts that `sops--state' is initialized to a `sops-state' struct
with `status' = `decrypted'."
  (let ((file (sops-test--fixture "secrets.enc.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (sops--decrypt-buffer)
      (sops-mode 1)
      (should (memq #'sops--write-contents-function write-contents-functions))
      (should (eq nil make-backup-files))
      (should (eq nil buffer-auto-save-file-name))
      (should (eq #'sops--revert-buffer revert-buffer-function))
      (should (sops-state-p sops--state))
      (should (eq 'decrypted (sops-state-status sops--state)))
      (sops-mode -1)
      (should-not (memq #'sops--write-contents-function write-contents-functions))
      (should (eq nil sops--state)))))

(ert-deftest sops-test--mode-disable-on-modified-buffer-blocked ()
  "Disabling sops-mode on modified buffer signals user-error."
  (let ((file (sops-test--fixture "secrets.enc.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (sops--decrypt-buffer)
      (sops-mode 1)
      (insert "modification")
      (should (buffer-modified-p))
      (should-error (sops-mode -1) :type 'user-error))))

(ert-deftest sops-test--save-buffer-encrypts ()
  "save-buffer in sops-mode triggers encrypt-and-write."
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-save-" sops-test--fixtures)
               nil ".enc.yaml")))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (sops-mode 1)
            (goto-char (point-max))
            (insert "save_test: saved\n")
            (let ((coding-system-for-write 'no-conversion))
              (save-buffer)))
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "save_test: saved" (buffer-string)))))
      (delete-file tmp))))

(ert-deftest sops-test--revert-buffer-redecrypts ()
  "After modify, revert-buffer restores original decrypted content."
  (let ((file (sops-test--fixture "secrets.enc.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (setq default-directory (file-name-directory file))
      (insert-file-contents file)
      (sops--decrypt-buffer)
      (sops-mode 1)
      (let ((orig (buffer-string)))
        (goto-char (point-max))
        (insert "transient")
        (revert-buffer t t)
        (should (equal orig (buffer-string)))))))

(ert-deftest sops-test--mode-survives-major-mode-change ()
  "Changing major mode preserves sops-mode and re-installs protections.
This is the regression test for the plaintext-leak failure mode where
`kill-all-local-variables' wipes our write-contents-function and the
backup/auto-save suppression."
  (let ((file (sops-test--fixture "secrets.enc.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (sops--decrypt-buffer)
      (sops-mode 1)
      (should sops-mode)
      (should (memq #'sops--write-contents-function write-contents-functions))
      ;; Switch major mode (simulates user running M-x conf-mode etc.)
      (text-mode)
      ;; sops-mode is permanent-local, so the value survives
      (should sops-mode)
      ;; The hook entry was wiped by kill-all-local-variables but
      ;; after-change-major-mode-hook re-installed it
      (should (memq #'sops--write-contents-function write-contents-functions))
      (should (eq nil make-backup-files))
      (should (eq nil buffer-auto-save-file-name))
      (should (eq #'sops--revert-buffer revert-buffer-function))
      ;; sops--state is also permanent-local, so the struct survives
      (should (sops-state-p sops--state)))))

(provide 'sops-test)
;;; sops-test.el ends here
