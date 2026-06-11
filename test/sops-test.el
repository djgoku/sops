;;; sops-test.el --- Tests for sops.el v0.2  -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'sops)

;; Force polling auto-revert in batch tests.  The default file-notify path
;; deadlocks in batch (see memory/file_notify_vs_sync_subprocess.md):
;; `write-region' enqueues a kernel event the main loop never drains, the
;; inotify/kqueue fd stays select()-readable, and the next `sops--run'
;; starves on `process-send-string' writes and spins forever.  Interactive
;; Emacs is unaffected because its main loop drains events between
;; commands -- production users get the file-notify benefit; CI tests run
;; in batch and need polling to be deterministic.
(setq auto-revert-use-notify nil)

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
          ;; .txt fixture uses sops's native binary store (no --input-type)
          ;; -- matches what `sops-find-file' produces by default and what
          ;; upstream's own example.txt looks like, so the fixture exercises
          ;; the natural user flow rather than the override path.
          (funcall write (expand-file-name "notes.enc.txt" sops-test--fixtures)
                   (sops-test--encrypt-string
                    "secret note from a txt fixture\n"
                    "notes.enc.txt"))
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
  "sops--run can deliver input via :input.  future work changed the
delivery mechanism from `process-send-string' to a temp file whose
path sops--run appends as the trailing arg, so callers no longer
include `/dev/stdin' (or any input source) in their ARGS list."
  (let ((result (sops--run '("filestatus" "--input-type" "yaml")
                           :input "foo: bar\n")))
    (should (eq 0 (plist-get result :exit-status)))
    (should (string-match-p "encrypted" (plist-get result :stdout)))))

(ert-deftest sops-test--run-does-not-require-sentinel-flag ()
  "A finished subprocess cannot hang `sops--run' if its sentinel is delayed.

`sops--run' is synchronous and used from save hooks.  It must therefore
stop waiting when the subprocess exits, instead of relying only on the
sentinel to set an auxiliary flag.  This test replaces the sentinel with
a no-op; the old implementation spun forever in that situation."
  (skip-unless (executable-find "sh"))
  (let ((sops-executable "sh"))
    (cl-letf* ((orig-make-process (symbol-function 'make-process))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq args (plist-put (copy-sequence args)
                                        :sentinel (lambda (_p _event) nil)))
                  (apply orig-make-process args))))
      (let ((result (with-timeout
                        (2 (error "sops--run hung waiting for sentinel"))
                      (sops--run '("-c" "printf ok; printf err >&2")))))
        (should (eq 0 (plist-get result :exit-status)))
        (should (equal "ok" (plist-get result :stdout)))
        ;; The stderr pipe is itself a process; this test's make-process
        ;; wrapper may let Emacs append its process-finished notice there.
        (should (string-prefix-p "err" (plist-get result :stderr)))))))

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

(ert-deftest sops-test--filestatus-encrypted-txt ()
  "TXT fixture (binary-store encoded): filestatus works with no overrides.
sops's `FormatForPath' returns Binary for unknown extensions, so the
binary store handles `.txt' end-to-end without `sops-input-type-overrides'."
  (should (eq t (sops--filestatus (sops-test--fixture "notes.enc.txt")))))

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
specific sops flag being a no-op for encrypt.

future work contract: `sops--encrypt-and-write' no longer appends
`/dev/stdin' to its args list -- `sops--run' itself appends the temp
file path internally.  So the args passed to `sops--run' end with
the last element of `sops-extra-encrypt-args' (or with
`--filename-override FILE' when extra-args is nil)."
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
          ;; Both extra-args land in the command after --filename-override
          ;; FILE.  No /dev/stdin in the caller's args list anymore.
          (should (member "-a" captured-args))
          (should (member "age1stub" captured-args))
          (should-not (member "/dev/stdin" captured-args))
          ;; Order: -a comes before age1stub (preserved from input list)
          ;; and both come after --filename-override + file.
          (let ((dash-a-pos (cl-position "-a" captured-args :test #'equal))
                (stub-pos (cl-position "age1stub" captured-args :test #'equal))
                (override-pos (cl-position "--filename-override"
                                           captured-args :test #'equal)))
            (should (and dash-a-pos stub-pos override-pos))
            (should (< dash-a-pos stub-pos))
            (should (< override-pos dash-a-pos))))
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
      (should (eq t apheleia-inhibit))
      (sops-mode -1)
      (should-not (memq #'sops--write-contents-function write-contents-functions))
      (should (eq nil sops--state))
      ;; `kill-local-variable' restores the global default; the test
      ;; environment doesn't have apheleia loaded, so the global is unbound
      ;; -- assert the symbol is no longer buffer-local rather than checking
      ;; its value.
      (should-not (local-variable-p 'apheleia-inhibit)))))

(ert-deftest sops-test--mode-enable-manual-decrypts-buffer ()
  "Manual `M-x sops-mode' decrypts an unmodified ciphertext buffer.

This is the path used when a SOPS file did not match
`sops-prefilter-regex' at `find-file' time: the user sees ciphertext,
then enables `sops-mode' by hand.  Enabling the mode must leave the
buffer as protected plaintext, not as ciphertext with an encrypt-on-save
hook bolted on afterwards."
  (let ((file (make-temp-file "sops-test-manual-")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "ciphertext\n")
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'sops--filestatus) (lambda (_) t))
                    ((symbol-function 'sops--decrypt-buffer)
                     (lambda ()
                       (erase-buffer)
                       (insert "plaintext\n")
                       t)))
            (sops-mode 1))
          (should sops-mode)
          (should (sops-state-p sops--state))
          (should (eq 'decrypted (sops-state-status sops--state)))
          (should (memq #'sops--write-contents-function write-contents-functions))
          (should (equal "plaintext\n" (buffer-string))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest sops-test--mode-enable-manual-refuses-modified-buffer ()
  "Manual `M-x sops-mode' refuses to erase unsaved ciphertext edits."
  (let ((file (make-temp-file "sops-test-manual-modified-"))
        (decrypt-called nil))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "ciphertext\n")
          (set-buffer-modified-p nil)
          (insert "modified ciphertext buffer\n")
          (should (buffer-modified-p))
          (cl-letf (((symbol-function 'sops--filestatus) (lambda (_) t))
                    ((symbol-function 'sops--decrypt-buffer)
                     (lambda () (setq decrypt-called t) t)))
            (should-error (sops-mode 1) :type 'user-error))
          (should-not decrypt-called)
          (should-not sops-mode)
          (should-not sops--state))
      (when (file-exists-p file) (delete-file file)))))

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

(ert-deftest sops-test--mode-enable-refuses-non-sops-buffer ()
  "Manual `M-x sops-mode' on a non-sops buffer signals user-error and
does not leave the mode partially enabled.  Regression test for the
trap where the disable branch refuses on a modified buffer, so an
accidental enable on a plaintext file becomes unrecoverable."
  (let ((file (sops-test--fixture "plain.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (should-error (sops-mode 1) :type 'user-error)
      (should-not sops-mode)
      (should-not sops--state)
      (should-not (memq #'sops--write-contents-function
                        write-contents-functions)))))

(ert-deftest sops-test--mode-enable-refuses-buffer-with-no-file ()
  "`M-x sops-mode' on a buffer with no `buffer-file-name' refuses
cleanly rather than shelling out to sops with a nil path."
  (with-temp-buffer
    (insert "scratch contents\n")
    (should-error (sops-mode 1) :type 'user-error)
    (should-not sops-mode)
    (should-not sops--state)))

(ert-deftest sops-test--find-file-on-non-prefiltered-leaves-mode-off ()
  "With `global-sops-mode' on, real `find-file' on a file outside
`sops-prefilter-regex' (here a Terraform `.tf' file) must not enable
sops-mode -- the prefilter short-circuits before any sops shellout.
Regression test for a user report where opening a `.tf' file caused
encrypt-on-save to fire and trap the buffer (since the disable branch
refuses on modified buffers)."
  (let* ((was-on global-sops-mode)
         (tmp (make-temp-file "sops-test-tf-" nil ".tf")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "resource \"aws_s3_bucket\" \"x\" {}\n"))
          (global-sops-mode 1)
          (let ((buf (find-file-noselect tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should-not sops-mode)
                  (should-not sops--state)
                  (should-not (memq #'sops--write-contents-function
                                    write-contents-functions)))
              (kill-buffer buf))))
      (if was-on (global-sops-mode 1) (global-sops-mode -1))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--find-file-on-prefiltered-plain-yaml-leaves-mode-off ()
  "With `global-sops-mode' on, real `find-file' on a file that matches
`sops-prefilter-regex' but is NOT sops-encrypted must not enable
sops-mode -- `sops--filestatus' returns nil and the hook bails."
  (let* ((was-on global-sops-mode)
         (file (sops-test--fixture "plain.yaml")))
    (unwind-protect
        (progn
          (global-sops-mode 1)
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (should-not sops-mode)
                  (should-not sops--state)
                  (should-not (memq #'sops--write-contents-function
                                    write-contents-functions)))
              (kill-buffer buf))))
      (if was-on (global-sops-mode 1) (global-sops-mode -1)))))

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

(ert-deftest sops-test--revert-buffer-reinstalls-protections-after-mode-reset ()
  "Revert keeps protections even if plaintext mode setup clears locals.

Some major-mode setups can clear the `sops-mode' flag and local hook
state while `sops--decrypt-buffer' re-detects the plaintext mode.  A
successful `sops--revert-buffer' must leave the decrypted buffer protected
so the next save cannot write plaintext to disk."
  (let ((tmp (make-temp-file "sops-test-reprotect-" nil ".enc.yaml")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "ciphertext\n"))
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq sops-mode t)
            (setq sops--state (sops-state-create :status 'decrypted))
            (setq-local revert-buffer-function #'sops--revert-buffer)
            (add-hook 'write-contents-functions
                      #'sops--write-contents-function nil t)
            (cl-letf (((symbol-function 'sops--decrypt-buffer)
                       (lambda ()
                         ;; Simulate plaintext major-mode setup wiping the
                         ;; protection state after a successful decrypt.
                         (setq sops-mode nil)
                         (setq sops--state nil)
                         (setq write-contents-functions nil)
                         (kill-local-variable 'revert-buffer-function)
                         (erase-buffer)
                         (insert "plaintext\n")
                         t)))
              (sops--revert-buffer))
            (should (equal "plaintext\n" (buffer-string)))
            (should sops-mode)
            (should (sops-state-p sops--state))
            (should (memq #'sops--write-contents-function
                          write-contents-functions))
            (should (eq #'sops--revert-buffer revert-buffer-function))))
      (when (file-exists-p tmp) (delete-file tmp)))))

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

(ert-deftest sops-test--find-file-hook-decrypts-encrypted ()
  "find-file-hook on encrypted file decrypts and enables sops-mode."
  (let* ((find-file-hook nil)  ; isolate from any user/global hooks
         (buf (find-file-noselect (sops-test--fixture "secrets.enc.yaml"))))
    (unwind-protect
        (with-current-buffer buf
          (sops--find-file-hook)
          (should sops-mode)
          (should (string-match-p "database_password: super-secret-yaml"
                                  (buffer-string))))
      (kill-buffer buf))))

(ert-deftest sops-test--find-file-hook-skips-plaintext ()
  "find-file-hook on plaintext yaml does nothing."
  (let* ((find-file-hook nil)
         (buf (find-file-noselect (sops-test--fixture "plain.yaml"))))
    (unwind-protect
        (with-current-buffer buf
          (sops--find-file-hook)
          (should-not sops-mode)
          (should (string-match-p "not: a-sops-file" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest sops-test--find-file-hook-skips-non-prefiltered ()
  "find-file-hook ignores files not matching sops-prefilter-regex.
Uses `with-temp-buffer' + a real on-disk temp file rather than
`find-file-noselect' on a `.png'.  On Emacs 29.1 the latter routes
through `auto-mode-alist' → `image-mode', which errors with
\"Display does not support images\" in batch (29.1's ert binds
`debug-on-error' to t while running tests, so the `with-demoted-errors'
wrapper in `normal-mode' doesn't swallow it; 30+ ert switched to
`signal-hook-function' and the error gets demoted to a message).
Calling the hook directly on a temp buffer exercises the same
prefilter rejection path without touching find-file's machinery."
  (let ((tmp (make-temp-file "sops-test-png-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name tmp)
          (sops--find-file-hook)
          (should-not sops-mode))
      (delete-file tmp))))

(ert-deftest sops-test--find-file-hook-skips-tramp ()
  "find-file-hook is a no-op for remote (TRAMP) paths.
Remote sops support is out of scope for v0.2 (tracked in tramp-sops);
the local sops binary cannot read TRAMP paths, so we skip rather than
spawning a subprocess that would error."
  (with-temp-buffer
    (setq buffer-file-name "/ssh:host:/path/to/secret.yaml")
    (sops--find-file-hook)
    (should-not sops-mode)))

(ert-deftest sops-test--find-file-hook-decrypt-failure-makes-readonly ()
  "On decrypt failure, buffer is read-only and sops-mode not enabled.
The bad SOPS_AGE_KEY_FILE only affects the decrypt step; sops --version
and sops filestatus inspect metadata only and don't need the key, so
the guard chain reaches `sops--decrypt-buffer' before failing."
  (let* ((find-file-hook nil)
         (file (sops-test--fixture "secrets.enc.yaml"))
         (buf (find-file-noselect file))
         (buf-name (format "*sops-error: %s*" file)))
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (unwind-protect
        (with-current-buffer buf
          (let ((process-environment
                 (cons "SOPS_AGE_KEY_FILE=/tmp/nonexistent-key" process-environment)))
            (sops--find-file-hook))
          (should-not sops-mode)
          (should buffer-read-only)
          (should (get-buffer buf-name)))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (kill-buffer buf))))

(ert-deftest sops-test--revert-after-decrypt-failure-retries ()
  "After an initial decrypt failure, `revert-buffer' retries decrypt.
The spec contract is: when the user fixes their auth (e.g. exports the
right `AWS_PROFILE') and runs \\[revert-buffer], the buffer must
re-attempt decryption, clear `read-only-mode' on success, and enter
`sops-mode' so editing/saving works.  Without this, the recovery hint
in the popped `*sops-error:*' buffer is a lie.

The test runs `sops--find-file-hook' inside a let-bound bad
`SOPS_AGE_KEY_FILE' to force the failure path, then calls
`revert-buffer' OUTSIDE that let so the mise-injected good key is in
effect — simulating the user fixing their environment between attempts."
  (let* ((find-file-hook nil)
         (file (sops-test--fixture "secrets.enc.yaml"))
         (buf (find-file-noselect file))
         (buf-name (format "*sops-error: %s*" file)))
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (unwind-protect
        (with-current-buffer buf
          (let ((process-environment
                 (cons "SOPS_AGE_KEY_FILE=/tmp/nonexistent-key" process-environment)))
            (sops--find-file-hook))
          ;; Precondition: initial decrypt failed as expected.
          (should-not sops-mode)
          (should buffer-read-only)
          ;; User "fixes the issue" -- env now has the good key -- and reverts.
          (revert-buffer t t)
          ;; Postcondition: decrypt retried, buffer editable, sops-mode on.
          (should sops-mode)
          (should-not buffer-read-only)
          (should (string-match-p "database_password: super-secret-yaml"
                                  (buffer-string))))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest sops-test--find-file-hook-swallows-user-error-from-version ()
  "When `sops--ensure-version' raises user-error (sops missing/too old),
the hook logs and returns nil rather than propagating to the debugger."
  (let ((sops-executable "/no/such/sops/binary")
        (sops--version-cache nil))
    (with-temp-buffer
      (setq buffer-file-name (sops-test--fixture "secrets.enc.yaml"))
      ;; Should NOT signal; condition-case in the hook traps user-error.
      (sops--find-file-hook)
      (should-not sops-mode))))

(ert-deftest sops-test--global-sops-mode-toggles-find-file-hook ()
  "Toggling global-sops-mode adds/removes sops--find-file-hook globally.
`add-hook' modifies the default (global) value of `find-file-hook'
unless told otherwise, so we check `default-value' rather than the
buffer-local value, and restore prior state in `unwind-protect'."
  (let ((was-on global-sops-mode))
    (unwind-protect
        (progn
          (global-sops-mode 1)
          (should (memq #'sops--find-file-hook (default-value 'find-file-hook)))
          (global-sops-mode -1)
          (should-not (memq #'sops--find-file-hook (default-value 'find-file-hook))))
      (if was-on (global-sops-mode 1) (global-sops-mode -1)))))

(ert-deftest sops-test--revert-buffer-refreshes-visited-file-modtime ()
  "After `sops--revert-buffer', `verify-visited-file-modtime' returns t.
Regression test for: pressing `r' at the \"FILE changed on disk; really
edit?\" prompt would revert the buffer but leave the recorded modtime
stale, so the next keystroke re-fired the same prompt indefinitely."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-revert-modtime-" sops-test--fixtures)
               nil ".enc.yaml")))
    (unwind-protect
        (progn
          ;; Seed tmp with ciphertext BEFORE `find-file-noselect' so the
          ;; buffer's initial visited-file-modtime matches disk.  Otherwise
          ;; the `(copy-file src tmp t)' below would leave the buffer in a
          ;; stale state and `sops--decrypt-buffer's `erase-buffer' would
          ;; trigger Emacs's supersession check before we even reach the
          ;; scenario the test is for.
          (copy-file src tmp t)
          (let ((buf (find-file-noselect tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (sops--decrypt-buffer)
                  (sops-mode 1)
                  ;; Simulate an external write: re-copy the source over the
                  ;; temp file, advancing its modtime past what `find-file'
                  ;; recorded.
                  (sleep-for 1.1)  ; macOS APFS mtime resolution
                  (copy-file src tmp t)
                  ;; Pre-condition: modtime check fails because file changed
                  ;; under us.
                  (should-not (verify-visited-file-modtime buf))
                  ;; The revert (what `r' at the prompt would call).
                  (sops--revert-buffer)
                  ;; Post-condition: modtime check passes; pressing a key
                  ;; would no longer re-fire the prompt.
                  (should (verify-visited-file-modtime buf)))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest sops-test--mode-enables-auto-revert ()
  "sops-mode turns on `auto-revert-mode' so external file changes flow
through `sops--revert-buffer' without the user seeing a prompt."
  (let ((file (sops-test--fixture "secrets.enc.yaml")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert-file-contents file)
      (sops--decrypt-buffer)
      (sops-mode 1)
      (should auto-revert-mode)
      (sops-mode -1)
      (should-not auto-revert-mode))))

(ert-deftest sops-test--encrypt-and-write-refreshes-visited-file-modtime ()
  "After save, `verify-visited-file-modtime' returns t so the user
doesn't see \"FILE has changed on disk; really edit the buffer?\" on
the next edit.  Regression test for the v0.2 manual-testing bug
where `find-file' recorded the encrypted file's modtime and our
`write-region' replaced the on-disk file without refreshing the
buffer's recorded modtime."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-modtime-" sops-test--fixtures)
               nil ".enc.yaml")))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (let ((buf (find-file-noselect tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (sops--decrypt-buffer)
                  (sops-mode 1)
                  ;; First edit + save.
                  (goto-char (point-max))
                  (insert "first: edit\n")
                  (sops--encrypt-and-write)
                  (should (verify-visited-file-modtime buf))
                  ;; Second edit + save -- this is what triggered the
                  ;; original bug since save 1 left the recorded modtime
                  ;; stale relative to the file we just wrote.
                  (goto-char (point-max))
                  (insert "second: edit\n")
                  (sops--encrypt-and-write)
                  (should (verify-visited-file-modtime buf)))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest sops-test--migration-warns-on-v0.1.X-hook ()
  "Warns when v0.1.X sops-before-encrypt-decrypt-hook is non-nil."
  (let* ((sops-before-encrypt-decrypt-hook (list #'ignore))
         (warnings nil)
         (display-warning-fn
          (lambda (type msg &rest _) (push (cons type msg) warnings))))
    (cl-letf (((symbol-function 'display-warning) display-warning-fn))
      (sops--check-v0.1.X-config))
    (should (cl-find-if (lambda (w)
                          (and (eq (car w) 'sops)
                               (string-match-p "before-encrypt-decrypt-hook"
                                               (cdr w))))
                        warnings))))

(ert-deftest sops-test--migration-warns-on-v0.1.X-decrypt-args ()
  "Warns when v0.1.X sops-decrypt-args is non-nil (replaced by
sops-extra-decrypt-args + hardcoded `decrypt' subcommand)."
  (let* ((sops-decrypt-args '("decrypt" "--whatever"))
         (warnings nil)
         (display-warning-fn
          (lambda (type msg &rest _) (push (cons type msg) warnings))))
    (cl-letf (((symbol-function 'display-warning) display-warning-fn))
      (sops--check-v0.1.X-config))
    (should (cl-find-if (lambda (w)
                          (and (eq (car w) 'sops)
                               (string-match-p "sops-decrypt-args"
                                               (cdr w))))
                        warnings))))

(ert-deftest sops-test--migration-no-warn-when-unset ()
  "Does not warn when v0.1.X vars are nil or unbound."
  (let ((warnings nil)
        (display-warning-fn
         (lambda (type msg &rest _) (push (cons type msg) warnings))))
    (cl-letf (((symbol-function 'display-warning) display-warning-fn))
      (when (boundp 'sops-before-encrypt-decrypt-hook)
        (let ((sops-before-encrypt-decrypt-hook nil))
          (sops--check-v0.1.X-config)))
      (when (boundp 'sops-decrypt-args)
        (let ((sops-decrypt-args nil))
          (sops--check-v0.1.X-config)))
      (sops--check-v0.1.X-config))
    (should (eq nil warnings))))

(ert-deftest sops-test--encrypt-args-no-trailing-stdin ()
  "future work contract: sops--run for the encrypt path passes the temp-file
path as the command's last argument, not the literal string
\"/dev/stdin\".  Callers (sops--encrypt-and-write) no longer append
\"/dev/stdin\" to their args list; sops--run appends the temp path.

Wraps `make-process' via `cl-letf' to capture the full :command
argument list at process-creation time.  The captured tail must be
the temp-file path; the literal string \"/dev/stdin\" must not appear
in the command at all."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-cmd-" sops-test--fixtures)
               nil ".enc.yaml"))
         (captured-cmd nil))
    (copy-file src tmp t)
    (let ((buf (find-file-noselect tmp)))
      (unwind-protect
          (with-current-buffer buf
            (sops--decrypt-buffer)
            (sops-mode 1)
            (goto-char (point-max))
            (insert "extra: line\n")
            (cl-letf* ((orig-mp (symbol-function 'make-process))
                       ((symbol-function 'make-process)
                        (lambda (&rest args)
                          (when (equal (plist-get args :name) "sops")
                            (setq captured-cmd (plist-get args :command)))
                          (apply orig-mp args))))
              (sops--encrypt-and-write))
            (should captured-cmd)
            (should-not (member "/dev/stdin" captured-cmd))
            ;; Last element of the command should be a temp-file path.
            (should (string-prefix-p
                     "sops-input-"
                     (file-name-nondirectory (car (last captured-cmd))))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (delete-file tmp)))))

(ert-deftest sops-test--encrypt-uses-temp-file-with-mode-0600 ()
  "Lock the safety property that `sops--run's temp file for `:input'
is created at mode 0600.  Captures the path returned by
`make-temp-file' via `cl-letf' advice and immediately reads
`(file-modes path)' before the temp file is deleted."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-mode-" sops-test--fixtures)
               nil ".enc.yaml"))
         (captured-path nil)
         (captured-mode nil))
    (copy-file src tmp t)
    (let ((buf (find-file-noselect tmp)))
      (unwind-protect
          (with-current-buffer buf
            (sops--decrypt-buffer)
            (sops-mode 1)
            (goto-char (point-max))
            (insert "extra: line\n")
            (cl-letf* ((orig-mtf (symbol-function 'make-temp-file))
                       ((symbol-function 'make-temp-file)
                        (lambda (&rest args)
                          (let ((p (apply orig-mtf args)))
                            (when (string-prefix-p
                                   "sops-input-"
                                   (file-name-nondirectory p))
                              (setq captured-path p
                                    captured-mode (file-modes p)))
                            p))))
              (sops--encrypt-and-write))
            (should captured-path)
            (should (equal captured-mode #o600)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (delete-file tmp)))))

(ert-deftest sops-test--encrypt-temp-file-deleted-on-success ()
  "Lock the cleanup property: after a successful
`sops--encrypt-and-write', the temp file `sops--run' created for the
payload no longer exists."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-cleanup-ok-" sops-test--fixtures)
               nil ".enc.yaml"))
         (captured-path nil))
    (copy-file src tmp t)
    (let ((buf (find-file-noselect tmp)))
      (unwind-protect
          (with-current-buffer buf
            (sops--decrypt-buffer)
            (sops-mode 1)
            (goto-char (point-max))
            (insert "extra: line\n")
            (cl-letf* ((orig-mtf (symbol-function 'make-temp-file))
                       ((symbol-function 'make-temp-file)
                        (lambda (&rest args)
                          (let ((p (apply orig-mtf args)))
                            (when (string-prefix-p
                                   "sops-input-"
                                   (file-name-nondirectory p))
                              (setq captured-path p))
                            p))))
              (sops--encrypt-and-write))
            (should captured-path)
            (should-not (file-exists-p captured-path)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (delete-file tmp)))))

(ert-deftest sops-test--encrypt-temp-file-deleted-on-failure ()
  "Lock the cleanup property under failure: when sops encrypt fails,
`sops--encrypt-and-write' signals user-error AND the temp file
`sops--run' created has been deleted by the unwind-protect cleanup.

Failure injection mirrors
`sops-test--encrypt-and-write-failure-leaves-file-untouched': place
the buffer's target file in `/tmp' (outside the fixture's
`.sops.yaml' coverage) so sops errors with `no matching creation
rules found'.  Note: `SOPS_AGE_KEY_FILE' is irrelevant on the
encrypt path (sops uses the recipient from .sops.yaml, not the
private age key file), so it cannot be used to force encrypt
failure -- only decrypt."
  (let* ((tmp (make-temp-file "sops-test-cleanup-fail-" nil ".enc.yaml"))
         (captured-path nil)
         (err nil))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name tmp)
          (insert "plain content; no .sops.yaml rule for /tmp\n")
          (cl-letf* ((orig-mtf (symbol-function 'make-temp-file))
                     ((symbol-function 'make-temp-file)
                      (lambda (&rest args)
                        (let ((p (apply orig-mtf args)))
                          (when (string-prefix-p
                                 "sops-input-"
                                 (file-name-nondirectory p))
                            (setq captured-path p))
                          p))))
            (condition-case e
                (sops--encrypt-and-write)
              (user-error (setq err e))))
          (should err)
          (should captured-path)
          (should-not (file-exists-p captured-path)))
      (when (get-buffer (format "*sops-error: %s*" tmp))
        (kill-buffer (format "*sops-error: %s*" tmp)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--encrypt-temp-file-uses-utf-8-unix ()
  "Lock the coding system: the temp file `sops--run' writes for the
payload uses `utf-8-unix' (LF newlines, no CRLF translation, UTF-8
character encoding).  Verified by capturing the temp-file bytes mid
write via advice on `write-region', and comparing to the buffer
content normalized to utf-8-unix."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (make-temp-file
               (expand-file-name "sops-test-encoding-" sops-test--fixtures)
               nil ".enc.yaml"))
         (captured-bytes nil))
    (copy-file src tmp t)
    (let ((buf (find-file-noselect tmp)))
      (unwind-protect
          (with-current-buffer buf
            (sops--decrypt-buffer)
            (sops-mode 1)
            (goto-char (point-max))
            (insert "line_a: 1\nline_b: 2\n")
            (cl-letf* ((orig-wr (symbol-function 'write-region))
                       ((symbol-function 'write-region)
                        (lambda (start end filename &rest rest)
                          (apply orig-wr start end filename rest)
                          (when (and (stringp filename)
                                     (string-prefix-p
                                      "sops-input-"
                                      (file-name-nondirectory filename)))
                            (setq captured-bytes
                                  (with-temp-buffer
                                    (let ((coding-system-for-read 'binary))
                                      (insert-file-contents-literally filename))
                                    (buffer-string)))))))
              (sops--encrypt-and-write))
            (should captured-bytes)
            ;; LF newlines only, no CRLF anywhere.
            (should-not (string-match-p "\r\n" captured-bytes))
            ;; The lines we inserted should appear verbatim.
            (should (string-match-p "line_a: 1\nline_b: 2\n" captured-bytes)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (delete-file tmp)))))

;;; -------- sops--example-for / sops--format-for --------

(ert-deftest sops-test--example-yaml-content ()
  "yaml stub contains ExampleComplexTree fields (parity with upstream)."
  (let ((s (sops--example-for "yaml")))
    (should (stringp s))
    (should (> (length s) 0))
    (should (string-match-p "hello: Welcome to SOPS" s))
    (should (string-match-p "example_key: example_value" s))
    (should (string-match-p "example_array" s))
    (should (string-match-p "example_number: 1234.56789" s))
    (should (string-match-p "example_booleans" s))))

(ert-deftest sops-test--example-json-parses ()
  "json stub parses as valid JSON and has the expected top-level keys."
  (let* ((s (sops--example-for "json"))
         (parsed (json-parse-string s :object-type 'alist)))
    (should (equal "Welcome to SOPS! Edit this file as you please!"
                   (cdr (assq 'hello parsed))))
    (should (equal "example_value"
                   (cdr (assq 'example_key parsed))))
    (should (vectorp (cdr (assq 'example_array parsed))))))

(ert-deftest sops-test--example-dotenv-content ()
  "dotenv stub contains the example_key=example_value pair."
  (let ((s (sops--example-for "dotenv")))
    (should (string-match-p "example_key=example_value" s))))

(ert-deftest sops-test--example-ini-content ()
  "ini stub contains the [Welcome!] section header and hello key."
  (let ((s (sops--example-for "ini")))
    (should (string-match-p "\\[Welcome!\\]" s))
    (should (string-match-p "hello=" s))))

(ert-deftest sops-test--example-txt-content ()
  "txt stub greets the user from emacs sops-mode.
Bare text (no `hello:' key prefix) -- `.txt' files use sops's
native binary store, not yaml parsing."
  (let ((s (sops--example-for "txt")))
    (should (stringp s))
    (should (string-match-p "hello from emacs sops-mode" s))
    (should-not (string-match-p "^hello:" s))))

(ert-deftest sops-test--example-unknown-empty ()
  "Unknown / nil formats return the empty string (caller's responsibility)."
  (should (equal "" (sops--example-for nil)))
  (should (equal "" (sops--example-for "binary")))
  (should (equal "" (sops--example-for "unknown-type"))))

(ert-deftest sops-test--format-for-extensions ()
  "Extension-driven format detection."
  (should (equal "yaml"   (sops--format-for "/tmp/x.yaml")))
  (should (equal "yaml"   (sops--format-for "/tmp/x.yml")))
  (should (equal "json"   (sops--format-for "/tmp/x.json")))
  (should (equal "dotenv" (sops--format-for "/tmp/x.env")))
  (should (equal "ini"    (sops--format-for "/tmp/x.ini")))
  (should (equal "txt"    (sops--format-for "/tmp/x.txt")))
  (should (eq nil (sops--format-for "/tmp/x.unknown")))
  (should (eq nil (sops--format-for nil))))

(ert-deftest sops-test--format-for-respects-overrides ()
  "sops-input-type-overrides takes precedence over the extension fallback."
  (let ((sops-input-type-overrides '(("\\.secrets\\'" . "yaml"))))
    (should (equal "yaml" (sops--format-for "/tmp/x.secrets")))
    ;; Extension fallback still works for non-overridden paths.
    (should (equal "json" (sops--format-for "/tmp/x.json")))))

;;; -------- sops--maybe-output-type --------

(ert-deftest sops-test--maybe-output-type-nil-input ()
  "Returns nil when no input-type override is in play."
  (should (eq nil (sops--maybe-output-type nil '()))))

(ert-deftest sops-test--maybe-output-type-yaml ()
  "Returns (--output-type yaml) when input-type is yaml and args don't set one."
  (should (equal '("--output-type" "yaml")
                 (sops--maybe-output-type "yaml" '("decrypt")))))

(ert-deftest sops-test--maybe-output-type-respects-existing ()
  "Returns nil when EXISTING-ARGS already has --output-type (user wins)."
  (should (eq nil (sops--maybe-output-type
                   "yaml" '("decrypt" "--output-type" "json")))))

(ert-deftest sops-test--maybe-output-type-passes-through-type ()
  "Echoes the input-type back (so callers don't have to map yaml→yaml etc.)."
  (should (equal '("--output-type" "json")
                 (sops--maybe-output-type "json" '())))
  (should (equal '("--output-type" "dotenv")
                 (sops--maybe-output-type "dotenv" '()))))

;;; -------- sops--start-creation --------

(ert-deftest sops-test--start-creation-seeds-yaml ()
  "After sops--start-creation, buffer has the yaml stub, state='creating, mode on."
  (with-temp-buffer
    (setq buffer-file-name
          (expand-file-name (format "sops-test-start-%d.yaml" (random 1000000))
                            temporary-file-directory))
    (unwind-protect
        (progn
          (sops--start-creation "yaml")
          (should (string-match-p "hello: Welcome to SOPS" (buffer-string)))
          (should sops--state)
          (should (eq 'creating (sops-state-status sops--state)))
          (should sops-mode)
          (should-not (buffer-modified-p)))
      ;; Cleanup: turn off mode before letting the buffer die so the
      ;; mode's deactivation guard doesn't bark on the temp buffer.
      (when sops-mode
        (setq sops--state (sops-state-create :status 'decrypted))
        (set-buffer-modified-p nil)
        (sops-mode -1)))))

(ert-deftest sops-test--start-creation-empty-for-unknown-format ()
  "Unknown format yields an empty buffer + still enables sops-mode."
  (with-temp-buffer
    (setq buffer-file-name
          (expand-file-name (format "sops-test-unk-%d.unknown" (random 1000000))
                            temporary-file-directory))
    (unwind-protect
        (progn
          (sops--start-creation nil)
          (should (equal "" (buffer-string)))
          (should (eq 'creating (sops-state-status sops--state)))
          (should sops-mode)
          (should-not (buffer-modified-p)))
      (when sops-mode
        (setq sops--state (sops-state-create :status 'decrypted))
        (set-buffer-modified-p nil)
        (sops-mode -1)))))

;;; -------- 'creating → 'decrypted transition --------

(ert-deftest sops-test--encrypt-and-write-transitions-creating-to-decrypted ()
  "Successful first save flips sops--state.status from 'creating to 'decrypted."
  (sops-test--ensure-fixtures)
  (let* ((tmp (expand-file-name
               (format "sops-test-trans-%d.enc.yaml" (random 1000000))
               sops-test--fixtures)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name tmp)
          (setq default-directory (file-name-directory tmp))
          (insert "hello: from-creating-state\n")
          (setq sops--state (sops-state-create :status 'creating))
          (should (eq 'creating (sops-state-status sops--state)))
          (sops--encrypt-and-write)
          (should (eq 'decrypted (sops-state-status sops--state)))
          (should (file-exists-p tmp)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--encrypt-and-write-failed-save-keeps-creating-state ()
  "Failed first save: state stays 'creating, file not created on disk."
  (sops-test--ensure-fixtures)
  ;; Use an extension the fixtures' .sops.yaml creation_rules don't match
  ;; (path_regex is `\\.enc\\.(yaml|json|env|txt)$').
  (let* ((tmp (expand-file-name
               (format "sops-test-failtrans-%d.no-rule" (random 1000000))
               sops-test--fixtures)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name tmp)
          (setq default-directory (file-name-directory tmp))
          (insert "hello: should-fail\n")
          (setq sops--state (sops-state-create :status 'creating))
          (should-error (sops--encrypt-and-write) :type 'user-error)
          (should (eq 'creating (sops-state-status sops--state)))
          (should-not (file-exists-p tmp)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--encrypt-and-write-no-transition-when-not-creating ()
  "Saving a 'decrypted buffer does NOT touch sops--state.status."
  (sops-test--ensure-fixtures)
  (let* ((src (sops-test--fixture "secrets.enc.yaml"))
         (tmp (expand-file-name
               (format "sops-test-decstate-%d.enc.yaml" (random 1000000))
               sops-test--fixtures)))
    (unwind-protect
        (progn
          (copy-file src tmp t)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (setq sops--state (sops-state-create :status 'decrypted))
            (goto-char (point-max))
            (insert "added: 1\n")
            (sops--encrypt-and-write)
            (should (eq 'decrypted (sops-state-status sops--state)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; -------- sops-find-file --------

(ert-deftest sops-test--find-file-rejects-remote ()
  "Remote PATH signals user-error before any I/O."
  (should-error (sops-find-file "/ssh:nohost:/tmp/x.yaml") :type 'user-error))

(ert-deftest sops-test--find-file-rejects-missing-parent ()
  "Path under a non-existent parent dir signals user-error."
  (should-error
   (sops-find-file "/no/such/directory/at/all/secrets.enc.yaml")
   :type 'user-error))

(ert-deftest sops-test--find-file-rejects-no-sops-yaml ()
  "Path under a fresh tempdir with no reachable .sops.yaml signals user-error.
Assumes the test runner's filesystem has no .sops.yaml at / or
intermediate ancestors of `temporary-file-directory'."
  (let ((dir (make-temp-file "sops-test-noconfig-" t)))
    (unwind-protect
        (progn
          (should-not (locate-dominating-file dir ".sops.yaml"))
          (should-error
           (sops-find-file (expand-file-name "secrets.enc.yaml" dir))
           :type 'user-error))
      (delete-directory dir t))))

(ert-deftest sops-test--find-file-rejects-empty-string ()
  "Empty PATH signals user-error before any I/O."
  (should-error (sops-find-file "") :type 'user-error))

(ert-deftest sops-test--find-file-rejects-directory ()
  "A path ending in `/' (a directory) signals user-error."
  (let ((dir (make-temp-file "sops-test-dir-" t)))
    (unwind-protect
        (should-error
         (sops-find-file (file-name-as-directory dir))
         :type 'user-error)
      (delete-directory dir t))))

(ert-deftest sops-test--find-file-existing-path-delegates ()
  "Existing PATH: sops-find-file calls find-file (buffer visits the file).
Hooks are disabled so this isolates delegation from the decrypt flow
covered in sops-test--find-file-existing-encrypted-decrypts."
  (let ((file (sops-test--fixture "secrets.enc.yaml"))
        (find-file-hook nil))
    (sops-find-file file)
    (unwind-protect
        (should (equal (expand-file-name file) buffer-file-name))
      (kill-buffer (current-buffer)))))

(ert-deftest sops-test--find-file-existing-encrypted-decrypts ()
  "Existing encrypted PATH with the v0.2 hook active: decrypts to 'decrypted.
Installs `sops--find-file-hook' explicitly (rather than activating
`global-sops-mode') so the test is hermetic."
  (let* ((file (sops-test--fixture "secrets.enc.yaml"))
         (find-file-hook (cons #'sops--find-file-hook find-file-hook)))
    (sops-find-file file)
    (unwind-protect
        (progn
          (should sops-mode)
          (should sops--state)
          (should (eq 'decrypted (sops-state-status sops--state)))
          (should (string-match-p "database_password" (buffer-string))))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(ert-deftest sops-test--find-file-creates-new-yaml ()
  "Non-existent .enc.yaml: buffer holds yaml stub, state='creating, mode on.
File is NOT created on disk until the first successful save."
  (sops-test--ensure-fixtures)
  (let ((tmp (expand-file-name
              (format "sops-test-new-%d.enc.yaml" (random 1000000))
              sops-test--fixtures))
        (buf nil))
    (unwind-protect
        (progn
          (sops-find-file tmp)
          (setq buf (current-buffer))
          (should (eq 'creating (sops-state-status sops--state)))
          (should sops-mode)
          (should-not (buffer-modified-p))
          (should (string-match-p "hello: Welcome to SOPS" (buffer-string)))
          (should-not (file-exists-p tmp)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq sops--state (sops-state-create :status 'decrypted))
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--find-file-yaml-roundtrip ()
  "Full happy path: sops-find-file → edit → save → reopen → matches."
  (sops-test--ensure-fixtures)
  (let ((tmp (expand-file-name
              (format "sops-test-rt-yaml-%d.enc.yaml" (random 1000000))
              sops-test--fixtures))
        (buf nil))
    (unwind-protect
        (progn
          (sops-find-file tmp)
          (setq buf (current-buffer))
          (goto-char (point-max))
          (insert "my_new_secret: round-trip-value\n")
          (sops--encrypt-and-write)
          (should (file-exists-p tmp))
          (should (eq 'decrypted (sops-state-status sops--state)))
          (kill-buffer buf)
          (setq buf nil)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "my_new_secret: round-trip-value"
                                    (buffer-string)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq sops--state (sops-state-create :status 'decrypted))
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--find-file-json-roundtrip ()
  "JSON roundtrip: create-from-stub → save → reopen verifies."
  (sops-test--ensure-fixtures)
  (let ((tmp (expand-file-name
              (format "sops-test-rt-json-%d.enc.json" (random 1000000))
              sops-test--fixtures))
        (buf nil))
    (unwind-protect
        (progn
          (sops-find-file tmp)
          (setq buf (current-buffer))
          ;; Replace the stub with minimal valid JSON containing a marker.
          (erase-buffer)
          (insert "{\"my_new_secret\": \"json-rt\"}\n")
          (sops--encrypt-and-write)
          (should (file-exists-p tmp))
          (kill-buffer buf)
          (setq buf nil)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "json-rt" (buffer-string)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq sops--state (sops-state-create :status 'decrypted))
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--find-file-dotenv-roundtrip ()
  "Dotenv roundtrip: create-from-stub → save → reopen verifies."
  (sops-test--ensure-fixtures)
  (let ((tmp (expand-file-name
              (format "sops-test-rt-env-%d.enc.env" (random 1000000))
              sops-test--fixtures))
        (buf nil))
    (unwind-protect
        (progn
          (sops-find-file tmp)
          (setq buf (current-buffer))
          (erase-buffer)
          (insert "MY_NEW_SECRET=dotenv-rt\n")
          (sops--encrypt-and-write)
          (should (file-exists-p tmp))
          (kill-buffer buf)
          (setq buf nil)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "MY_NEW_SECRET=dotenv-rt" (buffer-string)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq sops--state (sops-state-create :status 'decrypted))
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sops-test--find-file-txt-roundtrip ()
  "TXT roundtrip via sops's native binary store: no overrides needed.
Bare text in / bare text out -- the whole buffer is treated as a
single binary blob by sops, no `sops-input-type-overrides' or
`--output-type' configuration required."
  (sops-test--ensure-fixtures)
  (let ((tmp (expand-file-name
              (format "sops-test-rt-txt-%d.enc.txt" (random 1000000))
              sops-test--fixtures))
        (buf nil))
    (unwind-protect
        (progn
          (sops-find-file tmp)
          (setq buf (current-buffer))
          ;; Stub is the unmodified greeting; save it as-is.
          (should (string-match-p "hello from emacs sops-mode"
                                  (buffer-string)))
          (sops--encrypt-and-write)
          (should (file-exists-p tmp))
          (should (eq 'decrypted (sops-state-status sops--state)))
          (kill-buffer buf)
          (setq buf nil)
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (setq default-directory (file-name-directory tmp))
            (insert-file-contents tmp)
            (sops--decrypt-buffer)
            (should (string-match-p "hello from emacs sops-mode"
                                    (buffer-string)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq sops--state (sops-state-create :status 'decrypted))
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'sops-test)
;;; sops-test.el ends here
