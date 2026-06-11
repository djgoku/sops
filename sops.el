;;; sops.el --- Edit SOPS-encrypted files transparently  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>

;; Author:  Jonathan Carroll Otsuka <pitas.axioms0c@icloud.com>
;; Keywords: convenience files tools sops encrypt decrypt
;; Version: 0.2.0
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
  ;; status:
  ;;   'decrypted -- set by sops--find-file-hook after a successful decrypt,
  ;;                 or by sops--encrypt-and-write after the first save of a
  ;;                 'creating buffer; the buffer represents an on-disk
  ;;                 ciphertext file the user is editing in plaintext.
  ;;   'creating  -- set by sops--start-creation when sops-find-file visits
  ;;                 a non-existent path; the on-disk file does not exist
  ;;                 yet and is created by the first encrypt-and-write.
  status
  last-error)   ; nil or string with most recent sops stderr

(defvar-local sops--state nil
  "An `sops-state' struct for the current buffer, or nil if sops-mode is off.")

(defvar sops--restore-hook-installed nil
  "Non-nil once the major-mode-change restoration hook is installed.
Set on first `sops-mode' enable so a bare `(require \\='sops)' adds no
global hook; see `sops--restore-after-major-mode-change'.")

(defgroup sops nil
  "Edit SOPS-encrypted files transparently."
  :group 'convenience
  :prefix "sops-")

(defcustom sops-prefilter-regex
  "\\.\\(ya?ml\\|json\\|env\\|ini\\|txt\\)\\'"
  "Filename regex.  Files matching trigger `sops filestatus' on `find-file'.
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
  "Return input-type for FILENAME from `sops-input-type-overrides', or nil.
Pairs in the alist are tried in list order; the first match wins."
  (when filename
    (cdr (cl-find-if (lambda (pair) (string-match-p (car pair) filename))
                     sops-input-type-overrides))))

(defconst sops--example-yaml
  "hello: Welcome to SOPS! Edit this file as you please!
example_key: example_value
# Example comment
example_array:
    - example_value1
    - example_value2
example_number: 1234.56789
example_booleans:
    - true
    - false
"
  "YAML stub seeded into new buffers by `sops-find-file'.
Mirrors upstream sops's `ExampleComplexTree' rendering
\(see `stores/yaml/store.go:EmitExample').")

(defconst sops--example-json
  "{
    \"hello\": \"Welcome to SOPS! Edit this file as you please!\",
    \"example_key\": \"example_value\",
    \"example_array\": [
        \"example_value1\",
        \"example_value2\"
    ],
    \"example_number\": 1234.56789,
    \"example_booleans\": [
        true,
        false
    ]
}
"
  "JSON stub seeded into new buffers by `sops-find-file'.
Mirrors upstream sops's `ExampleComplexTree' rendering
\(see `stores/json/store.go:EmitExample').")

(defconst sops--example-dotenv
  "# Welcome to SOPS! Edit this file as you please!
example_key=example_value
"
  "Dotenv stub seeded into new buffers by `sops-find-file'.
Mirrors upstream sops's dotenv example rendering
\(see `stores/dotenv/store.go:EmitExample').")

(defconst sops--example-ini
  "[Welcome!]
; This is an example file.
hello=Welcome to SOPS! Edit this file as you please!
example_key=example_value
"
  "INI stub seeded into new buffers by `sops-find-file'.
Mirrors upstream sops's `ExampleSimpleTree' rendering
\(see `stores/ini/store.go:EmitExample').")

(defconst sops--example-txt
  "hello from emacs sops-mode!
"
  "Plain-text stub seeded into new `.txt' buffers by `sops-find-file'.
`.txt' files use sops's native binary store: the whole buffer is
encrypted as a single blob (not parsed as yaml/json/etc.), so no
`sops-input-type-overrides' configuration is required by default.

If you prefer structured content for `.txt' (yaml-shaped keys with
selective encryption), map the path to \"yaml\" in
`sops-input-type-overrides'; sops-mode then auto-threads
`--output-type yaml' (via `sops--maybe-output-type') to work around
upstream sops's binary-store fallback (see getsops/sops#235).")

(defun sops--example-for (format)
  "Return the stub string for FORMAT, or \"\" if FORMAT is nil/unknown.
FORMAT is a key string: \"yaml\", \"json\", \"dotenv\", \"ini\", or
\"txt\".  The first four match sops's native `--input-type' values
verbatim; \"txt\" is a sops-mode-only key for plain-text files,
which sops itself doesn't parse -- callers should pair `.txt' paths
with a `sops-input-type-overrides' entry mapping to a real parser.
Any other value (including nil, \"binary\", arbitrary strings)
returns the empty string."
  (pcase format
    ("yaml"   sops--example-yaml)
    ("json"   sops--example-json)
    ("dotenv" sops--example-dotenv)
    ("ini"    sops--example-ini)
    ("txt"    sops--example-txt)
    (_        "")))

(defun sops--format-for (filename)
  "Return the `sops--example-for' key for FILENAME, or nil if unknown.
Standard extensions (\"yaml\"/\"yml\"/\"json\"/\"env\"/\"ini\"/\"txt\") win
first so a `.txt' file always picks the txt greeting stub, even when the
user also has a `sops-input-type-overrides' entry mapping the path
to a sops parser (e.g. for encrypt to succeed).  For non-standard
extensions like `.secrets', `sops-input-type-overrides' is the
fallback, mapping the path to one of the recognized stub keys.

Returns one of \"yaml\", \"json\", \"dotenv\", \"ini\", \"txt\", or
nil.  The first four are also valid sops `--input-type' values;
\"txt\" is sops-mode-only -- sops itself doesn't parse a `txt'
input-type, so `.txt' creators must configure
`sops-input-type-overrides' (e.g. `(\"\\\\.enc\\\\.txt\\\\'\" . \"yaml\")') for
encrypt to succeed."
  (let ((ext (and filename (file-name-extension filename))))
    (or (pcase ext
          ((or "yaml" "yml") "yaml")
          ("json"            "json")
          ("env"             "dotenv")
          ("ini"             "ini")
          ("txt"             "txt"))
        (sops--input-type-for filename))))

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
  "Run sops with ARGS.  KEYS is a plist of keyword options.
Keyword args:
  :input STRING  -- write STRING to a temp file (mode 0600) and pass
                    its path to sops as the trailing argument.  The
                    temp file is deleted in the `unwind-protect' cleanup.
                    Matches Emacs core's EPG/EPA convention: stdin is
                    reserved for prompt responses; payload bytes go
                    via the file path.
  :filter FN     -- process filter (nil in v0.2; populated in future work)
Return plist (:exit-status N :stdout STR :stderr STR)."
  (let* ((input (plist-get keys :input))
         (filter (plist-get keys :filter))
         (stdout-buf (generate-new-buffer " *sops-stdout*" t))
         (stderr-buf (generate-new-buffer " *sops-stderr*" t))
         (input-file (when input
                       (with-file-modes #o600
                         (make-temp-file "sops-input-"))))
         (done nil)
         (proc nil))
    (unwind-protect
        (let ((process-environment
               (cons "SOPS_DISABLE_VERSION_CHECK=true" process-environment)))
          (when input
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region input nil input-file nil 'silent)))
          (setq proc
                (make-process
                 :name "sops"
                 :buffer stdout-buf
                 :stderr stderr-buf
                 :command (cons sops-executable
                                (if input-file
                                    (append args (list input-file))
                                  args))
                 :connection-type 'pipe
                 :filter filter
                 :sentinel (lambda (_p _event) (setq done t))))
          (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
          (while (and (not done) (process-live-p proc))
            (accept-process-output proc 0.1))
          (accept-process-output proc 0 nil t)
          (list :exit-status (process-exit-status proc)
                :stdout (with-current-buffer stdout-buf (buffer-string))
                :stderr (with-current-buffer stderr-buf (buffer-string))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
      (when (and input-file (file-exists-p input-file))
        (delete-file input-file)))))

(defvar sops--version-cache nil
  "Cons of (PATH . VERSION-STRING) cached after first successful sops --version.
Recomputed when `sops-executable' changes or path is otherwise different.")

(defun sops--ensure-version ()
  "Verify sops binary exists and is >= 3.9.0.  Return version string.
Signals `user-error' if sops is missing or too old.  Caches result."
  (when (or (null sops--version-cache)
            (not (equal (car sops--version-cache) sops-executable)))
    (unless (executable-find sops-executable)
      (user-error "Sops: executable not found: %s" sops-executable))
    (let* ((result (sops--run '("--version")))
           (out (plist-get result :stdout))
           (version (when (string-match "[0-9]+\\.[0-9]+\\.[0-9]+" out)
                      (match-string 0 out))))
      (unless (and version (version<= "3.9.0" version))
        (user-error "Sops: requires >= 3.9.0, found %s"
                    (or version "unknown")))
      (setq sops--version-cache (cons sops-executable version))))
  (cdr sops--version-cache))

(defun sops--filestatus (file)
  "Return t if FILE is sops-encrypted, nil otherwise.
Threads `sops-input-type-overrides' as `--input-type' if matched.

A nil return means \"not known to be encrypted\" -- sops errored, the
file is unreadable, the JSON parse failed, or the file is genuinely
plaintext.  Callers must not treat nil as a positive plaintext signal;
only treat t as a positive encrypted signal."
  (let* ((input-type (sops--input-type-for file))
         (args (append '("filestatus")
                       (when input-type (list "--input-type" input-type))
                       (list file)))
         (result (sops--run args)))
    (and (eq 0 (plist-get result :exit-status))
         (sops--parse-filestatus (plist-get result :stdout)))))

(defun sops--popup-error (file args exit-status stderr)
  "Pop up *sops-error: FILE* with details of a sops invocation failure.
ARGS is the list passed to sops, EXIT-STATUS the exit code (integer),
STDERR the captured stderr (string; pass \"\" if absent).  Returns the
displayed buffer."
  (let* ((buf-name (format "*sops-error: %s*" file))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "sops %s\n" (mapconcat #'identity args " "))
                (format "Exit status: %d\n" exit-status)
                (format "Time: %s\n" (format-time-string "%FT%T%z"))
                "─── stderr ───\n"
                stderr
                "\n─── recovery ───\n"
                "Fix the issue above (e.g., re-auth, plug in yubikey,"
                " set AWS_PROFILE), then in the original buffer:\n"
                "  C-x C-s            retry save (encrypt errors)\n"
                "  M-x revert-buffer  retry decrypt (decrypt errors)\n"))
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (local-set-key (kbd "q") #'quit-window))
    (display-buffer buf)
    buf))

(defcustom sops-extra-decrypt-args nil
  "Additional arguments to sops on decrypt.
Inserted between the `decrypt' subcommand (plus any auto-threaded
`--input-type'/`--output-type' args) and the trailing file path.
Example for forcing yaml output:
\\='(\"--output-type\" \"yaml\").

Mirrors `sops-extra-encrypt-args' in shape and intent."
  :type '(repeat string)
  :group 'sops)

(defcustom sops-extra-encrypt-args nil
  "Additional arguments to sops on encrypt.
Inserted between `--filename-override FILE' and the trailing
input-file path that `sops--run' appends.  Example for age SSH:
\\='(\"-a\" \"<ssh-key>\")."
  :type '(repeat string)
  :group 'sops)

(defcustom sops-before-decrypt-hook nil
  "Hook run before each sops decrypt invocation.
Runs in the buffer being decrypted; the variable `buffer-file-name'
holds the encrypted file's path.  Use to set `AWS_PROFILE', age key paths.
The hook fires unconditionally before `sops--run', regardless of
whether the subsequent decrypt succeeds."
  :type 'hook
  :group 'sops)

(defcustom sops-before-encrypt-hook nil
  "Hook run before each sops encrypt invocation.
Runs in the buffer being encrypted; the variable `buffer-file-name'
holds the target file's path.  Use to set `AWS_PROFILE', age key paths.
The hook fires unconditionally before `sops--run'."
  :type 'hook
  :group 'sops)

(defun sops--maybe-output-type (input-type existing-args)
  "Return `(\"--output-type\" INPUT-TYPE)' to thread into a sops call, or nil.
Workaround for upstream sops bug: when `--input-type' overrides a
non-native extension (e.g. `.txt'), sops picks the *output* store
independently from the filename and falls back to the binary store,
which then can't dump a structured tree -- the user sees
\"error emitting binary store: no binary data found in tree\".  See
getsops/sops#235 (closed without a code fix; the maintainers added
the error hint instead of inferring output-type from input-type).

Returns the pair only when INPUT-TYPE is non-nil and EXISTING-ARGS
doesn't already contain `--output-type' (so a user who explicitly
sets one wins).  EXISTING-ARGS is the user-supplied args list this
call is being threaded into (`sops-extra-decrypt-args' for decrypt,
`sops-extra-encrypt-args' for encrypt)."
  (when (and input-type
             (not (member "--output-type" existing-args)))
    (list "--output-type" input-type)))

(defun sops--decrypt-buffer ()
  "Decrypt current buffer's file via sops, replacing buffer contents.
Return t on success, nil on failure (popping an error buffer).
Caller must have set the variable `buffer-file-name' to the encrypted file path."
  (run-hooks 'sops-before-decrypt-hook)
  (let* ((file buffer-file-name)
         (input-type (sops--input-type-for file))
         (args (append '("decrypt")
                       (when input-type (list "--input-type" input-type))
                       (sops--maybe-output-type
                        input-type sops-extra-decrypt-args)
                       sops-extra-decrypt-args
                       (list file)))
         (result (sops--run args))
         (exit (plist-get result :exit-status)))
    (if (eq 0 exit)
        (progn
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (plist-get result :stdout)))
          (set-buffer-modified-p nil)
          ;; Re-detect major mode against the now-decrypted plaintext, but
          ;; via `set-auto-mode' (extension/auto-mode-alist only) rather
          ;; than `normal-mode' which would also process file-local
          ;; variables and `-*- eval: ... -*-' cookies.  The decrypted
          ;; content is not from a trusted source -- a third party with
          ;; write access to the ciphertext could embed malicious
          ;; cookies that `normal-mode' would honor at decrypt time.
          (set-auto-mode)
          t)
      (sops--popup-error file args exit (plist-get result :stderr))
      nil)))

(defun sops--encrypt-and-write ()
  "Encrypt current buffer via sops and write to the variable `buffer-file-name'.
Return t on success.  Signals `user-error' on failure (aborts save).

Reads the full buffer (widened) so a narrowed buffer isn't silently
truncated on save.  Suppresses backups (`make-backup-files') around
the write because ciphertext backups accumulate without recovery
value -- the user can't usefully edit them manually.

After `write-region', refreshes `visited-file-modtime' so Emacs's
modtime check (used by `verify-visited-file-modtime') matches the
file we just wrote.  Without this, the next edit triggers a
\"FILE has changed on disk; really edit the buffer?\" prompt
because `find-file' recorded the *encrypted* file's modtime and
our write replaced it.

On the first successful save of a `sops-find-file' \\='creating
buffer, transitions `sops--state.status' to \\='decrypted so
subsequent saves and reverts follow the normal v0.2 paths."
  (run-hooks 'sops-before-encrypt-hook)
  (let* ((file buffer-file-name)
         (input-type (sops--input-type-for file))
         (args (append '("encrypt" "--filename-override")
                       (list file)
                       (when input-type (list "--input-type" input-type))
                       (sops--maybe-output-type
                        input-type sops-extra-encrypt-args)
                       sops-extra-encrypt-args))
         (result (sops--run args :input (save-restriction
                                          (widen)
                                          (buffer-substring-no-properties
                                           (point-min) (point-max)))))
         (exit (plist-get result :exit-status))
         (stdout (plist-get result :stdout)))
    (cond
     ((not (eq 0 exit))
      (sops--popup-error file args exit (plist-get result :stderr))
      (user-error "Sops encrypt failed (exit %d)" exit))
     ((zerop (length stdout))
      (sops--popup-error file args exit "sops: encrypt produced empty output\n")
      (user-error "Sops encrypt produced empty output")))
    (let ((coding-system-for-write 'no-conversion)
          (make-backup-files nil))
      (write-region stdout nil file nil 'silent))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    ;; First-save transition for sops-find-file's 'creating buffers.
    ;; After this, the buffer is indistinguishable from one decrypted
    ;; via the find-file-hook; reverts go through sops--revert-buffer.
    (when (and sops--state
               (eq (sops-state-status sops--state) 'creating))
      (setf (sops-state-status sops--state) 'decrypted))
    t))

(defun sops--write-contents-function ()
  "Hook function for `write-contents-functions'.
Returns t when save was handled (skipping normal write).
Signals `user-error' on save failure."
  (sops--encrypt-and-write))

(defun sops--retry-decrypt-on-revert (&rest _args)
  "Retry sops decrypt as a `revert-buffer-function' after initial failure.
Installed in `sops--find-file-hook' when the first `sops--decrypt-buffer'
call exits non-zero -- without this hook, `revert-buffer' would fall
through to the default implementation which just re-reads the encrypted
bytes and never re-invokes sops, so the recovery hint printed into
`*sops-error:*' (\"fix auth, then \\[revert-buffer]\") would be a lie.

Re-reads the encrypted file from disk (in case the user also fixed
things externally) and re-runs `sops--decrypt-buffer'.  On success,
disables `read-only-mode' and enables `sops-mode' -- enabling sops-mode
installs the real `sops--revert-buffer' for subsequent reverts, so this
retry function only runs as long as decrypt keeps failing.  On
continued failure, `sops--decrypt-buffer' pops the error buffer again
and the buffer stays read-only with ciphertext."
  (save-restriction
    (widen)
    (set-visited-file-modtime)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents buffer-file-name)))
  (set-buffer-modified-p nil)
  (when (sops--decrypt-buffer)
    ;; Mirror the find-file-hook failure path: set `buffer-read-only'
    ;; directly so `revert-buffer's read-only state restoration (inline
    ;; on Emacs 29.1, hook-driven on 30+) leaves us writable.
    (setq buffer-read-only nil)
    ;; Pre-set state so the `sops-mode' enable guard skips its own
    ;; `sops--filestatus' re-check -- we just decrypted, the file is
    ;; sops-encrypted by definition.
    (setq sops--state (sops-state-create :status 'decrypted))
    (sops-mode 1)))

(defun sops--revert-buffer (&rest _args)
  "Revert function for sops-mode buffers: re-read encrypted file and decrypt.
Widens before erasing so a narrowed buffer doesn't corrupt itself with
mixed encrypted + plaintext content (parallels the narrowing defense
in `sops--encrypt-and-write').

Refreshes `visited-file-modtime' BEFORE `erase-buffer'.  Two reasons:

  1. After the revert, `verify-visited-file-modtime' must return t
     so the next keystroke doesn't re-fire the \"FILE has changed on
     disk\" prompt.

  2. `erase-buffer' triggers Emacs's `lock-file' path which calls
     `ask-user-about-supersession-threat' if the modtime is stale.
     In batch mode that errors out (\"Cannot resolve conflict in
     batch mode\"); interactively it would re-fire the supersession
     prompt mid-revert.  Updating the recorded modtime first
     suppresses the check because the buffer now \"agrees\" with disk."
  (save-restriction
    (widen)
    (set-visited-file-modtime)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents buffer-file-name)))
  (when (sops--decrypt-buffer)
    (setq sops-mode t)
    (setq sops--state (sops-state-create :status 'decrypted))
    (sops--restore-after-major-mode-change))
  (set-buffer-modified-p nil))

;;;###autoload
(define-minor-mode sops-mode
  "Edit the current SOPS-encrypted file transparently.
Decryption happens at `find-file'; encryption happens at `save-buffer'.
Plaintext never reaches disk (backups and auto-save are suppressed)."
  :init-value nil
  :lighter " sops"
  :group 'sops
  (cond
   (sops-mode
    ;; Refuse to enable on a buffer whose visited file isn't sops-encrypted.
    ;; `sops--find-file-hook' and `sops--retry-decrypt-on-revert' both
    ;; validate via `sops--filestatus' + a successful decrypt before
    ;; reaching here, so they pre-set `sops--state' to signal "trust me".
    ;; This guard catches manual `M-x sops-mode' on a regular buffer,
    ;; which would otherwise install encrypt-on-save hooks that fail at
    ;; save time -- and the disable branch's modified-buffer guardrail
    ;; would then trap the user with no clean escape.
    (unless sops--state
      (unless (and buffer-file-name
                   (not (file-remote-p buffer-file-name))
                   (sops--filestatus buffer-file-name))
        (setq sops-mode nil)
        (user-error "sops-mode: %s is not a sops-encrypted file"
                    (or buffer-file-name "this buffer")))
      (when (buffer-modified-p)
        (setq sops-mode nil)
        (user-error "sops-mode: refusing to decrypt modified buffer; revert first"))
      (unless (sops--decrypt-buffer)
        (setq sops-mode nil)
        (user-error "sops-mode: failed to decrypt %s" buffer-file-name))
      (setq sops-mode t)
      (setq sops--state (sops-state-create :status 'decrypted)))
    (setq-local make-backup-files nil)
    (setq-local buffer-auto-save-file-name nil)
    (setq-local revert-buffer-function #'sops--revert-buffer)
    (add-hook 'write-contents-functions #'sops--write-contents-function nil t)
    ;; External writes (magit discard, git checkout, sops -e from CLI) update
    ;; the file behind our back.  auto-revert-mode picks them up and calls
    ;; `revert-buffer-function' (= `sops--revert-buffer') so the user sees
    ;; the new ciphertext re-decrypted instead of a stale buffer + the
    ;; "really edit?" prompt.  Defaults to file-notify (kqueue/inotify) when
    ;; available, falling back to polling.  See test/sops-test.el for why
    ;; batch tests force polling (the interactive main loop drains queued
    ;; events between commands; batch does not, and that deadlocks the next
    ;; `sops--run').
    (auto-revert-mode 1)
    ;; Inhibit apheleia (and any future formatters that respect this var
    ;; convention).  Two reasons: (1) apheleia's before-save formatter runs
    ;; before our `write-contents-functions' hook and can hang the save flow
    ;; before sops is even reached; (2) reformatting decrypted plaintext
    ;; before encrypt would change the ciphertext on every save, producing
    ;; meaningless `git diff' churn even on no-op edits.  apheleia documents
    ;; `apheleia-inhibit' as its buffer-local opt-out.
    (setq-local apheleia-inhibit t)
    ;; Lazy-install the global major-mode-change restoration hook.  Doing
    ;; this on first `sops-mode' enable rather than at package load keeps
    ;; `(require \\='sops)' free of global side effects -- users who load
    ;; sops.el but never visit a SOPS file pay no per-buffer hook cost.
    (unless sops--restore-hook-installed
      (add-hook 'after-change-major-mode-hook
                #'sops--restore-after-major-mode-change)
      (setq sops--restore-hook-installed t)))
   (t
    (when (buffer-modified-p)
      (setq sops-mode 1)  ; revert the toggle
      (user-error
       "Sops: buffer modified; revert-buffer first or use M-x read-only-mode"))
    (auto-revert-mode -1)
    (kill-local-variable 'make-backup-files)
    (kill-local-variable 'buffer-auto-save-file-name)
    (kill-local-variable 'revert-buffer-function)
    (kill-local-variable 'apheleia-inhibit)
    (remove-hook 'write-contents-functions #'sops--write-contents-function t)
    (setq sops--state nil))))

;; Survive `kill-all-local-variables' (which fires whenever the user changes
;; major mode).  Without this, our protections evaporate and a subsequent
;; save would write plaintext to disk.
(put 'sops-mode 'permanent-local t)
(put 'sops--state 'permanent-local t)

(defun sops--restore-after-major-mode-change ()
  "Re-install sops-mode buffer protections after a major-mode change.
`kill-all-local-variables' wipes our hook entries and buffer-local var
settings, but `sops-mode' itself is permanent-local and survives.  This
function checks for that surviving flag and re-installs the rest."
  (when sops-mode
    (setq-local make-backup-files nil)
    (setq-local buffer-auto-save-file-name nil)
    (setq-local revert-buffer-function #'sops--revert-buffer)
    (add-hook 'write-contents-functions
              #'sops--write-contents-function nil t)
    (auto-revert-mode 1)
    (setq-local apheleia-inhibit t)))

(defun sops--start-creation (format)
  "Seed buffer with FORMAT's stub and enable `sops-mode' in \\='creating state.
FORMAT is a sops type name; see `sops--example-for' for accepted values.

The variable `buffer-file-name' must already be set (caller has typically
just called `find-file' on a non-existent path).  This helper:

  1. Erases the buffer and inserts the matching stub.
  2. Marks the buffer unmodified -- the example is not user edits.
  3. Pre-sets `sops--state' to \\='creating so the `sops-mode' enable
     guard skips its filestatus check (the file doesn't exist yet).
  4. Enables `sops-mode'.

On first successful save, `sops--encrypt-and-write' will transition
`sops--state.status' from \\='creating to \\='decrypted."
  (let ((stub (sops--example-for format)))
    (erase-buffer)
    (insert stub))
  (set-buffer-modified-p nil)
  (setq sops--state (sops-state-create :status 'creating))
  (sops-mode 1))

(defun sops--find-file-hook ()
  "On find-file: if file matches prefilter and is sops-encrypted, decrypt.
Skips remote (TRAMP) files in v0.2 — local sops binary cannot read TRAMP
paths.  Remote support belongs in the separate `tramp-sops' package."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (sops--prefilter-p buffer-file-name)
             (file-readable-p buffer-file-name))
    (condition-case err
        (when (sops--ensure-version)
          (when (sops--filestatus buffer-file-name)
            (if (sops--decrypt-buffer)
                (progn
                  ;; Pre-set state so the `sops-mode' enable guard skips
                  ;; its own `sops--filestatus' re-check -- we just
                  ;; validated above and ran decrypt successfully.
                  (setq sops--state
                        (sops-state-create :status 'decrypted))
                  (sops-mode 1))
              ;; Decrypt failed: park the retry function on
              ;; `revert-buffer-function' so the popped error buffer's
              ;; "M-x revert-buffer to retry" hint actually works.  On
              ;; successful retry, sops-mode activation replaces this with
              ;; the real `sops--revert-buffer'.
              (setq-local revert-buffer-function
                          #'sops--retry-decrypt-on-revert)
              ;; Set `buffer-read-only' directly rather than calling
              ;; `read-only-mode' so we don't populate
              ;; `read-only-mode--state'.  `revert-buffer' on Emacs 29.1
              ;; (inline) and 30+ (`revert-buffer-restore-read-only')
              ;; both capture that state pre-revert and re-apply
              ;; `buffer-read-only' from it post-revert, which would
              ;; undo the `(setq buffer-read-only nil)' our retry path
              ;; performs on successful re-decrypt.  With the state var
              ;; left untouched, the captured value is nil and the
              ;; restoration is a no-op.
              (setq buffer-read-only t))))
      (user-error
       ;; sops missing or too old: log once, do nothing
       (message "sops: %s" (error-message-string err))))))

;; Declare the removed v0.1.X variable globally special with a nil default.  A
;; bare `(defvar SYMBOL)' only marks the variable special inside the current
;; file's scope, so `let'-bindings of the same name in other files (tests,
;; user init) would be lexical and invisible to `symbol-value' here.
;; Providing a default value makes the symbol globally special; v0.1.X configs
;; setting it to a non-nil value will then trip the warning below.
(defvar sops-before-encrypt-decrypt-hook nil
  "Removed in v0.2.  Was a single hook fired before both decrypt and encrypt.
Use `sops-before-decrypt-hook' and `sops-before-encrypt-hook' instead.
Kept only as a special variable so v0.1.X configs can be detected at
`global-sops-mode' activation; see `sops--check-v0.1.X-config'.")

(defvar sops-decrypt-args nil
  "Removed in v0.2.  Replaced by `sops-extra-decrypt-args'.
Drop the leading \"decrypt\" subcommand from any prior value; that
subcommand is now hardcoded in `sops--decrypt-buffer'.  Kept only as
a special variable so v0.1.X configs can be detected at
`global-sops-mode' activation; see `sops--check-v0.1.X-config'.")

(defun sops--check-v0.1.X-config ()
  "Warn the user about removed v0.1.X configuration that's still set."
  (when (and (boundp 'sops-before-encrypt-decrypt-hook)
             (symbol-value 'sops-before-encrypt-decrypt-hook))
    (display-warning
     'sops
     (concat "`sops-before-encrypt-decrypt-hook' is removed in v0.2. "
             "Use `sops-before-decrypt-hook' and "
             "`sops-before-encrypt-hook' instead.")))
  (when (and (boundp 'sops-decrypt-args)
             (symbol-value 'sops-decrypt-args))
    (display-warning
     'sops
     (concat "`sops-decrypt-args' is replaced in v0.2 by "
             "`sops-extra-decrypt-args'.  Drop the leading \"decrypt\" "
             "subcommand and rename."))))

;;;###autoload
(define-globalized-minor-mode global-sops-mode
  sops-mode
  (lambda () nil)  ; sops-mode itself is enabled inside sops--find-file-hook, not here
  :group 'sops
  (if global-sops-mode
      (progn
        (sops--check-v0.1.X-config)
        (add-hook 'find-file-hook #'sops--find-file-hook))
    (remove-hook 'find-file-hook #'sops--find-file-hook)))

;;;###autoload
(defun sops-find-file (path)
  "Visit PATH; if PATH does not exist, create it as a new SOPS-encrypted file.

For existing paths this delegates to `find-file' -- the v0.2
`find-file-hook' handles transparent decryption when `global-sops-mode'
is active.

For non-existent paths, signals `user-error' before any buffer is
created if:
  - PATH is empty, or names a directory (ends in `/');
  - PATH is remote (TRAMP);
  - PATH's parent directory does not exist;
  - no `.sops.yaml' is reachable in any ancestor of PATH's parent;
  - sops binary is missing or older than 3.9.0.

Otherwise visits PATH (which creates an empty buffer with no file on
disk yet), seeds it with a format-appropriate SOPS example -- ported
from upstream sops's `EmitExample' -- and enables `sops-mode' in
\\='creating state.  On the first successful `save-buffer', sops
encrypts the buffer's contents to PATH and the buffer transitions
to the normal \\='decrypted state.

Interactive: prompts via `read-file-name'."
  (interactive (list (read-file-name "Find SOPS file: ")))
  (when (string-empty-p path)
    (user-error "Sops-find-file: not a file path: %s" path))
  (let ((path (expand-file-name path)))
    (when (directory-name-p path)
      (user-error "Sops-find-file: not a file path: %s" path))
    (when (file-remote-p path)
      (user-error "Sops-find-file: remote paths not supported: %s" path))
    (if (file-exists-p path)
        (find-file path)
      (let ((parent (file-name-directory path)))
        (unless (and parent (file-exists-p parent))
          (user-error "Sops-find-file: parent directory does not exist: %s"
                      parent))
        (unless (locate-dominating-file parent ".sops.yaml")
          (user-error
           "Sops-find-file: no .sops.yaml found in any ancestor of %s"
           parent))
        (sops--ensure-version)
        (let ((format (sops--format-for path)))
          (find-file path)
          (sops--start-creation format))))))

(provide 'sops)
;;; sops.el ends here
