;;; morg.el --- Sync Matrix rooms into org for triage  -*- lexical-binding: t; -*-
;;
;; The pipeline:
;;   a book / show / note is posted to a Matrix room
;;     → `morg-sync'  — pulls every message not yet in org from each room in `morg-rooms' and
;;                      files it as an entry in that room's file (optionally under a heading,
;;                      with tags and a TODO keyword)
;;     → triage in org as usual — refile, TODO, delete
;;     → `morg-sync' then offers to redact the filed messages, which clears out the rooms.
;;       Redaction only happens after the org buffers are saved.
;;
;; Org entries are keyed by a MATRIX_EVENT property, searched across every configured file and
;; its archives, so a message is only ever filed once, even after the entry is archived or the
;; redaction was declined.  Messages already in org but still in the room are offered for
;; redaction again on the next sync.
;;
;; Only unencrypted rooms are supported: this talks to the client-server API directly and
;; can't decrypt E2EE events.
;;
;; Auth: morg authenticates with an application service token rather than a login, so it
;; works when the homeserver delegates login to SSO/OIDC.  The appservice is registered with
;; `url: null' (the homeserver never pushes anything to it) and a non-exclusive user namespace
;; covering `morg-user', and every request is made as that user via the `user_id' query
;; parameter.  So morg reads and redacts exactly as you would, with no bot to invite and no
;; power levels to change.  `morg-register' generates the registration; see its docstring.

(require 'json)
(require 'url)
(require 'url-http)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'auth-source)

(defvar morg-homeserver nil
  "Base URL of the Matrix homeserver, e.g. \"https://matrix.example.org\".")

(defvar morg-rooms nil
  "The rooms to sync: a list of (ROOM :file FILE [:heading HEADING] [:tags TAGS] [:todo KW]).
ROOM is an alias (`#books:example.org') or a room id (`!abc:example.org').  Messages are
filed into FILE as children of the heading named HEADING (created when missing), or at top
level when HEADING is omitted.  TAGS is a list of tag strings; KW a TODO keyword.  For example:

  \\='((\"#books:example.org\" :file \"~/personal/org/unfiled.org\" :heading \"Books\"
     :tags (\"book\"))
    (\"#watch:example.org\" :file \"~/personal/org/unfiled.org\" :heading \"Watch\"
     :tags (\"movie\"))
    (\"!abcd:example.org\"  :file \"~/personal/org/notes.org\" :todo \"TODO\"))")

(defvar morg-user nil
  "Matrix user id morg acts as, e.g. \"@me:example.org\".  It must match the user namespace
of morg's appservice registration (`morg-register' writes one for it).")

(defvar morg-token nil
  "Appservice token (the registration's as_token): a string, or a function returning one.
When nil, looked up with auth-source.")

(defvar morg-auth-user "morg"
  "Login auth-source looks up the appservice token under, e.g. in ~/.authinfo.gpg:

  machine matrix.example.org login morg password <as_token>

This is just a label for the entry, not your Matrix username.  `morg-register' and
`morg-set-token' write it.")

(defvar morg-auth-file nil
  "File `morg-register' and `morg-set-token' store the token in.  nil means the first .gpg
file in `auth-sources', else ~/.authinfo.gpg.  It must be one of `auth-sources' to be found
again.")

(defvar morg-appservice-id "morg"
  "Id of morg's appservice registration.")

(defvar morg-sender-localpart "morg"
  "Localpart of the appservice's own user (unused by morg, but every registration has one).
It must not be an existing user.")

(defvar morg-senders nil
  "When non-nil, a list of user ids (`@me:example.org'); only their messages are synced.")

(defvar morg-redact-reason "filed to org"
  "Reason given when redacting synced messages.")

(defvar morg-heading-max 100
  "Longest first line used as-is for a heading.  Longer ones are truncated in the heading and
kept whole in the body.")

(defvar morg-page-size 100
  "Events requested per /messages page.")

(defvar morg--memo nil
  "Per-command cache (hash table), bound by each interactive command.")

(defvar morg--txn 0
  "Counter making redaction transaction ids unique within one Emacs session.")

(defmacro morg--with-session (&rest body)
  `(let ((morg--memo (make-hash-table :test 'equal)))
     ,@body))

(defun morg--memoize (key fn)
  (if (not morg--memo)
      (funcall fn)
    (let ((hit (gethash key morg--memo 'miss)))
      (if (eq hit 'miss)
          (puthash key (funcall fn) morg--memo)
        hit))))

;;;; Matrix

(defun morg--host ()
  (url-host (url-generic-parse-url (morg--homeserver))))

(defun morg--homeserver ()
  (or (and morg-homeserver (string-remove-suffix "/" morg-homeserver))
      (user-error "morg: set `morg-homeserver'")))

(defun morg--token ()
  "The access token: `morg-token', else auth-source."
  (let ((token (or morg-token
                   (plist-get (car (auth-source-search :host (morg--host)
                                                       :user morg-auth-user :max 1))
                              :secret))))
    (or (if (functionp token) (funcall token) token)
        (user-error "morg: no appservice token for %s (run `morg-register' or `morg-set-token')"
                    (morg--host)))))

(defun morg--user ()
  (or morg-user (user-error "morg: set `morg-user'")))

(defun morg--http (method url data token)
  "Send METHOD to URL with DATA (a lisp value, JSON-encoded), authorized by TOKEN when it's
non-nil.  Return (STATUS RAW-BODY)."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (append (when token `(("Authorization" . ,(concat "Bearer " token))))
                  (when data '(("Content-Type" . "application/json")))))
         (url-request-data (and data (encode-coding-string (json-encode data) 'utf-8)))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (user-error "morg: no response from %s %s" method url))
    (unwind-protect
        (with-current-buffer buf
          (let ((bstart (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)))
            (list url-http-response-status
                  (and bstart (< bstart (point-max))
                       (string-trim (buffer-substring-no-properties bstart (point-max)))))))
      (kill-buffer buf))))

(defun morg--parse (raw)
  (when (and raw (> (length raw) 0))
    (ignore-errors
      (let ((json-object-type 'alist) (json-array-type 'list)
            (json-null nil) (json-false nil))
        (json-read-from-string (decode-coding-string raw 'utf-8))))))

(defun morg--request (method path &optional data query token)
  "Send METHOD to PATH under /_matrix/client/v3 with DATA (JSON-encoded) and QUERY (an alist
of strings), and return parsed JSON.  Authorized by the appservice TOKEN, or the stored one
when TOKEN is nil, acting as `morg-user'.  Rate-limited requests are retried after the delay
the server asks for; any other non-2xx signals a `user-error' with the status and body."
  (let* ((query (cons (cons "user_id" (morg--user)) query))
         ;; Not `url-build-query-string': it leaves `+' alone, which decodes as a space.
         (url (concat (morg--homeserver) "/_matrix/client/v3" path "?"
                      (mapconcat (lambda (p) (concat (url-hexify-string (car p)) "="
                                                     (url-hexify-string (cdr p))))
                                 query "&")))
         (token (or token (morg--token))))
    (cl-loop for attempt from 1
             for (status raw) = (morg--http method url data token)
             for json = (morg--parse raw)
             if (and (eql status 429) (< attempt 5))
             do (sleep-for (/ (or (alist-get 'retry_after_ms json) 1000) 1000.0))
             else return
             (cond
              ((null status)
               (user-error "morg: no HTTP status from %s %s" method path))
              ((not (and (>= status 200) (< status 300)))
               (user-error "morg: %s %s failed (HTTP %d): %s" method path status
                           (if (and raw (> (length raw) 0)) raw "(empty body)")))
              (t json)))))

(defun morg--room-id (room)
  "ROOM's id, resolving it first when it's an alias."
  (if (string-prefix-p "!" room)
      room
    (morg--memoize
     (list 'alias room)
     (lambda ()
       (or (alist-get 'room_id (morg--request "GET" (concat "/directory/room/"
                                                           (url-hexify-string room))))
           (user-error "morg: couldn't resolve %s" room))))))

(defun morg--relation (event)
  (alist-get 'm.relates_to (alist-get 'content event)))

(defun morg--edit-p (event)
  (equal (alist-get 'rel_type (morg--relation event)) "m.replace"))

(defun morg--apply-edits (events)
  "Drop edit events from EVENTS (chronological), folding each one's new content into the
event it replaces, so a message is filed as last edited."
  (let ((edits (make-hash-table :test 'equal)))
    (dolist (ev events)
      (when (morg--edit-p ev)
        (puthash (alist-get 'event_id (morg--relation ev))
                 (alist-get 'm.new_content (alist-get 'content ev))
                 edits)))
    (cl-loop for ev in events
             unless (morg--edit-p ev)
             collect (let ((new (gethash (alist-get 'event_id ev) edits)))
                       (if new (cons (cons 'content new) ev) ev)))))

(defun morg--keep-p (event)
  "Non-nil when EVENT is a live text-bearing message from a synced sender.
Redacted events have empty content, so they drop out here."
  (and (equal (alist-get 'type event) "m.room.message")
       (alist-get 'body (alist-get 'content event))
       (or (null morg-senders) (member (alist-get 'sender event) morg-senders))))

(defun morg--room-messages (room-id)
  "Every message in ROOM-ID, oldest first, with edits applied."
  (let ((path (format "/rooms/%s/messages" (url-hexify-string room-id)))
        (filter (json-encode '((types . ["m.room.message"]))))
        from events (more t))
    (while more
      (let* ((page (morg--request "GET" path nil
                                  `(("dir" . "b")
                                    ("limit" . ,(number-to-string morg-page-size))
                                    ("filter" . ,filter)
                                    ,@(when from `(("from" . ,from))))))
             (chunk (alist-get 'chunk page))
             (end (alist-get 'end page)))
        (setq events (nconc events chunk)
              more (and chunk end (not (equal end from)))
              from end)))
    (cl-remove-if-not #'morg--keep-p (morg--apply-edits (nreverse events)))))

(defun morg--redact (room-id event-id)
  (morg--request "PUT" (format "/rooms/%s/redact/%s/%s"
                               (url-hexify-string room-id)
                               (url-hexify-string event-id)
                               (format "morg-%d-%d" (truncate (* 1000 (float-time)))
                                       (cl-incf morg--txn)))
                 `((reason . ,morg-redact-reason))))

;;;; Auth

(defun morg--auth-file ()
  (expand-file-name
   (or morg-auth-file
       (cl-find-if (lambda (s) (and (stringp s) (string-suffix-p ".gpg" s))) auth-sources)
       "~/.authinfo.gpg")))

(defun morg--store-token (host token)
  "Write TOKEN for HOST into `morg--auth-file', replacing morg's existing entry for HOST.
Return the file.  auth-source's own saving is bypassed, so this works even when
`auth-source-save-behavior' is nil."
  (let* ((file (morg--auth-file))
         (visited (get-file-buffer file))
         (line (format "machine %s login %s password %s" host morg-auth-user token)))
    (with-current-buffer (or visited (find-file-noselect file))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (format "^machine %s login %s .*$"
                                       (regexp-quote host) (regexp-quote morg-auth-user))
                               nil t)
            (replace-match line t t)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert line "\n")))
      (save-buffer)
      (set-file-modes file #o600)
      ;; Don't leave the decrypted secrets sitting in a buffer we opened.
      (unless visited (kill-buffer)))
    (auth-source-forget-all-cached)
    file))

(defun morg--whoami (token)
  "User id TOKEN acts as, or nil when it doesn't work."
  (ignore-errors
    (alist-get 'user_id (morg--request "GET" "/account/whoami" nil nil token))))

(defun morg--random-token ()
  "64 hex characters from the system's CSPRNG."
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (let ((coding-system-for-read 'binary))
                   (call-process "head" "/dev/urandom" t nil "-c" "32"))
                 (buffer-string))))
    (unless (= (length bytes) 32)
      (user-error "morg: couldn't read /dev/urandom"))
    (mapconcat (lambda (c) (format "%02x" c)) bytes "")))

(defun morg--registration (as-token hs-token)
  "Appservice registration YAML for AS-TOKEN and HS-TOKEN."
  (format "# morg: Synapse appservice registration.
# Reference this file from homeserver.yaml and restart Synapse:
#   app_service_config_files:
#     - /path/to/morg-registration.yaml
id: %s
url: null
as_token: %s
hs_token: %s
sender_localpart: %s
rate_limited: false
namespaces:
  users:
    - regex: '^%s$'
      exclusive: false
  aliases: []
  rooms: []
"
          morg-appservice-id as-token hs-token morg-sender-localpart
          (regexp-quote (morg--user))))

(defun morg--report-stored (file)
  (cond
   (morg-token "  (but `morg-token' is set and takes precedence)")
   ((not (member file (mapcar (lambda (s) (and (stringp s) (expand-file-name s)))
                              auth-sources)))
    "  (add it to `auth-sources' so morg finds it)")
   (t "")))

(defun morg--read-config ()
  "Prompt for `morg-homeserver' and `morg-user' when they're unset."
  (unless morg-homeserver
    (setq morg-homeserver (string-trim (read-string "Homeserver URL: " "https://"))))
  (unless morg-user
    (setq morg-user (string-trim (read-string "Matrix user id morg acts as: "
                                              (format "@%s:%s" (user-login-name)
                                                      (morg--host)))))))

;;;###autoload
(defun morg-register ()
  "Generate an appservice registration for morg and store its token.

Generates fresh tokens, stores the as_token in `morg-auth-file' and shows the registration
YAML in a buffer.  Save that on the homeserver, add it to `app_service_config_files' in
homeserver.yaml and restart Synapse; morg can then act as `morg-user'.  Running this again
replaces the stored token, so the registration on the server must be replaced too."
  (interactive)
  (morg--read-config)
  (let ((old (ignore-errors (morg--token))))
    (when (and old
               (not (y-or-n-p "morg already has a token; replace it (and the registration)? ")))
      (user-error "Aborted")))
  (let* ((as-token (morg--random-token))
         (file (morg--store-token (morg--host) as-token))
         (buf (get-buffer-create "*morg-registration*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (morg--registration as-token (morg--random-token)))
        (when (fboundp 'yaml-mode) (yaml-mode))
        (goto-char (point-min))))
    (pop-to-buffer buf)
    (message "morg: token saved to %s%s; install the registration shown and restart Synapse"
             (abbreviate-file-name file) (morg--report-stored file))))

;;;###autoload
(defun morg-set-token (token)
  "Store TOKEN, an existing appservice as_token, for morg in `morg-auth-file'.
It's checked against the homeserver first, acting as `morg-user'."
  (interactive
   (progn (morg--read-config)
          (list (string-trim (read-passwd "Appservice as_token: ")))))
  (let ((user (morg--whoami token)))
    (unless (equal user (morg--user))
      (user-error "morg: that token can't act as %s (is the registration installed?)"
                  (morg--user)))
    (let ((file (morg--store-token (morg--host) token)))
      (message "morg: acting as %s; token saved to %s%s"
               user (abbreviate-file-name file) (morg--report-stored file)))))

;;;###autoload
(defun morg-check ()
  "Report whether morg's stored token works."
  (interactive)
  (let ((user (morg--whoami (morg--token))))
    (if user
        (message "morg: OK, acting as %s on %s" user (morg--homeserver))
      (user-error "morg: the token doesn't work as %s (is the registration installed?)"
                  (morg--user)))))

;;;; Org

(defun morg--files ()
  "Every configured :file, expanded."
  (delete-dups
   (cl-loop for (room . config) in morg-rooms
            collect (expand-file-name
                     (or (plist-get config :file)
                         (user-error "morg: %s has no :file in `morg-rooms'" room))))))

(defun morg--org-entries ()
  "Hash of MATRIX_EVENT → t for every entry in the configured files, archives included."
  (let ((entries (make-hash-table :test 'equal))
        (org-agenda-files (cl-remove-if-not #'file-exists-p (morg--files))))
    (when org-agenda-files
      (org-map-entries
       (lambda () (puthash (org-entry-get nil "MATRIX_EVENT") t entries))
       "MATRIX_EVENT={.}" 'agenda-with-archives))
    entries))

(defun morg--split-body (body)
  "Split message BODY into (HEADING . REST); REST is nil when there's nothing more."
  (let* ((lines (split-string (string-trim body) "\n"))
         (first (string-trim (car lines)))
         (rest (string-trim (string-join (cdr lines) "\n")))
         (long (> (length first) morg-heading-max)))
    (cons (cond ((string-empty-p first) "(empty)")
                (long (concat (string-trim-right (substring first 0 morg-heading-max)) "…"))
                (t first))
          (let ((text (string-join (delq nil (list (and long first)
                                                   (unless (string-empty-p rest) rest)))
                                   "\n")))
            (unless (string-empty-p text) text)))))

(defun morg--escape-body (text)
  "Indent TEXT's lines that would otherwise read as org headings."
  (replace-regexp-in-string "^\\*" " *" text))

(defun morg--timestamp (event)
  "Inactive org timestamp of when EVENT was sent."
  (let ((ms (alist-get 'origin_server_ts event)))
    (when ms
      (format-time-string "[%Y-%m-%d %a %H:%M]" (seconds-to-time (/ ms 1000.0))))))

(defun morg--goto-heading (heading)
  "Move to the first heading titled HEADING, appending a top-level one when there's none."
  (goto-char (point-min))
  (unless (re-search-forward (format org-complex-heading-regexp-format
                                     (regexp-quote heading))
                             nil t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " heading "\n")
    (forward-line -1))
  (org-back-to-heading t))

(defun morg--file-event (event room-id config)
  "File EVENT (from ROOM-ID) as an entry per CONFIG.  Return the buffer it went into; the
caller saves it."
  (let* ((content (alist-get 'content event))
         (split (morg--split-body (alist-get 'body content)))
         (heading (plist-get config :heading))
         (todo (plist-get config :todo))
         (tags (plist-get config :tags))
         (mxc (alist-get 'url content)))
    (with-current-buffer (find-file-noselect (expand-file-name (plist-get config :file)))
      (org-with-wide-buffer
       (let ((level (if heading
                        (progn (morg--goto-heading heading)
                               (prog1 (1+ (org-current-level))
                                 (org-end-of-subtree t t)))
                      (goto-char (point-max))
                      1)))
         (unless (bolp) (insert "\n"))
         (let ((start (point)))
           (insert (make-string level ?*) " " (if todo (concat todo " ") "")
                   (replace-regexp-in-string "[\n\r]+" " " (car split)) "\n"
                   (string-join (delq nil (list (morg--timestamp event)
                                                (and (cdr split)
                                                     (morg--escape-body (cdr split)))))
                                "\n")
                   "\n")
           (goto-char start)
           (when tags (org-set-tags tags))
           (org-entry-put nil "MATRIX_EVENT" (alist-get 'event_id event))
           (org-entry-put nil "MATRIX_ROOM" room-id)
           (org-entry-put nil "MATRIX_SENDER" (alist-get 'sender event))
           (when mxc (org-entry-put nil "MATRIX_MXC" mxc)))))
      (current-buffer))))

;;;; Sync

(defun morg--offer-redact (events)
  "Offer to redact EVENTS, a list of (ROOM-ID . EVENT-ID).  Failures are reported, not fatal."
  (when (and events
             (y-or-n-p (format "Redact %d message(s) from Matrix? " (length events))))
    (let ((failed 0))
      (pcase-dolist (`(,room-id . ,event-id) events)
        (condition-case err
            (morg--redact room-id event-id)
          (error (cl-incf failed)
                 (message "morg: couldn't redact %s: %s" event-id (error-message-string err)))))
      (message "morg: redacted %d message(s)%s" (- (length events) failed)
               (if (> failed 0) (format ", %d failed (see *Messages*)" failed) "")))))

;;;###autoload
(defun morg-sync ()
  "File every message in `morg-rooms' that isn't in org yet, then offer to redact them.

Messages already in org (from an earlier sync whose redaction was declined) are offered for
redaction again.  Org buffers are saved before anything is redacted."
  (interactive)
  (unless morg-rooms (user-error "morg: set `morg-rooms'"))
  (morg--with-session
   (let ((known (morg--org-entries))
         buffers filed present counts)
     (unwind-protect
         (pcase-dolist (`(,room . ,config) morg-rooms)
           (let ((room-id (morg--room-id room)) (n 0))
             (dolist (ev (morg--room-messages room-id))
               (let ((id (alist-get 'event_id ev)))
                 (if (gethash id known)
                     (push (cons room-id id) present)
                   (cl-pushnew (morg--file-event ev room-id config) buffers)
                   (puthash id t known)
                   (push (cons room-id id) filed)
                   (cl-incf n))))
             (push (cons room n) counts)))
       (dolist (b buffers)
         (with-current-buffer b (save-buffer))))
     (message "morg: filed %d (%s); %d already in org"
              (length filed)
              (mapconcat (lambda (c) (format "%s %d" (car c) (cdr c))) (nreverse counts) ", ")
              (length present))
     (morg--offer-redact (append (nreverse filed) (nreverse present))))))

;;;; At point

(defun morg--heading-at-point ()
  "Marker or position of the org heading at point (org buffer or agenda line), or nil."
  (cond
   ((derived-mode-p 'org-agenda-mode) (org-get-at-bol 'org-marker))
   ((and (derived-mode-p 'org-mode) (not (org-before-first-heading-p))) (point))))

;;;###autoload
(defun morg-browse-at-point ()
  "Open the Matrix message of the org heading at point via matrix.to (org buffer or agenda).
Only useful before the message is redacted."
  (interactive)
  (let* ((pom (morg--heading-at-point))
         (room (and pom (org-entry-get pom "MATRIX_ROOM" t)))
         (event (and pom (org-entry-get pom "MATRIX_EVENT" t))))
    (if (and room event)
        (browse-url (format "https://matrix.to/#/%s/%s"
                            (url-hexify-string room) (url-hexify-string event)))
      (message "morg: no Matrix message here"))))

;;
(provide 'morg)
;;; morg.el ends here
