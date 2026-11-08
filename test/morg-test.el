;;; morg-test.el --- Tests for morg  -*- lexical-binding: t; -*-
;;
;; emacs -Q --batch -L . -l morg-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'morg)

;;;; A fake homeserver

(defvar morg-test--rooms nil
  "Alist of room id → list of events, oldest first.")
(defvar morg-test--redacted nil)
(defvar morg-test--requests nil)

(defun morg-test--event (id body &rest extra)
  `((type . "m.room.message") (event_id . ,id) (sender . "@me:x")
    (origin_server_ts . 1761868800000)
    (content . ((msgtype . "m.text") (body . ,body) ,@extra))))

(defun morg-test--request (method path &optional data query token)
  (push (list method path data query token) morg-test--requests)
  (cond
   ((equal path "/account/whoami")
    (if (equal token "as_good") `((user_id . ,morg-user)) (user-error "HTTP 401")))
   ((string-match "\\`/directory/room/\\(.+\\)\\'" path)
    `((room_id . ,(concat "!" (substring (url-unhex-string (match-string 1 path)) 1)))))
   ((string-match "\\`/rooms/\\([^/]+\\)/messages\\'" path)
    ;; Pages of two, newest first; `from' is how many events have been returned so far.
    (let* ((events (reverse (alist-get (url-unhex-string (match-string 1 path))
                                       morg-test--rooms nil nil #'equal)))
           (from (string-to-number (or (cdr (assoc "from" query)) "0")))
           (chunk (seq-take (nthcdr from events) 2)))
      `((chunk . ,chunk)
        ,@(when (< (+ from 2) (length events))
            `((end . ,(number-to-string (+ from 2))))))))
   ((string-match "\\`/rooms/\\([^/]+\\)/redact/\\([^/]+\\)/" path)
    (push (url-unhex-string (match-string 2 path)) morg-test--redacted)
    '((event_id . "$r")))
   (t (error "unexpected %s %s" method path))))

(defmacro morg-test--with-server (rooms &rest body)
  (declare (indent 1))
  `(let ((morg-test--rooms ,rooms)
         morg-test--redacted morg-test--requests
         (morg-homeserver "https://matrix.test")
         (morg-user "@ryan:matrix.test")
         (morg-senders nil))
     (cl-letf (((symbol-function 'morg--request) #'morg-test--request))
       ,@body)))

(defmacro morg-test--with-org-file (var contents &rest body)
  (declare (indent 2))
  `(let ((,var (make-temp-file "morg-test" nil ".org" ,contents)))
     (unwind-protect (progn ,@body)
       (let ((b (find-buffer-visiting ,var)))
         (when b (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))
       (delete-file ,var))))

(defun morg-test--contents (file)
  (with-current-buffer (find-file-noselect file)
    (revert-buffer t t)
    (buffer-substring-no-properties (point-min) (point-max))))

;;;; Fetching

(ert-deftest morg-test-messages-paginate-and-filter ()
  (morg-test--with-server
      `(("!r:x" . (,(morg-test--event "$1" "one")
                   ,(morg-test--event "$2" "two")
                   ((type . "m.room.message") (event_id . "$3") (sender . "@me:x")
                    (content))            ; redacted
                   ,(morg-test--event "$4" "* fourr" '(m.relates_to (rel_type . "m.replace")
                                                                     (event_id . "$1"))
                                      '(m.new_content (msgtype . "m.text") (body . "one, edited")))
                   ,(morg-test--event "$5" "five"))))
    (let ((events (morg--room-messages "!r:x")))
      (should (equal (mapcar (lambda (e) (alist-get 'event_id e)) events) '("$1" "$2" "$5")))
      (should (equal (alist-get 'body (alist-get 'content (car events))) "one, edited"))
      ;; 5 events in pages of two.
      (should (= 3 (length morg-test--requests))))))

(ert-deftest morg-test-senders ()
  (morg-test--with-server
      `(("!r:x" . (,(morg-test--event "$1" "mine")
                   ,(cons '(sender . "@them:x") (morg-test--event "$2" "theirs")))))
    (let ((morg-senders '("@me:x")))
      (should (equal (mapcar (lambda (e) (alist-get 'event_id e)) (morg--room-messages "!r:x"))
                     '("$1"))))))

;;;; Formatting

(ert-deftest morg-test-split-body ()
  (should (equal (morg--split-body "Dune\n") '("Dune")))
  (should (equal (morg--split-body "  Dune\n\nby Frank Herbert\n") '("Dune" . "by Frank Herbert")))
  (let* ((morg-heading-max 10)
         (split (morg--split-body "abcdefghijklmnop\nmore")))
    (should (equal (car split) "abcdefghij…"))
    (should (equal (cdr split) "abcdefghijklmnop\nmore")))
  (should (equal (morg--escape-body "a\n* b\n** c") "a\n * b\n ** c")))

;;;; Sync

(ert-deftest morg-test-sync-files-dedups-and-redacts ()
  (morg-test--with-org-file books "* Books\n** Older :book:\n* Other\n"
    (morg-test--with-org-file notes ""
      (morg-test--with-server
          `(("!books:x" . (,(morg-test--event "$b1" "Dune\nhttps://example.com/dune")
                           ,(morg-test--event "$b2" "Piranesi")))
            ("!watch:x" . (,(morg-test--event "$w1" "Andor")))
            ("!notes:x" . (,(morg-test--event "$n1" "idea\n* not a heading"
                                              '(url . "mxc://x/abc")))))
        (let ((morg-rooms `(("#books:x" :file ,books :heading "Books" :tags ("book"))
                            ("!watch:x" :file ,books :heading "Watch" :tags ("movie"))
                            ("!notes:x" :file ,notes :todo "TODO")))
              (asked nil))
          ;; First run: decline redaction.
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (push p asked) nil)))
            (morg-sync))
          (should (equal asked '("Redact 4 message(s) from Matrix? ")))
          (should-not morg-test--redacted)
          (let ((text (morg-test--contents books)))
            ;; Filed after the existing child, in order, under the existing heading.
            (should (string-match-p
                     (concat "\\`\\* Books\n\\*\\* Older :book:\n"
                             "\\*\\* Dune +:book:\n:PROPERTIES:\n:MATRIX_EVENT: \\$b1\n"
                             ":MATRIX_ROOM: !books:x\n:MATRIX_SENDER: @me:x\n:END:\n"
                             "\\[2025-10-3[01] [A-Za-z]+ [0-9:]+\\]\nhttps://example.com/dune\n"
                             "\\*\\* Piranesi +:book:\n")
                     text))
            (should (string-match-p "\\*\\* Piranesi[^*]+\\* Other\n" text))
            ;; Missing heading is created at the end.
            (should (string-match-p "\n\\* Watch\n\\*\\* Andor +:movie:\n" text)))
          (let ((text (morg-test--contents notes)))
            (should (string-match-p "\\`\\* TODO idea\n" text))
            (should (string-match-p ":MATRIX_MXC: mxc://x/abc\n" text))
            (should (string-match-p "\n \\* not a heading\n" text)))

          ;; Second run: nothing new filed, everything offered for redaction again.
          (let ((before (morg-test--contents books)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (morg-sync))
            (should (equal (morg-test--contents books) before)))
          (should (equal (sort morg-test--redacted #'string<) '("$b1" "$b2" "$n1" "$w1"))))))))

(ert-deftest morg-test-redact-failure-continues ()
  (morg-test--with-server nil
    (let ((calls 0))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'morg--redact)
                 (lambda (_room _event)
                   (when (= 1 (cl-incf calls)) (user-error "HTTP 403")))))
        (morg--offer-redact '(("!r:x" . "$1") ("!r:x" . "$2")))
        (should (= calls 2))))))

;;;; Auth

(ert-deftest morg-test-request-acts-as-user ()
  (let ((morg-homeserver "https://matrix.test/")
        (morg-user "@ryan+x:matrix.test")
        (morg-token "as_tok")
        sent)
    (cl-letf (((symbol-function 'morg--http)
               (lambda (method url _data token)
                 (push (list method url token url-request-extra-headers) sent)
                 '(200 "{\"ok\": true}"))))
      (should (equal (morg--request "GET" "/rooms/!r:x/messages" nil '(("dir" . "b")))
                     '((ok . t))))
      (pcase-let ((`(,method ,url ,token) (car sent)))
        (should (equal method "GET"))
        (should (equal token "as_tok"))
        (should (equal url (concat "https://matrix.test/_matrix/client/v3/rooms/!r:x/messages"
                                   "?user_id=%40ryan%2Bx%3Amatrix.test&dir=b")))))
    (let (morg-user)
      (should-error (morg--request "GET" "/account/whoami") :type 'user-error))))

(ert-deftest morg-test-store-token-appends-and-replaces ()
  (morg-test--with-org-file auth "machine other login x password y"
    (let ((morg-auth-file auth))
      (morg--store-token "m.test" "tok1")
      (morg--store-token "m.test" "tok2")
      (should-not (find-buffer-visiting auth))
      (should (equal (morg-test--contents auth)
                     "machine other login x password y\nmachine m.test login morg password tok2\n"))
      (should (= (file-modes auth) #o600)))))

(ert-deftest morg-test-random-token ()
  (let ((a (morg--random-token)) (b (morg--random-token)))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" a))
    (should-not (equal a b))))

(ert-deftest morg-test-register ()
  (morg-test--with-org-file auth ""
    (morg-test--with-server nil
      (let ((morg-auth-file auth)
            (auth-sources (list auth))
            (morg-token nil)
            (morg-user "@ryan.c+x:matrix.test"))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (morg-register))
        (let* ((yaml (with-current-buffer "*morg-registration*" (buffer-string)))
               (token (morg--token)))
          (kill-buffer "*morg-registration*")
          (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" token))
          (should (equal (morg-test--contents auth)
                         (format "machine matrix.test login morg password %s\n" token)))
          (should (string-match-p (concat "^as_token: " token "$") yaml))
          (should (string-match-p "^hs_token: [0-9a-f]\\{64\\}$" yaml))
          (should-not (string-match-p (concat "^hs_token: " token "$") yaml))
          (should (string-match-p "^url: null$" yaml))
          (should (string-match-p "^rate_limited: false$" yaml))
          ;; The namespace regex matches exactly morg-user.
          (should (string-match-p
                   (regexp-quote "- regex: '^@ryan\\.c\\+x:matrix\\.test$'\n      exclusive: false")
                   yaml))
          ;; Registering again asks first, and declining keeps the token.
          (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
            (should-error (morg-register) :type 'user-error))
          (should (equal (morg--token) token)))))))

(ert-deftest morg-test-set-token ()
  (morg-test--with-org-file auth ""
    (morg-test--with-server nil
      (let ((morg-auth-file auth)
            (auth-sources (list auth))
            (morg-token nil))
        ;; A token the server rejects stores nothing.
        (should-error (morg-set-token "as_bad") :type 'user-error)
        (should (equal (morg-test--contents auth) ""))
        (morg-set-token "as_good")
        (should (equal (morg-test--contents auth)
                       "machine matrix.test login morg password as_good\n"))
        (should (equal (morg--token) "as_good"))
        (should (equal (morg-check) "morg: OK, acting as @ryan:matrix.test on https://matrix.test"))))))

;;; morg-test.el ends here
