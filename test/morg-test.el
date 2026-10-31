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
   ((equal path "/login")
    (if (equal (alist-get 'password data) "right")
        '((user_id . "@ryan:x") (access_token . "syt_new"))
      (user-error "morg: POST /login failed (HTTP 403): M_FORBIDDEN")))
   ((equal path "/account/whoami")
    (if (equal token "syt_old") '((user_id . "@ryan:x")) (user-error "HTTP 401")))
   ((equal path "/logout") nil)
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

;;;; Login

(ert-deftest morg-test-store-token-appends-and-replaces ()
  (morg-test--with-org-file auth "machine other login x password y"
    (let ((morg-auth-file auth))
      (morg--store-token "m.test" "tok1")
      (morg--store-token "m.test" "tok2")
      (should-not (find-buffer-visiting auth))
      (should (equal (morg-test--contents auth)
                     "machine other login x password y\nmachine m.test login morg password tok2\n"))
      (should (= (file-modes auth) #o600)))))

(ert-deftest morg-test-login ()
  (morg-test--with-org-file auth ""
    (morg-test--with-server nil
      (let ((morg-auth-file auth)
            (auth-sources (list auth))
            (morg-token nil)
            (passwords '("wrong" "right")))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "ryan"))
                  ((symbol-function 'read-passwd) (lambda (&rest _) (copy-sequence (pop passwords))))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          ;; A bad password stores nothing.
          (should-error (morg-login) :type 'user-error)
          (should (equal (morg-test--contents auth) ""))
          ;; The password goes to /login unauthenticated; only the token is stored.
          (morg-login)
          (should (equal (morg-test--contents auth)
                         "machine matrix.test login morg password syt_new\n"))
          (let ((login (assoc "POST" morg-test--requests)))
            (should (eq (nth 4 login) :none))
            (should (equal (alist-get 'initial_device_display_name (nth 2 login)) "morg")))
          (should (equal (morg--token) "syt_new"))
          ;; Logging in again replaces the old token and logs its session out.
          (let ((morg-token "syt_old"))
            (setq passwords '("right")
                  morg-test--requests nil)
            (morg-login)
            (should (member '("POST" "/logout") (mapcar (lambda (r) (seq-take r 2))
                                                        morg-test--requests)))))))))

;;; morg-test.el ends here
