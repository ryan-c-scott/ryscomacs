;;;;;;;;;
(cl-defmacro rysco-bind-keys (lead &rest bindings)
  `(progn
     ,@(cl-loop
        for (key bind) in bindings collect
        `(global-set-key
          (kbd (concat ,lead " " ,key))
          ,bind))))

(cl-defmacro rysco-auto-modes (list &rest elements)
  `(progn
     ,@(cl-loop
        for entry in elements collect
        `(add-to-list 'auto-mode-alist ',entry))))

(cl-defmacro rysco-add-to-list (list &rest entries)
  `(progn
     ,@(reverse
        (cl-loop
         for it in entries collect
         `(add-to-list ',list ',it)))))

(cl-defmacro rysco-add-to-loadpath (&rest pathlist)
  `(progn
     ,@(reverse
        (cl-loop
         with base = nil
         for path in pathlist
         if (and (listp path) (equal (car path) :base)) do
         (setq base (cadr path))
         else collect
         `(add-to-list 'load-path ,(concat base "/" path))))))

(cl-defmacro rysco-autoloads (&rest entries)
  `(progn
     ,@(cl-loop
        for (sym file doc) in entries collect
        `(autoload ',sym ,file ,doc t))))

(cl-defmacro rysco-packages (&key packages)
  (cl-loop
   with loads
   for pkg in packages
   collect `(straight-use-package ',pkg) into loads
   finally return
   `(progn
      (message "Loading straight.el packages:")
      (let ((straight-current-profile 'rysco))
        ,@loads))))

(cl-defmacro rysco-exec-path (&rest paths)
  `(prog1 nil
     ,@(cl-loop
        with path-form
        with path-env
        for p in paths
        collect `(add-to-list 'exec-path ,p) into path-form
        concat (format
                "%s%s"
                p
                (if (eq system-type 'windows-nt)
                    ";"
                  ":"))
        into path-env
        finally return
        (prog1 path-form
          (setenv "PATH" (concat path-env (getenv "PATH")))))))
  
(defun rysco-semantic-mode (&optional state)
  (if state
      (with-demoted-errors
          (semantic-mode t))
    (semantic-mode -1)))

;;;;;;;;;
(defun rysco-split-dwim (dir &optional switch)
  ""
  (let* ((can-horizontal (not (or (windmove-find-other-window 'left) (windmove-find-other-window 'right))))
         (window-below (windmove-find-other-window 'down))
         (can-vertical (not (or (windmove-find-other-window 'up) (and window-below (not (window-minibuffer-p window-below)))))))

    (cond ((and (or (eq dir 'left) (eq dir 'right)) can-horizontal) (split-window-right))
          ((and (or (eq dir 'up) (eq dir 'down)) can-vertical) (split-window-below)))

         (windmove-do-window-select dir)))

(defun rysco-split-up-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'up t))

(defun rysco-split-down-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'down t))

(defun rysco-split-right-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'right t))

(defun rysco-split-left-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'left t))

(cl-defun rysco-delete-or-clone-window-dwim (arg)
  (interactive "P")
  (if arg
      (rysco-clone-and-narrow)
    (delete-window)))

(cl-defun rysco-delete-or-kill-other-windows-dwim (arg)
  (interactive "P")
  (if arg
      (rysco-kill-all-clones)
    (delete-other-windows)))

(cl-defun rysco-clone-and-narrow ()
  (interactive)
  (let ((master
         (or
          (and (boundp 'rysco-cloned-from) rysco-cloned-from)
          (current-buffer))))

    (with-current-buffer (clone-indirect-buffer-other-window nil t)
      (setq-local rysco-cloned-from master)
      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (pcase major-mode
          ('org-mode (org-narrow-to-subtree))
          (_ (narrow-to-defun)))))))

(cl-defun rysco-kill-all-clones ()
  (interactive)
  (cl-loop
   with kill-list
   with get-master = (lambda (&optional buf)
                       (with-current-buffer (or buf (current-buffer))
                         (or
                          (and (boundp 'rysco-cloned-from) rysco-cloned-from)
                          (current-buffer))))
   with master = (funcall get-master)
   for buf being the buffers
   as this-master = (funcall get-master buf)
   if (and this-master
           (not (equal buf master))
           (equal this-master master))
   collect buf into kill-list

   finally do
   (progn
     (message "Killing %s clones %s" master kill-list)
     (cl-loop
      for buf in kill-list do
      (kill-buffer buf)))))

(defun rysco-comment-dwim (arg)
  ""
  (interactive "*P")
  (if mark-active
      (progn
        (when arg
          (kill-ring-save (region-beginning) (region-end)))
        (comment-dwim nil))
    (comment-line 1)
    (forward-line -1)))

(defun rysco-clear-undo ()
  ""
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun rysco-set-buffer-local-font (&optional font-family)
  "Sets font in current buffer"
  (interactive)
  (let ((font-family (or font-family
                         (helm :sources `(,(helm-build-sync-source "Font Families"
                                             :candidates (font-family-list)))))))
    (buffer-face-set `(:family ,font-family))))

(defun rysco-toggle-writing-face ()
  (interactive)
  (if (and (boundp 'buffer-face-mode) buffer-face-mode)
      (buffer-face-mode -1)
    (rysco-set-buffer-local-font rysco-writing-font)))

(defun replace-regexp-and-return (from to)
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun rysco-calendar-exit-and-insert-date (arg)
  "Capture the date at point, exit the Calendar, and insert the date using 'org-date-from-calendar."
  (interactive "P")
  (when (equal major-mode 'calendar-mode)
    (calendar-exit)
    (if (or arg (equal major-mode 'org-mode))
        (org-date-from-calendar)
      (seq-let (month day year) (org-get-date-from-calendar)
        (insert (format "%d-%02d-%02d" year month day))))))

(defun rysco-compile()
  "Byte compiles all of ~/ryscomacs/elisp."
  (interactive)
  (byte-recompile-directory "~/ryscomacs/elisp" 0))

(cl-defun helm-rysco-rotate-windows ()
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "Layouts"
     :candidates
     '(("even-horizontal" . rotate:even-horizontal)
       ("even-vertical" . rotate:even-vertical)
       ("main-horizontal" . rotate:main-horizontal)
       ("main-vertical" . rotate:main-vertical)
       ("tiled" . rotate:tiled)
       ("*next*" . rotate:layout))
     :action
     (lambda (func)
       (funcall-interactively func)))))

(cl-defun rysco-rotate-windows (arg)
  (interactive "P")
  (if arg
      (helm-rysco-rotate-windows)
    (--when-let (selected-window)
      (rotate-window)
      (select-window it))))

(defun vertical-windows-with-related()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (ff-find-related-file t t))

(defun ff-find-related-file-ignore-include()
  (interactive)
  (ff-find-related-file nil t))

(defun kill-all-matching (criteria)
  "Kills all buffers that the criteria function returns non-nil on"
  (mapc (lambda (buffer)
	  (when (funcall criteria buffer)
	    (kill-buffer buffer)))
	(buffer-list)))

(defun kill-all-matching-prefix (buffer-prefixes)
  (interactive "s")
  (kill-all-matching (lambda (buffer)
                       (let ((names buffer-prefixes)
                             (bufname (buffer-name buffer))
                             (found))
                         (while names
                           (if (string-match (car names) bufname)
                               (progn
                                 (setq found t)
                                 (setq names nil))
                             (setq names (cdr names))))
                         found))))

(defun killall()
  "Kill all non-system buffers"
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (not (string-match "^*" (buffer-name buffer)))))
  (when (fboundp 'dvc-kill-all-buffers)
    (dvc-kill-all-buffers))
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun kill-dired-buffers ()
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (eq 'dired-mode (buffer-local-value 'major-mode buffer)))))

(defun kill-rcirc-buffers ()
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (eq 'rcirc-mode (buffer-local-value 'major-mode buffer)))))

(defun rysco-revert-buffer ()
    "Revert buffer, prompting if it has been modified."
    (interactive)
    (--when-let (buffer-file-name)
      (let ((current-pos (point)))
        (when (find-alternate-file it)
          (goto-char current-pos)))))

(defun markdown-toc ()
  (interactive)
  (let (res depth title)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\(#*\\) \\(.*\\)$" nil t)
	(add-to-list 'res `(,(match-string 1) ,(match-string 2)))))

    (dolist (elt (reverse res))
      (setq depth (replace-regexp-in-string (regexp-quote "#") (car elt) "\t" nil 'literal))
      (setq title (cadr elt))

      (insert (format "%s1. [%s]" depth title))
      (setq title (replace-regexp-in-string "\s+" "-" title))
      (setq title (replace-regexp-in-string "[:*]" "" title))
      (insert (format "(#%s)\n" (downcase title))))))

(defun markdown-insert-youtube (id &optional label)
  (interactive "sID: \nsLabel: ")
  (insert (format "[![%s](http://img.youtube.com/vi/%s/0.jpg)](http://www.youtube.com/watch?v=%s)" label id id)))

(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ DONE" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ DONE" nil t)
            (org-advertized-archive-subtree))
          (org-display-all-todo-item)
          (message "Archive finished"))
      (org-display-all-todo-item)
      (message "No need to archive"))))

(defun rysco-org-export-writing-outline (&optional arg)
  "Exports the current org file to a master export (in org format) by traversing any headings tagged `:outline:' and outputing `\#+INCLUDE:' for every linked file found.  This is for ease of moving around sections and interspersing those references with notes as needed.
Normally the outline would also be tagged `:noexport:' so that it will be excluded from the output."
  (interactive "P")
  (require 'ox)
  
  (let (content)
    (setq content (buffer-string))
    
    (with-temp-buffer
      (insert content "\n")
      (goto-char (point-max))
      
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (el)
          (prog1 nil
            (when (member "outline" (org-element-property :tags (org-element-lineage el '(headline))))
              (insert (format "#+INCLUDE: \"%s%s\" :minlevel 1\n"
                              (org-element-property :path el)
                              (--if-let (org-element-property :search-option el)
                                  (concat "::" (org-link-unescape it))
                                "")))))))

      (org-mode)
      (org-export-expand-include-keyword)

      (if (not arg)
          (write-file (org-export-output-file-name ".org"))
        (let ((content (buffer-string))
              (buffer (get-buffer-create "*Writing Outline Master Export*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert content)
            (org-mode)
            (read-only-mode)
            (goto-char (point-min)))
          (switch-to-buffer-other-window buffer))))))

(defun edit-local-config()
  "Open ~/.emacs.d/elisp/localconfig.el"
  (interactive)
  (find-file "~/.emacs.d/elisp/localconfig.el"))

(defun rysco-generate-ssh-config ()
  "Generates ssh config file from snippets in directories specified in 'rysco-ssh-config-directories"
  (interactive)
  (let* ((out (find-file "~/.ssh/config"))
	 (directories
	  (cl-remove-if-not 'file-exists-p
			 (append '("~/.ssh/config.d/") rysco-ssh-config-directories))))
    (backup-buffer)
    (erase-buffer)

    (insert (format "### Generated by Ryscomacs.  Do not edit ###
### See snippets in %s\n\n" directories))
    
    (dolist (path directories)
      (dolist (include (directory-files path t))
	(when (not (file-directory-p include))
	  (insert (format "### %s\n" include))
	  (insert
	   (with-temp-buffer
	     (insert-file-contents include)

	     (if (equal "el" (file-name-extension include))
		 (eval (car
			(read-from-string
			 (concat "(with-output-to-string " (buffer-string) ")"))) t)
	     (buffer-string)))))))
    (save-buffer)
    (kill-buffer)))

(defun rysco-make-buffer-utf8 (&optional buf)
  "Tidy up a buffer by replacing all special Unicode characters
   (smart quotes, etc.) with their more sane cousins"
  (interactive)
 
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("[\u2013\|\u2014]" . "-")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
 
    (save-excursion
      (cl-loop for (key . value) in unicode-map do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun rysco-repo-status (&optional dir)
  "Run either `monky-status' or `magit-status' for hg or git repositories respectively"
  (interactive)
  (helm :sources
        (helm-build-sync-source "Projects"
          :candidates (cons "." projectile-known-projects)
          :action
          (lambda (dir)
            (cond
             ((--when-let (vc-find-root dir ".hg")
                (monky-status it)))
             ((--when-let (vc-find-root dir ".git")
                (magit-status it)))
             (t
              (message "Not a git or hg repository")))))))

(defun color-invert (color)
  (cl-loop for el in (if (stringp color) (color-name-to-rgb color) color)
           collect (- 1 el)))

(defun color-lerp (start end mix)
  (cl-loop for scol in (if (stringp start) (color-name-to-rgb start) start)
           for ecol in (if (stringp end) (color-name-to-rgb end) end)
           collect (+ (* scol (- 1 mix))
                      (* ecol mix))))

(defun color-ramp (start end steps)
  (cl-loop
   with start = (color-name-to-rgb start)
   with end = (color-name-to-rgb end)
   for i from 0 to (1- steps)
   collect
   (apply
    'color-rgb-to-hex
    (color-lerp start end (/ (float i) steps)))))

(defun int-to-bin-string (val &optional pad set-char unset-char)
  (let ((pad (or pad 1))
        (count (logb val))
        (set-char (or set-char "1"))
        (unset-char (or unset-char "0")))

    (cl-loop
     with out
     for pos from 0 to count concat
     (prog1 (if (= (logand 1 val) 1) set-char unset-char)
       (setq val (lsh val -1)))
     into out
     finally return
     (let ((out-length (length out)))
       (concat
        (make-string (max 0 (- (or pad out-length) out-length)) (string-to-char unset-char))
        (reverse out))))))

(defun pivot-table-columns (data &optional add-hlines)
  (let ((result
         (cl-loop
          for i from 0 below (length (car data)) collect
          (-select-column i data))))
    (if add-hlines
        (add-table-hlines result)
      result)))

(defun add-table-hlines (tbl)
  `(hline
    ,(car tbl)
    hline
    ,@(cdr tbl)
    hline))

(defun rysco-ido-fix ()
  (interactive)
  "Enable ido-mode with 'files option.
For some reason, currently, ido-mode will get disabled..."
  (ido-mode 'files))

;;;###autoload
(defun helm-rysco-project-ag (arg)
  "Preconfigured helm for grepping with AG in (projectile-project-root).
With prefix-arg prompt for type if available with your AG version."
  (interactive "P")
  (--when-let (projectile-project-root)
    (require 'helm-files)
    (helm-grep-ag it arg)))

(defun helm-rysco-occur-or-resume (arg)
  (interactive "P")
  (if arg
      (helm-resume nil)
    (helm-occur)))

(defun rysco-name-frame-project ()
  (interactive)
  (let ((name (projectile-project-name)))
    (if (string= name "-")
        (--when-let (read-string "Name: ")
          (set-frame-name it))
      (set-frame-name (format "[ %s ]" (upcase name))))))

(defun rysco-frame-by-name (arg)
  (interactive "P")
  (if arg
      (call-interactively 'rysco-name-frame-project arg)
    (call-interactively 'select-frame-by-name)))

(defun helm-rysco-semantic-or-imenu (arg)
  (interactive "P")
  (if arg
      (call-interactively 'helm-imenu-in-all-buffers)
    (call-interactively 'helm-semantic-or-imenu)))

(defvar helm-rysco-insert-icon--last nil)

(defun helm-rysco-insert-icon (arg)
  "Helper for discovering fonts from all-the-icons"
  (interactive "P")

  (when arg
    (setq helm-rysco-insert-icon--last nil))

  (helm :candidate-number-limit nil
        :preselect helm-rysco-insert-icon--last
        :sources
        (cl-loop for family in '(alltheicon faicon fileicon octicon material wicon) collect
                 (helm-build-sync-source (format "%s" family)
                   :candidates
                   (let* ((data-f (all-the-icons--data-name family))
                          (insert-f (all-the-icons--function-name family))
                          (data (funcall data-f)))
                     (mapcar
                      (lambda (it)
                        (let ((entry
                               `(,(format "%s - %s"
                                          (funcall
                                           insert-f
                                           (car it))
                                          (car it))
                                 ,family
                                 ,(car it)
                                 ,(funcall
                                   insert-f
                                   (car it)))))
                                                                  
                          (cons
                           (car entry)
                           entry)))
                      data))
                   
                   :action
                   `(("Insert Icon & Label" .
                      (lambda (item)
                        (cl-loop
                         for (label . parts) in (helm-marked-candidates :all-sources t) do
                         (progn
                           (setq helm-rysco-insert-icon--last label)
                           (insert label "\n")))))

                     ("Insert Icon Only" .
                      (lambda (item)
                        (cl-loop
                         for (label _ _ icon) in (helm-marked-candidates :all-sources t) do
                         (progn
                           (setq helm-rysco-insert-icon--last label)
                           (insert icon "\n")))))
                        
                     ("Insert Code" .
                      (lambda (item)
                        (cl-loop
                         for (label family name icon) in (helm-marked-candidates :all-sources t) do
                        (insert
                         (format "(all-the-icons-%s \"%s\")\n" family name))))))))))

(defun rysco-load-theme (&optional theme)
  (interactive)
  (let ((current (copy-list custom-enabled-themes)))
    (--if-let theme
        (load-theme theme)
      (call-interactively 'load-theme))
    
      (cl-loop for current-theme in current do
               (disable-theme current-theme))))

(cl-defun rysco-eshell-new ()
 "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(cl-defun rysco-eshell-rename (name)
  "Open a new instance of eshell."
  (interactive "sName: ")
  (rename-buffer (format "*eshell <%s>*" name)))

(defun rysco-eshell-prompt ()
  (cl-loop
   with vc-backend = (when (fboundp 'vc-deduce-backend)
                       (vc-deduce-backend))
   with default-face = '(:height 0.9 :background "DarkSlateGray" :foreground "white" :box nil)
   with dir-face = '(:foreground "salmon" :background "gray22")
   with vc-face = `(:foreground "Gray47" :background "gray22" :height 0.8)
   with components = `("["
                       ,(format-time-string "%H:%M ")
                       (,(eshell/whoami) . (:inherit ,default-face :foreground "darkgoldenrod1" :slant italic :weight bold))
                       "@"
                       ,(system-name)
                       "]"
                       (" " . ,dir-face)
                       (,(--when-let vc-backend
                           (cond
                            ((equal it 'Hg)
                             (all-the-icons-faicon "mercury" :face vc-face))
                            ((equal it 'Git)
                             (all-the-icons-alltheicon "git" :face vc-face))))
                        . nil)
                       (,(when vc-backend " ") . ,dir-face)
                       (,(s-replace (expand-file-name "~") "~" (eshell/pwd)) . ,dir-face)
                       (" " . ,dir-face)
                       ("$" . ,dir-face)
                       (" " . nil))

                       for piece in components if (or
                                                   (stringp piece)
                                                   (and (listp piece) (car piece)))
                       concat
                       (cond
                        ((consp piece)
                         (--if-let (cdr piece)
                             (propertize (car piece) 'face it)
                           (car piece)))
                        (t
                         (propertize piece 'face default-face)))))

(cl-defstruct rysco-magit-origin host host-type path url)

(defun rysco-magit-get-origin (&optional origin-url)
  (let* ((origin-url (or origin-url (magit-get "remote" "origin" "url")))
         (parts (cdr (s-match "\\([^@]+\\)@\\([^:]+\\):\\(.*\\).git" origin-url)))
         (user (first parts))
         (host (nth 1 parts))
         (path (nth 2 parts)))
    (make-rysco-magit-origin
     :host host
     :host-type (pcase host
                  ("github.com" 'gh)
                  ("bitbucket.org" 'bb))
     :path path
     :url (format "https://%s/%s" host path))))

(defun rysco-magit-goto-compare (&optional head base)
  (interactive)
  (let* ((origin (rysco-magit-get-origin))
         (url (rysco-magit-origin-url origin))
         (source
          `(,(helm-build-sync-source "Comman Branches"
               :candidates '("master"))
            ,(helm-build-sync-source "Other Branches"
               :candidates
               (cl-loop for branch in (magit-list-remote-branch-names) collect
                        (substring branch (length "origin/"))))))
         (head (or head (helm :sources source)))
         (base (or base (when head (helm :sources source)))))
    (when (and head base)
      (browse-url
       (pcase (rysco-magit-origin-host-type origin)
         ('gh (format "%s/compare/%s...%s?expand=1" url base head))
         ('bb (format "%s/branches/compare/%s..%s" url base head)))))))

(defun rysco-magit-pull-request ()
  (interactive)
  (rysco-magit-goto-compare (magit-get-current-branch)))

(defun rysco-magit-goto-branch ()
  (interactive)
  (let* ((origin (rysco-magit-get-origin))
         (url (rysco-magit-origin-url origin)))

    (browse-url
     (pcase (rysco-magit-origin-host-type origin)
       ('gh (format "%s/tree/%s" url (magit-get-current-branch)))
       ('bb (format "%s/branch/%s" url (magit-get-current-branch)))))))

(defun rysco-magit-goto-origin ()
  (interactive)
  (browse-url
   (rysco-magit-origin-url
    (rysco-magit-get-origin))))

(defun rysco-export-org-to-pdf ()
  "Searches for a file named `.pdf-master' in ancestor directories and uses the file name specified inside of that file as the file to open and export using `org-latex-export-to-pdf'"
  (interactive)
  (--if-let (locate-dominating-file (buffer-file-name) ".pdf-master")
      (let ((path
             (concat
              it
              (with-temp-buffer
                (insert-file-contents (concat it ".pdf-master"))
                (s-trim
                 (buffer-string))))))
        (message "Found .pdf-master in %s.  Exporting." it)
        (with-current-buffer (find-file path)
          (let ((org-confirm-babel-evaluate nil))
            (org-latex-export-to-pdf))))
    (message "No .pdf-master found in directory ancestors.")))

(defun rysco-simple-graph (patch &optional filename)
  (-let ((temp-path (make-temp-file "patch" nil ".dot"))
         (color-cache (make-hash-table :test 'equal)))
    (with-temp-file temp-path
      (insert "digraph patch {\n"
              "node  [style=\"rounded,filled,bold\", shape=box, fixedsize=true, width=1.3, fontname=\"Arial\"];\n")

      (cl-loop
       for (mod . connections) in patch
       do (insert (format "\"%s\";\n" mod)))

      (cl-loop
       for (mod . connections) in patch do
       (cl-loop
        for dest in connections do
        (insert
         (format "\"%s\" -> " mod)
         (pcase dest
           (`(,dest ,comment)
            (let ((color (gethash comment color-cache)))
              (unless color
                (setq color
                      (apply
                       'color-rgb-to-hex
                       `(,@(color-hsl-to-rgb
                            (cl-random 1.0)
                            (cl-random 1.0)
                            0.5)
                         2)))
                (puthash comment color color-cache))
              (format "\"%s\" [label=\"%s\", color=\"%s\", fontcolor=\"%s\",]" dest comment color color)))
           (dest
            (format "\"%s\"" dest)))
         ";\n")))

      (insert "\n}\n"))

    (-let ((filename (or filename
                         (when (boundp 'out)
                           out)
                         (when (equal major-mode 'org-mode)
                           (concat
                            (replace-regexp-in-string
                             "[/\\]" "-"
                             (s-join "-" (org-get-outline-path t)))
                            ".png"))))
           (out-path (format "%s.png" (file-name-sans-extension temp-path)))
           (command-result (string-trim
                            (shell-command-to-string
                             (graphviz-compile-command temp-path)))))
      (if (string-prefix-p "Error:" command-result)
          (message command-result)

        ;; Delete temp file?
        (rename-file out-path filename t)
        filename))))

;;
(provide 'rysco-util)