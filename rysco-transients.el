;;; -*- lexical-binding: t; -*-

(require 'transient)

;;;;;;;;;;;;;;;;;;;;
;; Main ryscomacs transient
(defface rysco-main-transient-title
  '((t
     :foreground "white"
     :weight normal
     :slant italic
     :height 1.2
     :underline t))
  "Face for Ryscomacs title in main transient"
  :group 'ryscomacs-faces)

(defun rysco-transient--wrap-command (name)
  (if (s-ends-with? "--suffix" (format "%s" name))
      name
    (let* ((wrapped (intern (format "%s--suffix" name)))
           (func (lambda ()
                   (interactive)
                   (call-interactively name))))
      (fset wrapped func)
      wrapped)))

(defun rysco-transient--wrap-children (children)
  (cl-loop
   for (type . data) in children
   as cmd = (rysco-transient--wrap-command (plist-get data :command))
   collect
   `(,type ,@(plist-put data :command cmd))))

(transient-define-prefix rysco-main-transient ()
  "Miscellany"
  [:description
   (lambda ()
     (concat
      (all-the-icons-faicon "registered" :face `(:inherit rysco-main-transient-title :height 0.8 :underline nil))
      (propertize " Miscellany" 'face 'rysco-main-transient-title)
      "\n"))

   ["Windows"
    :setup-children rysco-transient--wrap-children
    ("wl" "Layouts" helm-rysco-frame-layouts)
    ("wn" "Name Frame" set-frame-name)
    ("wp" "Name Frame [Project]" rysco-name-frame-project)
    ("wc" "Clone & Narrow" rysco-clone-and-narrow)
    ("wf" "Buffer Font" rysco-set-buffer-local-font)
    ("wy" "Repo path to kill ring" rysco-magit-path-to-kill-ring)]

   ["Buffer Killing"
    :setup-children rysco-transient--wrap-children
    ("kc" "Clones" rysco-kill-all-clones)
    ("ka" "All" killall)
    ("kp" "Projectile" projectile-kill-buffers)
    ("kb" "Buffer & Frame" rysco-kill-buffer-and-frame)]

   ["Agenda"
    :setup-children rysco-transient--wrap-children
    ("ta" "Agenda" rysco-org-agenda)
    ("tl" "Agenda List" org-agenda-list)
    ("tt" "Agenda Tasks" org-todo-list)
    ("ts" "Agenda Search" helm-org-ql-agenda-files)
    ("tr" "Agenda Reload Files" rysco-org-agenda-revert-files)
    ("ti" "Clock in Last" bluedot-org-clock-in-last)
    ("tj" "Jump to Clock" bluedot-org-jump-to-clock)
    ("tc" "Recapture" rysco-org-recapture-dwim)]

   ["Data Store"
    :setup-children rysco-transient--wrap-children
    ("sq" "Query" helm-rysco-store-query)
    ("sd" "Dired" rysco-store-directory-dired)
    ("sl" "Last captured" org-capture-goto-last-stored)
    ("sf" "Stamp Freshness" rysco-store-org-stamp-freshness)
    ("sc" "Check Freshness" helm-rysco-store-query-for-freshness)
    ("ss" "Store Load" rysco-store-load)
    ("sr" "HACK: Rebuild links" rysco-store-rebuild-links)]

   ["Describe"
    :setup-children rysco-transient--wrap-children
    ("dm" "Mode" describe-mode)
    ("dk" "Key Briefly" describe-key-briefly)
    ("db" "Binds" helm-descbinds)
    ("dc" "Character" describe-char)
    ("df" "Find Function" find-function)]]

  [""
   ["Utility"
    :setup-children rysco-transient--wrap-children
    ("up" "Magit Repositories" magit-list-repositories)
    ("ue" "EShell" eshell)
    ("un" "New EShell" rysco-eshell-new)
    ("ut" "Themes" rysco-load-theme)
    ("ud" "Default Theme" rysco-load-theme-default)
    ("us" "Open Current Directory (OS)" rysco-dired-os-open-dir)
    ("ur" "Agenda Rifle" helm-org-rifle-agenda-files)
    ("ul" "Process List" proced)]

   ["Packages"
    :setup-children rysco-transient--wrap-children
    ("pa" "Pull All" straight-pull-all)
    ("pr" "Rebuild All" straight-rebuild-all)
    ("pp" "Pull Package" straight-pull-package)
    ("pb" "Build Package" straight-rebuild-package)
    ("pt" "Reset to Locked" straight-thaw-versions)]

   ["Config"
    :setup-children rysco-transient--wrap-children
    ("cr" "Reload" rysco-load-local-config)
    ("ce" "Edit" rysco-edit-config)]

   ["Internet"
    :setup-children rysco-transient--wrap-children
    ("gl" "Links" helm-rysco-goto-common-links)
    ("gs" "Web Search" rysco-web-query)
    ("gw" "Wikipedia" helm-wikipedia-suggest)]

   ["Help"
    :setup-children rysco-transient--wrap-children
    ("hl" "Lossage" view-lossage)
    ("hi" "Info" helm-info)]]

  [("<SPC>" "Personal ➠" rysco-personal-transient :transient nil)])

;; Magit integration
(transient-define-prefix rysco-magit-transient ()
  "GitHub/Bitbucket helper transient for Magit"
  [["Goto"
    ("o" "Origin" rysco-magit-goto-origin)
    ("b" "Branch" rysco-magit-goto-branch)
    ("c" "Compare" rysco-magit-goto-compare)
    ("a" "Actions" rysco-magit-goto-actions)]
   ["Pull Request"
    ("r" "Requests" rysco-magit-goto-prs)
    ("p" "Create" rysco-magit-pull-request)
    ("m" "Create to master" rysco-magit-pull-request-master)]])

(with-eval-after-load 'magit
  (define-key magit-mode-map ">" 'rysco-magit-transient)
  (transient-append-suffix 'magit-dispatch
    "!"
    '(">" "Goto GH/BB" rysco-magit-transient))
  (transient-append-suffix 'magit-run
    "S"
    '("f" "with current file as %" rysco-magit-shell-command-on-file)))

;; MC
(with-eval-after-load 'multiple-cursors-core
  (add-to-list 'mc/cmds-to-run-once 'transient-quit-all))

(transient-define-prefix rysco-mc-transient ()
  "MC"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["General"
    ("n" "Next" mc/skip-to-next-like-this)
    ("m" "Mark" mc/mark-next-like-this)
    ("p" "Previous" mc/unmark-next-like-this)]
   ["Exit"
    ("<return>" "Exit" transient-quit-all)]]

  (interactive)
  (transient-setup 'rysco-mc-transient)
  (unless multiple-cursors-mode
    (with-current-buffer transient--original-buffer
      (call-interactively 'mc/mark-next-like-this))))

(defun rysco-mc-error-guard (func rest)
  (interactive "P")
  (ignore-errors
    (apply func rest)))

(advice-add 'mc/unmark-next-like-this :around 'rysco-mc-error-guard)
(advice-add 'mc/skip-to-next-like-this :around 'rysco-mc-error-guard)

;;;;;;;;;;;;;;
(provide 'rysco-transients)
