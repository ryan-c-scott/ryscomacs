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

(define-transient-command rysco-main-transient ()
  "Miscellany"
  [:description
   (lambda ()
     (concat
      (all-the-icons-faicon "registered" :face `(:inherit rysco-main-transient-title :height 0.8 :underline nil))
      (propertize " Miscellany" 'face 'rysco-main-transient-title)
      "\n"))
   ["Desktops"
    ("db" "Create" rysco-desktop+-create)
    ("dm" "Load" desktop+-load)]

   ["Windows"
    ("wc" "Clone & Narrow" rysco-clone-and-narrow)
    ("wf" "Buffer Font" rysco-set-buffer-local-font)]

   ["Buffer Killing"
    ("kc" "Clones" rysco-kill-all-clones)
    ("ka" "All" killall)
    ("kp" "Projectile" projectile-kill-buffers)
    ("ki" "IRC" kill-rcirc-buffers)]

   ["Time Management"
    ("ta" "Agenda" org-agenda)
    ("tl" "Agenda List" org-agenda-list)
    ("tt" "Agenda Tasks" org-todo-list)
    ("tr" "Agenda Reload Files" rysco-agenda-revert-files)
    ("tb" "Bluedot" bluedot)]

   ["Describe"
    ("dm" "Mode" describe-mode)
    ("dk" "Key Briefly" describe-key-briefly)
    ("db" "Binds" helm-descbinds)
    ("dc" "Character" describe-char)]]

   [""
    ["Utility"
    ("ui" "IRC" rcirc)
    ("ue" "EShell" eshell)
    ("un" "New EShell" rysco-eshell-new)
    ("ut" "Themes" rysco-load-theme)
    ("ud" "Default Theme" rysco-load-theme-default)]

   ["Packages"
    ("pa" "Pull All" straight-pull-all)
    ("pr" "Rebuild All" straight-rebuild-all)
    ("pp" "Pull Package" straight-pull-package)
    ("pb" "Build Package" straight-rebuild-package)
    ("pt" "Reset to Locked" straight-thaw-versions)]

   ["Config"
    ("cr" "Reload" rysco-load-local-config)
    ("ce" "Edit" rysco-edit-config)]

   ["Calendar"
    ("go" "Open" rysco-calendar-open)
    ("gf" "Fetch" org-gcal-fetch)
    ("gl" "Links" helm-rysco-goto-common-links)]

   ["Help"
    ("hl" "Lossage" view-lossage)
    ("hi" "Info" helm-info-info)
    ("he" "Elisp" helm-info-elisp)
    ("hc" "CL" helm-info-cl)]])

;; Magit intergation
(define-transient-command rysco-magit-transient ()
  "GitHub/Bitbucket helper transient for Magit"
  ["Goto"
   ("o" "Origin" rysco-magit-goto-origin)
   ("b" "Branch" rysco-magit-goto-branch)
   ("c" "Compare" rysco-magit-goto-compare)]
  ["Pull Request"
   ("p" "Create" rysco-magit-pull-request)])

(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map ">" 'rysco-magit-transient)
            (transient-append-suffix 'magit-dispatch
              "%"
              '(">" "Goto GH/BB" rysco-magit-transient))))


(provide 'rysco-transients)
