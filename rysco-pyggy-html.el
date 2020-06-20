(require 'org)
(require 'ox)
(require 'ox-html)

(defvar pyggy-pygments-path "pygmentize")

(defun pyggy-org-html-code (code contents info)
  (let ((temp-source-file (make-temp-file "pyggy")))
    (with-temp-file temp-source-file
      (insert (org-element-property :value code)))

    (shell-command-to-string
     (format "%s -l \"%s\" -f html %s"
	     (executable-find pyggy-pygments-path)
	     (or (org-element-property :language code)
		 "")
	     temp-source-file))))

(org-export-define-derived-backend 'pyggy-html 'html
  :translate-alist
  '((src-block .  pyggy-org-html-code)
    (example-block . pyggy-org-html-code)))

;;;###autoload
(defun org-pyggy-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((org-html-head
         (concat
          "<style type=\"text/css\">"
          (shell-command-to-string
           (format "%s -S monokai -f html"
                   (executable-find pyggy-pygments-path)))
          "pre { border: 1px solid #ccc; box-shadow: 3px 3px 3px #eee; padding: 8pt; font-family: monospace; overflow: auto; margin: 1.2em; background: #030303;}"
          "</style>")))
    (org-export-to-file 'pyggy-html (org-export-output-file-name ".html" subtreep))))
