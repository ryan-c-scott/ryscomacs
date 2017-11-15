;; helm-screenwriter.el --- Helm routines for interacting with screenwriter.el
;; Copyright (C) 2015  Ryan C. Scott

;; Author: Ryan C. Scott <ryan@5pmCasual.com>
;; Keywords: screenplay, screenwriter, movie, helm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with screenwriter.el; see the file COPYING.  If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Latest version of screenwriter.el is always available at:
;; http://www.nongnu.org/screenwriter
;; http://savannah.nongnu.org/cvs/?group=screenwriter

;; TODO:
;; .Replace the raw text used in the regexes to instead use the margin values from screenwriter-mode

;; USAGE:
;; Copy helm-screenwriter.el into a location that's in your load-path.
;; Include helm-screenwriter.el via autoload or require.
;; Add helm-screenwriter-init to the screenwriter-mode-hook will rebind M-s, M-d, and M-t setup initially in screenwriter-mode to use the 

;; (autoload 'screenwriter-mode "screenwriter" "Major mode for the screenwriter tool." t)
;; (autoload 'helm-screenwriter-init "helm-screenwriter" "Helm routines for screenwriter-mode." t)

;; ;; OR ;;
;; (require 'helm-screenwriter)

;; (add-hook 'screenwriter-mode-hook
;; 	  '(lambda ()
;;              (helm-screenwriter-init)))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar helm-screenwriter-regex-slugline "^\\([[:upper:]][[:upper:]\s\.'\"/-]+\\)$")
(defvar helm-screenwriter-regex-action "^[^[:space:]]+")
(defvar helm-screenwriter-regex-transition "^				\s*\\(.*\\)")
(defvar helm-screenwriter-regex-actor "^		    \\([^[:space:]].*?\\)\\($\\|(\\)")
(defvar helm-screenwriter-regex-dialogue "^	  [^[:space:]]")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-screenwriter-get-characters ()
  (let (res)
    (with-current-buffer helm-screenwriter-buffer
      (save-excursion
	(goto-char (point-min))
					; TODO: ignore notes
					;.Split multiple characters separated by '/'
	(while (re-search-forward helm-screenwriter-regex-actor nil t)
	  (let ((character (match-string 1)))
	    (add-to-list 'res character))))
      res)))

(defun helm-screenwriter-get-transitions ()
  (let (res)
    (with-current-buffer helm-screenwriter-buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward helm-screenwriter-regex-transition nil t)
	  (let ((transition (match-string 1)))
	    (add-to-list 'res transition))))
      res)))

(defun helm-screenwriter-get-sluglines ()
  (let (res (case-fold-search nil))
    (with-current-buffer helm-screenwriter-buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward helm-screenwriter-regex-slugline nil t)
	  (let ((slug (match-string 0)))
	    (add-to-list 'res slug))))
      res)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-scrn-character-source
      `(((name . "Screenwriter Characters")
	(init . (lambda ()
		  (helm-set-local-variable 'helm-screenwriter-buffer (current-buffer))))
	(candidates . helm-screenwriter-get-characters)
	(action . (("Use" . (lambda (candidate) (screenwriter-dialog-block candidate))))))

	;;
	((name . "Screenwriter Create Character")
	 (dummy)
	 (action . (("Create" . (lambda (candidate) (screenwriter-dialog-block (upcase candidate)))))))))

(setq helm-scrn-transition-source
      `(((name . "Screenwriter transitions")
	(init . (lambda ()
		  (helm-set-local-variable 'helm-screenwriter-buffer (current-buffer))))
	(candidates . helm-screenwriter-get-transitions)
	(action . (("Use" . (lambda (candidate) (screenwriter-transition candidate))))))

	;;
	((name . "Screenwriter Create Transition")
	 (dummy)
	 (action . (("Create" . (lambda (candidate) (screenwriter-transition (upcase candidate)))))))))

(setq helm-scrn-slug-source
      `(((name . "Screenwriter slugs")
	(init . (lambda ()
		  (helm-set-local-variable 'helm-screenwriter-buffer (current-buffer))))
	(candidates . helm-screenwriter-get-sluglines)
	(action . (("Use" . (lambda (candidate) (screenwriter-slugline candidate))))))

	;;
	((name . "Screenwriter Create Slugline")
	 (dummy)
	 (action . (("Create" . (lambda (candidate) (screenwriter-slugline (upcase candidate)))))))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helm-screenwriter-display-official ()
  (interactive)
  (buffer-face-set '(:family "Courier new" :height 120)))

(defun helm-screenwriter-display-normal ()
  (interactive)
  (buffer-face-set))

(defun helm-screenwriter-dialog-block ()
  (interactive)
  (helm :sources helm-scrn-character-source))
  
(defun helm-screenwriter-transition ()
  (interactive)
  (helm :sources helm-scrn-transition-source))

(defun helm-screenwriter-slugline ()
  (interactive)
  (helm :sources helm-scrn-slug-source))

(defun helm-screenwriter-guess-margins ()
  (interactive)
  ;; Do regex matches to determine which margin function to call from screenwriter
  (let ((line (thing-at-point 'line t)) case-fold-search)
    
    (cond
     ((string-match helm-screenwriter-regex-slugline line)
      (message "Found: Slugline")
      (scrn-margins)
      (back-to-indentation))

     ((string-match helm-screenwriter-regex-action line)
      (message "Found: action")
      (scrn-margins)
      (back-to-indentation))

     ((string-match helm-screenwriter-regex-transition line)
      (message "Found: Transition")
      (scrn-trans-margins)
      (back-to-indentation))
     
     ((string-match helm-screenwriter-regex-actor line)
      (message "Found: Actor")
      (scrn-dialog-margins)
      (setq left-margin 20)
      (back-to-indentation))

     ((string-match helm-screenwriter-regex-dialogue line)
      (message "Found: Dialogue")
      (scrn-dialog-margins)
      (back-to-indentation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-screenwriter-highlights
      `((,helm-screenwriter-regex-actor . font-lock-function-name-face)
	(,helm-screenwriter-regex-slugline . font-lock-keyword-face)
	(,helm-screenwriter-regex-transition . font-lock-constant-face)))


(defun helm-screenwriter-init ()
  (auto-fill-mode)

  (setq font-lock-defaults '(helm-screenwriter-highlights))
  (font-lock-mode)
  
  (local-set-key (kbd "M-i") 'helm-screenwriter-guess-margins)
  (local-set-key (kbd "M-s") 'helm-screenwriter-slugline)
  (local-set-key (kbd "M-d") 'helm-screenwriter-dialog-block)
  (local-set-key (kbd "M-t") 'helm-screenwriter-transition))

  
