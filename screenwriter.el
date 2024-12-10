;;; screenwriter.el --- A major mode for editing screenplays.

;; $Id: screenwriter.el,v 1.2 2013/04/29 03:26:59 notklaatu Exp $
;; Copyright (C) 2000, 2001, 2002, 2003, 2004  Vance L. Simpson

;; Author: V. L. Simpson <vls@freeshell.org>
;; Maintainer: Klaatu <klaatu@member.fsf.i.like.sp.am.org>
;; Keywords: screenplay, screenwriter, movie

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

;; Installing and using screenwriter.el:
;; Put this file somewhere on your emacs load-path.
;; Load the file with 'load-libray RET screenwriter RET'.
;; Open up your screenplay with M-x screenwriter-mode and have at it.

;; Use the Meta key (often the "alt") to inserting and editing basic
;; screenplay elements: scene headings; action blocks; dialog blocks.
;;
;; M-s asks for and inserts a scene heading, e.g., INT. HOUSE -- DAY.
;;
;; M-a moves into action block mode, e.g., Describing the
;; house exploding.
;;
;; M-d does the dialog headings and blocks
;;         BOB
;;   Got GNU Linux installed
;;   on this computer!
;;
;; M-t does transitions, such as  CUT TO:  and FADE IN:  and  FADE OUT.
;;
;; To edit a pre-existing screenwriter element, place cursor within that
;; element and execute the appropriate command with a prefix argument:
;; .e.g., C-u M-a will reset left and right margins as needed
;; for an action block.

;; Bugs and caveats:
;;
;; Don't enter any spurious newlines when finished editing any one
;; particular element.  Just hit the key combo for the next thing you
;; want to do, e.g., (type M-s) INT. HOUSE -- DAY (Type M-a to go
;; into a action block.
;;
;; TODO
;; Make the editing commands work from any point within a screenwriter
;;  element instead of end-of-line only.
;; Get all hard coded values into def{custom,var}'s
;; Make it so that files with a .scp or .screenplay extension
;;  open in screenwriter-mode automagically
;;
;; Thanks to Frederick Brown for the margin patch to trans

;;; Code:

(defconst screenwriter-version "1.6.6"
  "Current Emacs Screenwriter Mode version number.")
(defconst screenwriter-author-name  "V.L. Simpson")
(defconst screenwriter-author-email "vls@freeshell.org")
(defconst screenwriter-web-page     "http://www.nongnu.org/screenwriter/")
(defconst screenwriter-bug-address
  "klaatu@member.fsf.org"
  "Bug reports for Screenwriter Mode go here.")

(defgroup screenwriter nil
  "Screenwriter editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "screenwriter"))

;; FIXME: Not supposed to do this but easiest way to handle filling at
;; the moment.  May implement the old re-filling code from old version.
(defcustom screenwriter-mode-hook 'auto-fill-mode
  "List of functions to call when entering Screenwriter Mode."
  :type 'hook
  :group 'screenwriter)

(defcustom screenwriter-left-margin 0
  "Left margin for scene headings and action blocks"
  :type 'integer
  :group 'screenwriter)

(defcustom screenwriter-right-margin 50
  "Right margin for scene-headings and action blocks"
  :type 'integer
  :group 'screenwriter)

;; I'll give internal variables and defuns 'scrn' prefix.
(defvar scrn-scene-hist ()
  "History list for scene headings.")

(defvar scrn-dialog-name-hist ()
  "History list for dialog block name attribute.")

(defvar scrn-transition-hist ()
  "History list for transition attribute.")

(define-derived-mode screenwriter-mode fundamental-mode "Screenwriter"
  "Major mode for editing screenwriters.
\\{screenwriter-mode-map}"
(define-key screenwriter-mode-map (kbd "M-s") 'screenwriter-slugline)
(define-key screenwriter-mode-map (kbd "M-a") 'screenwriter-action-block)
(define-key screenwriter-mode-map (kbd "M-d") 'screenwriter-dialog-block)
(define-key screenwriter-mode-map (kbd "M-t") 'screenwriter-transition)

; alternately you could center all commands around sequences of tabs
; (define-key screenwriter-mode-map "\t\r" 'screenwriter-slugline)
; (define-key screenwriter-mode-map "\t\t\r" 'screenwriter-action-block)
; (define-key screenwriter-mode-map "\t\t\t\r" 'screenwriter-dialog-block)
; (define-key screenwriter-mode-map "\t\t\t\t\r" 'screenwriter-transition)

  (make-local-variable 'scrn-scene-hist)
  (make-local-variable 'screenwriter-right-margin)
  (make-local-variable 'screenwriter-left-margin)
  (make-local-variable 'scrn-dialog-name-hist)
  )

(defun scrn-margins ()
  "Set left-margin and fill-column for slugline and action blocks."
  (setq left-margin screenwriter-left-margin)
  (setq fill-column screenwriter-right-margin))

(defun screenwriter-read-slugline ()
  "Get scene heading.
Returns scene heading in upper-case format."
  (let ((scene-heading
         (let ((prompt "Enter scene heading: "))
           (read-from-minibuffer prompt
                                 nil           ;initial-contents
                                 nil           ;keymap
                                 nil           ;read
                                 'scrn-scene-hist   ;hist
                                 nil           ;default
                                 nil))))       ;inherit-input-method
    (upcase scene-heading)))

(defun scrn-edit-slugline ()
  (cond (current-prefix-arg
         (scrn-margins)
         nil)
        (t
         (screenwriter-read-slugline))))

(defun screenwriter-slugline (scene)
  "Insert a scene heading.
To edit an existing scene heading, put the cursor on that line
and call this function with a prefix-arg, i.e, C-u M-s."
  (interactive (list (scrn-edit-slugline)))
  ;;  (interactive (list (screenwriter-read-slugline)))
  (cond ((not scene)
         nil)
        (t
         (newline 2)
         (scrn-margins)
         (indent-to-left-margin)
         (insert scene))))

(defun screenwriter-action-block ()
  "Edit a description block.
With a prefix argument, just set margins and fill-column for an
action block element."
  (interactive)
  (cond (current-prefix-arg
         (scrn-margins))
        (t
         (newline 2)
         (scrn-margins)
         (use-hard-newlines -1)
         (indent-to-left-margin))))

(defun screenwriter-dialog-char-name ()
"Return uppercase dialog block character tag."
  (let ((char-name
         (let ((prompt "Enter character name: "))
           (read-from-minibuffer prompt
                                 nil
                                 nil
                                 nil
                                 'scrn-dialog-name-hist
                                 nil
                                 nil))))
    (upcase char-name)))

(defvar screenwriter-dialog-left-margin 10)
(defvar screenwriter-dialog-right-margin 40)

(defun scrn-dialog-margins ()
  (setq left-margin screenwriter-dialog-left-margin)
  (setq fill-column screenwriter-dialog-right-margin))

(defun scrn-edit-dialog ()
  (cond (current-prefix-arg
         (scrn-dialog-margins)
         (use-hard-newlines 1 t)
         nil)
        (t
         (screenwriter-dialog-char-name))))

(defun screenwriter-dialog-block (name)
  "Edit dialog block."
  (interactive (list (scrn-edit-dialog)))
  (cond ((not name)
         nil)
        (t
         (use-hard-newlines 1 t)
         (newline 2)
         (setq left-margin 20)
         (indent-to-left-margin)
         (insert name)
         (newline 1)
         (setq left-margin 10)
         (setq fill-column 40)
         (indent-to-left-margin))))

(defun scrn-trans-margins ()
  "Set left-margin for transitions."
  (setq left-margin 35))

(defun screenwriter-read-transition ()
  "Get transition.
Returns transition in upper-case format."
  (let ((trans-name
         (let ((prompt "Enter transition: "))
           (read-from-minibuffer prompt
                                 nil           ;initial-contents
                                 nil           ;keymap
                                 nil           ;read
                                 'scrn-transition-hist   ;hist
                                 nil           ;default
                                 nil))))       ;inherit-input-method
    (upcase trans-name)))

(defun scrn-edit-transition ()
  (cond (current-prefix-arg
         (scrn-trans-margins)
         nil)
        (t
         (screenwriter-read-transition))))

(defun screenwriter-transition (trans)
  "Insert a transition.
To edit an existing transition, put the cursor on that line
and call this function with a prefix-arg, i.e, C-u M-t"
  (interactive (list (scrn-edit-transition)))
  (cond ((not trans)
         nil)
        (t
         (newline 2)
         (scrn-trans-margins)
         (indent-to-left-margin)
         (while ( < (length trans) 30 )
           (setq trans (concat " " trans)))
         (insert trans))))

(defun screenwriter-version ()
  "Display current program version in echo area."
  (interactive)
  (message "Screenwriter Mode Version %s" screenwriter-version))

(defun screenwriter-submit-bug-report ()
  "Submit a bug report for Screenwriter Mode."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   screenwriter-bug-address
   (concat "screenwriter-" screenwriter-version)
   nil
   nil
   nil
   "Please make your report as detailed as possible.
I'll try to fix it as soon as possible.

Thanks,
Klaatu
Emacs Screenwriter Mode
http://www.nongnu.org/screenwriter/"))

;;; define a screenwriter menu
  (define-key screenwriter-mode-map [menu-bar] (make-sparse-keymap))

  (let ((menuMap (make-sparse-keymap "Screenwriter")))
    (define-key screenwriter-mode-map [menu-bar screenwriter] (cons "Screenwriter" menuMap))

    (define-key menuMap [trans]
      '("Transition" . screenwriter-transition))
    (define-key menuMap [dialogue]
      '("Dialogue" . screenwriter-dialog-block))
    (define-key menuMap [action]
      '("Action" . screenwriter-action-block))
    (define-key menuMap [slug]
      '("Slug line" . screenwriter-slugline)))

(provide 'screenwriter)
;;; screenwriter.el ends here
