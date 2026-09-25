;;; treesit-fold.el --- Tree-sitter folding  -*- lexical-binding: t; -*-

;; Author: Ryan C. Scott <ryan@5pmcasual.com>
;; Original Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides a simple command ‘treesit-fold-toggle’ that
;; toggles folding for the defun at point.

;;; Code:

(require 'treesit)

(defun rysco-treesit-fold--c-valid-p (node)
  "Return non-nil if NODE is foldable in a C/C++ tree-sitter buffer.
Replaces `c-ts-mode--defun-valid-p', which rejects any struct/enum
inside a namespace (its unanchored \"declaration\" regexp matches the
namespace's \"declaration_list\" body)."
  (not (member (treesit-node-type node)
               '("expression_statement" "compound_statement"))))

(defun rysco-treesit-fold--defun-at-point (tactic)
  "Return the defun node at point using TACTIC."
  (if (and (derived-mode-p 'c-ts-base-mode)
           (consp treesit-defun-type-regexp))
      (treesit-thing-at-point
       (cons (car treesit-defun-type-regexp) #'rysco-treesit-fold--c-valid-p)
       tactic)
    (let ((treesit-defun-tactic tactic))
      (treesit-defun-at-point))))

(defun rysco-treesit-fold--header-p (node pos)
  "Return non-nil if POS is within the header lines of NODE.
The header runs from the line NODE starts on through the line holding
the opening brace of its body (or the line before the body when it
isn't brace-delimited, e.g. Python)."
  (let* ((start (treesit-node-start node))
         (body (treesit-node-child-by-field-name node "body"))
         (header-end
          (save-excursion
            (if (null body)
                (goto-char start)
              (goto-char (treesit-node-start body))
              (unless (eq (char-after) ?{)
                (forward-line -1)))
            (max (line-end-position)
                 (progn (goto-char start) (line-end-position))))))
    (<= (save-excursion (goto-char start) (line-beginning-position))
        pos
        header-end)))

;;;###autoload
(defun rysco-treesit-fold-toggle (arg)
  "Toggle folding for the defun at point.

The first and last line of the defun are preserved, the rest are
folded.

If called interactively with argument (ARG), toggle the top-level
defun. Top-level folding and non-top-level folding are on
separate channels, meaning top-level toggle wouldn’t unfold
non-top-level folding, and vice versa.

What constitutes as a defun is determined by the major mode.
This command only works in a tree-sitter major mode."
  (interactive "p")
  (let* ((fold-face 'custom-variable-button)
         (tactic (if (eq arg 4) 'top-level 'nested))
         (starting-point (point))
         (pos (save-excursion (back-to-indentation) (point)))
         (node (save-excursion
                 (goto-char pos)
                 (rysco-treesit-fold--defun-at-point tactic)))
         (start (and node (treesit-node-start node)))
         (end (and node (treesit-node-end node))))

    (if (null node)
        (user-error "No defun at point")
      (let ((indent (save-excursion
                      (goto-char start)
                      (current-indentation)))
            (beg (save-excursion
                   (goto-char start)
                   (end-of-line)
                   (point)))
            (has-fold nil))
        ;; If this defun has its own fold, unfold it.  Only match an
        ;; overlay starting at this defun's header, so a parent doesn't
        ;; unfold its folded children.  But if the folding overlay has
        ;; different tactic than the one we are using now, leave it.
        (dolist (ov (overlays-in beg end))
          (when (and (overlay-get ov 'treesit-fold)
                     (= (overlay-start ov) beg)
                     (eq (overlay-get ov 'treesit-fold-tactic)
                         tactic))
            (setq has-fold t)
            (delete-overlay ov)))

        ;; If there aren’t existing overlay with the same tactic, add
        ;; new folding.  Nested folds only start from the defun's
        ;; header, so point in a body doesn't fold the parent.
        (when (and (null has-fold)
                   (eq tactic 'nested)
                   (not (rysco-treesit-fold--header-p node pos)))
          (user-error "Not on a defun header"))
        (when (null has-fold)
          (let ((ov (make-overlay beg end nil t nil)))
            (overlay-put ov 'treesit-fold t)
            (overlay-put ov 'treesit-fold-tactic tactic)
            (overlay-put ov 'display
                         (concat
                          " "
                          (propertize
                           (or
                            (bound-and-true-p org-ellipsis)
                            "..."
                            )
                           'face fold-face)))
                          
            (overlay-put ov 'after-string
                         (propertize
                          "."
                          'display '(space :align-to right)
                          'face
                          fold-face)))

          (goto-char starting-point))))))

;;;###autoload
(defun rysco-treesit-fold-unfold-all ()
  (interactive)
  (cl-loop
   for ov being the overlays
   when (overlay-get ov 'treesit-fold) do
   (delete-overlay ov)))

;;;;
(provide 'rysco-treesit-fold)
