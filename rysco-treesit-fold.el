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
  (let* ((tactic (if (eq arg 4) 'top-level 'nested))
         (node (let ((treesit-defun-tactic tactic))
                 (treesit-defun-at-point)))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))

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
        ;; If there are existing overlays, this defun must have been
        ;; folded, then unfold it. But if the folding overlay has
        ;; different tactic than the one we are using now, leave it.
        (dolist (ov (overlays-in beg end))
          (when (and (overlay-get ov 'treesit-fold)
                     (eq (overlay-get ov 'treesit-fold-tactic)
                         tactic))
            (setq has-fold t)
            (delete-overlay ov)))

        ;; If there aren’t existing overlay with the same tactic, add
        ;; new folding.
        (when (null has-fold)
          (let ((ov (make-overlay beg end nil t nil)))
            (overlay-put ov 'treesit-fold t)
            (overlay-put ov 'treesit-fold-tactic tactic)
            (overlay-put ov 'display (concat (make-string indent ?\s)
                                             (or
                                              org-ellipsis
                                              " ..."))))
          (goto-char end)
          (forward-line 1))))))

;;;###autoload
(defun rysco-treesit-fold-unfold-all ()
  (interactive)
  (cl-loop
   for ov being the overlays
   when (overlay-get ov 'treesit-fold) do
   (delete-overlay ov)))

;;;;
(provide 'rysco-treesit-fold)
