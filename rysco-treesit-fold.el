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

(defun rysco-treesit-fold--defun-pred ()
  "Return the `treesit-node-match-p' predicate for foldable defuns."
  (if (and (derived-mode-p 'c-ts-base-mode)
           (consp treesit-defun-type-regexp))
      (cons (car treesit-defun-type-regexp) #'rysco-treesit-fold--c-valid-p)
    (or treesit-defun-type-regexp 'defun)))

(defun rysco-treesit-fold--defun-at-point (tactic)
  "Return the defun node at point using TACTIC."
  (treesit-thing-at-point (rysco-treesit-fold--defun-pred) tactic))

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


;; A fold hides everything from the end of NODE's first line to NODE's end.
(defun rysco-treesit-fold--beg (node)
  "Return the position where NODE's fold starts."
  (save-excursion
    (goto-char (treesit-node-start node))
    (line-end-position)))

(defun rysco-treesit-fold--own-overlay (node tactic)
  "Return NODE's own fold overlay made with TACTIC, if any.
Folds of NODE's children fall within NODE's range but start elsewhere,
so they aren't matched."
  (let ((beg (rysco-treesit-fold--beg node)))
    (seq-find (lambda (ov)
                (and (overlay-get ov 'treesit-fold)
                     (= (overlay-start ov) beg)
                     (eq (overlay-get ov 'treesit-fold-tactic) tactic)))
              (overlays-in beg (treesit-node-end node)))))

(defun rysco-treesit-fold--make (node tactic)
  "Fold NODE with a TACTIC overlay and return it."
  (let ((fold-face 'custom-variable-button)
        (ov (make-overlay (rysco-treesit-fold--beg node)
                          (treesit-node-end node) nil t nil)))
    (overlay-put ov 'treesit-fold t)
    (overlay-put ov 'treesit-fold-tactic tactic)
    (overlay-put ov 'display
                 (concat
                  " "
                  (propertize
                   (or
                    (bound-and-true-p org-ellipsis)
                    "...")
                   'face fold-face)))
    (overlay-put ov 'after-string
                 (propertize
                  "."
                  'display '(space :align-to right)
                  'face
                  fold-face))
    ov))

(defun rysco-treesit-fold--foldable-p (node)
  "Return non-nil if NODE spans more than one line."
  (< (rysco-treesit-fold--beg node) (treesit-node-end node)))

(defun rysco-treesit-fold--child-defuns (node)
  "Return the foldable defuns nearest below NODE.
Defuns nested inside those children aren't included."
  (let* ((pred (rysco-treesit-fold--defun-pred))
         (tree (treesit-induce-sparse-tree
                node (lambda (n) (treesit-node-match-p n pred t)))))
    ;; Each entry is (NODE . SUBTREE).  The top is a nil-rooted entry, so
    ;; when NODE itself matches, TREE is (nil (NODE . CHILDREN)).
    (when (and (null (car tree))
               (cadr tree)
               (treesit-node-eq (car (cadr tree)) node))
      (setq tree (cadr tree)))
    (seq-filter #'rysco-treesit-fold--foldable-p
                (mapcar #'car (cdr tree)))))

(defun rysco-treesit-fold--header-node ()
  "Return the defun whose header point is on, or signal an error."
  (let* ((pos (save-excursion (back-to-indentation) (point)))
         (node (save-excursion
                 (goto-char pos)
                 (rysco-treesit-fold--defun-at-point 'nested))))
    (unless node
      (user-error "No defun at point"))
    (unless (rysco-treesit-fold--header-p node pos)
      (user-error "Not on a defun header"))
    node))

;;;###autoload
(defun rysco-treesit-fold-toggle (arg)
  "Toggle folding for the defun at point.

The first and last line of the defun are preserved, the rest are
folded.

If called interactively with argument (ARG), unfold all folded
regions with `rysco-treesit-fold-unfold-all'

What constitutes as a defun is determined by the major mode.
This command only works in a tree-sitter major mode."
  (interactive "p")

  (if (eq arg 4)
      (rysco-treesit-fold-unfold-all)
    (let* ((tactic 'nested)
           (pos (save-excursion (back-to-indentation) (point)))
           (node (save-excursion
                   (goto-char pos)
                   (rysco-treesit-fold--defun-at-point tactic))))
      (unless node
        (user-error "No defun at point"))
      (let ((ov (rysco-treesit-fold--own-overlay node tactic)))
        (cond
         ;; If this defun has its own fold, unfold it.  A fold with a
         ;; different tactic than the one we are using now is left alone.
         (ov (delete-overlay ov))
         ;; Nested folds only start from the defun's header, so point in
         ;; a body doesn't fold the parent.
         ((and (eq tactic 'nested)
               (not (rysco-treesit-fold--header-p node pos)))
          (user-error "Not on a defun header"))
         ((not (rysco-treesit-fold--foldable-p node))
          (user-error "Defun is only one line"))
         (t (rysco-treesit-fold--make node tactic)))))))

;;;###autoload
(defun rysco-treesit-fold-toggle-children ()
  "Toggle folding for every child defun of the defun at point.
If any child is folded, unfold them all; otherwise fold them all.
Children are folded with the nested tactic, so
`rysco-treesit-fold-toggle' on a child's header unfolds just that
child.  Point must be on the parent's header."
  (interactive)
  (let* ((kids (rysco-treesit-fold--child-defuns
                (rysco-treesit-fold--header-node)))
         (folds (delq nil (mapcar (lambda (kid)
                                    (rysco-treesit-fold--own-overlay kid 'nested))
                                  kids))))
    (cond
     ((null kids) (user-error "No child defuns"))
     (folds (mapc #'delete-overlay folds))
     (t (dolist (kid kids)
          (rysco-treesit-fold--make kid 'nested))))))

;;;###autoload
(defun rysco-treesit-fold-unfold-all ()
  (interactive)
  (cl-loop
   for ov being the overlays
   when (overlay-get ov 'treesit-fold) do
   (delete-overlay ov)))

;;;;
(provide 'rysco-treesit-fold)
