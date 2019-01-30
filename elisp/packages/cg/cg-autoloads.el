;;; cg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "cg" "cg.el" (0 0 0 0))
;;; Generated autoloads from cg.el

(defvar cg-command "vislcg3" "\
The vislcg3 command, e.g. \"/usr/local/bin/vislcg3\".

Buffer-local, so use `setq-default' if you want to change the
global default value.

See also `cg-extra-args' and `cg-pre-pipe'.")

(custom-autoload 'cg-command "cg" t)

(defvar cg-extra-args "--trace" "\
Extra arguments sent to vislcg3 when running `cg-check'.

Buffer-local, so use `setq-default' if you want to change the
global default value.

See also `cg-command'.")

(custom-autoload 'cg-extra-args "cg" t)

(defvar cg-pre-pipe "cg-conv" "\
Pipeline to run before vislcg3 when testing a file with `cg-check'.

Buffer-local, so use `setq-default' if you want to change the
global default value.  If you want to set it on a per-file basis,
put a line like

# -*- cg-pre-pipe: \"lt-proc foo.bin | cg-conv\"; othervar: value; -*-

in your .cg3/.rlx file.

See also `cg-command' and `cg-post-pipe'.")

(custom-autoload 'cg-pre-pipe "cg" t)

(defvar cg-post-pipe "" "\
Pipeline to run after vislcg3 when testing a file with `cg-check'.

Buffer-local, so use `setq-default' if you want to change the
global default value.  If you want to set it on a per-file basis,
put a line like

# -*- cg-post-pipe: \"cg-conv --out-apertium | lt-proc -b foo.bin\"; -*-

in your .cg3/.rlx file.

See also `cg-command' and `cg-pre-pipe'.")

(custom-autoload 'cg-post-pipe "cg" t)

(defvar cg-indentation 8 "\
The width for indentation in Constraint Grammar mode.")

(custom-autoload 'cg-indentation "cg" t)

(autoload 'cg-mode "cg" "\
Major mode for editing Constraint Grammar files.

CG-mode provides the following specific keyboard key bindings:

\\{cg-mode-map}

\(fn)" t nil)

(defvar cg-check-do-cache t "\
If non-nil, `cg-check' caches the output of `cg-pre-pipe' (the
cache is emptied whenever you make a change in the input buffer,
or call `cg-check' from another CG file).")

(custom-autoload 'cg-check-do-cache "cg" t)

(defvar cg-per-buffer-input nil "\
If this is non-nil, the input buffer created by
`cg-edit-input' will be specific to the CG buffer it was called
from, otherwise all CG buffers share one input buffer.")

(custom-autoload 'cg-per-buffer-input "cg" t)

(defvar cg-output-setup-hook nil "\
List of hook functions run by `cg-output-process-setup'.
See `run-hooks'.")

(custom-autoload 'cg-output-setup-hook "cg" t)

(defvar cg-check-after-change nil "\
If non-nil, run `cg-check' on grammar after each change to the buffer.")

(custom-autoload 'cg-check-after-change "cg" t)

(defvar cg-check-after-change-secs 1 "\
Minimum seconds between each `cg-check' after a change to a CG buffer.
Use 0 to check immediately after each change.")

(custom-autoload 'cg-check-after-change-secs "cg" t)

(add-to-list 'auto-mode-alist '("\\.cg3\\'" . cg-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cg" '("cg-")))

;;;***

(provide 'cg-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cg-autoloads.el ends here
