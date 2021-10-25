;;; -*- lexical-binding: t; -*-

(require 'dash)

;;;;;; Graphing utilities
(require 'graphviz-dot-mode)

(cl-defun rysco-graph--render-guess-filename (&optional ext)
  (when (boundp 'out)
    out)
  (when (equal major-mode 'org-mode)
    (concat
     (replace-regexp-in-string
      "[/\\:]" "-"
      (s-join "-" (org-get-outline-path t)))
     (or ext ".svg"))))

(cl-defun rysco-graph--render-generate-color (&optional rand-state)
  (apply
   'color-rgb-to-hex
   `(,@(color-hsl-to-rgb
        (cl-random 1.0 rand-state)
        (cl-random 1.0 rand-state)
        0.5)
     2)))

(defun rysco-graph--render-layer-visible (all-layers obj-layer)
  (or (or (eq all-layers nil)
          (eq obj-layer nil))
      (-let [current-layers (split-string
                             (format "%s" obj-layer)
                             "[:]"
                             t
                             "[ ]+")]
        (--any? (cl-member it all-layers :test 'string-equal) current-layers))))

(cl-defun rysco-graph--render-plist-to-settings (data &optional id color-cache rand-state layers ignore)
  (loop
   with out
   with obj-layer = (plist-get data :layer)
   with visible = (rysco-graph--render-layer-visible layers obj-layer)

   for k in data by 'cddr
   as k = (format "%s" k)
   for v in (cdr data) by 'cddr
   as v = (pcase v
            ('RCOL (rysco-graph--render-generate-color rand-state))
            ('UCOL
             (when color-cache
               (--if-let (gethash id color-cache)
                   it
                 (puthash
                  id
                  (setq color
                        (rysco-graph--render-generate-color rand-state))
                  color-cache))))
            ((pred listp)
             (s-join
              ","
              (--map (format "%s" it) v)))
            (_ v))

   when (and k v (not (cl-member k ignore :test 'string-equal)))
   concat
   (format "%s=\"%s\"," (substring k 1) v)
   into out

   finally return
   (concat
    out
    (when (not visible)
      "fontcolor=\"#FF000000\", bgcolor=\"#FF000000\", color=\"#FF000000\""))))

(defun rysco-graph--render-properties (data &optional layers)
  (loop
   with out
   with obj-layer = (plist-get data :layer)
   with visible = (rysco-graph--render-layer-visible layers obj-layer)

   for (k v) on data by 'cddr
   as k = (format "%s" k)
   as k = (if (equal (substring k 0 1) ":")
              (substring k 1)
            k)
   concat
   (format
    "%s=%s;\n"
    k
    (pcase v
      ((pred listp)
       (s-join
        ","
        (--map (format "%s" it) v)))
      (_ (prin1-to-string v))))
   into out

   finally return
   (concat
    out
    (when (not visible)
      "fontcolor=\"#FF000000\"; bgcolor=\"#FF000000\"; color=\"#FF000000\";\n"))))

(cl-defun rysco-graph--render-nodes (patch &key path name subgraph prefix properties rand-state color-cache layers ignore)
  (loop
   with entry-prefix = (if path (format "%s_" path) "")

   for entry in patch do
   (pcase entry
     (`(,(and (or :group :cluster) type) ,(and (or (pred stringp) (pred symbolp)) name) . ,group-data)
      (let ((global (equal name '_)))
        (unless global
          (insert (format "subgraph cluster_%s%s {\n" entry-prefix name)))

        (rysco-graph--render-nodes
         group-data
         :name name
         :path (if (eq type :cluster)
                   (if path (format "%s_%s" path name) name)
                 path)
         :subgraph t
         :prefix (eq type :cluster)
         :layers layers
         :ignore ignore)

        (unless global
          (insert (format "}\n")))))

     (`(:properties . ,property-data)
      (insert
       (rysco-graph--render-properties property-data layers)))

     (`((_ . ,properties))
      (insert
       (format "node [%s];\n"
               (rysco-graph--render-plist-to-settings
                properties
                '_
                color-cache
                rand-state
                layers))))

     (`(:rank ,type . ,nodes)
      (insert
       (format
        "{rank=%s; %s}\n"
        type
        (loop
         with prefix
         for n in nodes
         concat (format "%s%s" (or prefix "") n)
         do (setq prefix ", ")))))

     (`(:labels . ,data)
      (loop
       for (k v) on data by 'cddr do
       (puthash k (format "%s" v ) color-cache)))

     (`((,(and (or (pred stringp) (pred symbolp)) mod-name) . ,data) . ,_)
      (insert
       (format
        "\"%s%s\" [%s];\n"
        entry-prefix
        mod-name
        (rysco-graph--render-plist-to-settings
         data
         mod-name
         color-cache
         rand-state
         layers
         ignore)))))))

(cl-defun rysco-graph--render-connections (patch &key path name subgraph prefix properties rand-state color-cache layers)
  (cl-loop
   for entry in patch
   do
   (pcase entry
     (`(,(or :properties :group :cluster) . ,_))
     (`(:edge . ,props)
      (insert
       (format "edge [%s];\n"
               (rysco-graph--render-plist-to-settings
                   props nil color-cache rand-state layers))))
     (`(,from (,to ,label . ,props))
      (let ((color (gethash label color-cache)))
        (unless color
          (setq color
                (or (plist-get props :color)
                    (rysco-graph--render-generate-color rand-state)))
          (puthash label color color-cache))
        (insert
         (format "%s -> %s [label=\"%s\", color=\"%s\", fontcolor=\"%s\", %s];\n"
                 (rysco-graph--render-node-name from)
                 (rysco-graph--render-node-name to)
                 label
                 color color
                 (or
                  (rysco-graph--render-plist-to-settings
                   props label color-cache rand-state layers)
                  "")))))
     (`(,from ,to)
      (insert (format "%s -> %s;\n"
                      (rysco-graph--render-node-name from)
                      (rysco-graph--render-node-name to)))))))

(cl-defun rysco-graph--render-node-name (name)
  (let* ((name (format "%s" name))
         (split (s-index-of ":" name))
         (node (if split (substring name 0 split) name))
         (port (if split (substring name split) "")))
    (format "\"%s\"%s" node port)))

(cl-defun rysco-graph--render (patch &key filename graph-code rand-seed layers as-code ignore)
  (-let* ((temp-path (make-temp-file "patch" nil ".dot"))
          (color-cache (make-hash-table :test 'equal))
          (rand-state (cl-make-random-state rand-seed))
          (layers (when layers
                    (-flatten
                    (--map
                     (split-string (format "%s" it) "[:]" t "[ ]+")
                     (if (listp layers)
                         layers
                       `(,layers))))))
          (filename (or filename
                        (rysco-graph--render-guess-filename)))
          (out-path (format "%s.svg" (file-name-sans-extension temp-path)))
          out-code)

    (with-temp-buffer
      (insert "digraph patch {\n"
              "\nnode  [style=\"rounded,filled,bold\", shape=box, fixedsize=true, width=1.3, fontname=\"Arial\"];\n"
              (or graph-code "")
              "\n")

      ;; Insert nodes
      (rysco-graph--render-nodes
       patch
       :color-cache color-cache
       :rand-state rand-state
       :layers layers
       :ignore ignore)

      ;; Insert connections
      (rysco-graph--render-connections
       patch
       :color-cache color-cache
       :rand-state rand-state
       :layers layers)

      (insert "\n}\n")

      (if as-code
          (setq out-code (buffer-string))
        (write-file temp-path)))

    (if as-code
        out-code
      (-let* ((graphviz-dot-preview-extension "svg")
              (command-result (string-trim
                               (shell-command-to-string
                                (graphviz-compile-command temp-path)))))
        (if (string-prefix-p "Error:" command-result)
            (message command-result)

          (delete-file temp-path)

          (rename-file out-path filename t)
          filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun rysco-graph--fan (from connection-properties data &optional reverse)
  (loop
   with results
   with anchors
   with connection-properties = connection-properties

   for entry in data
   as out = nil
   do (pcase entry
        ((pred vectorp) (setq connection-properties (append entry nil)))
        (:break (setq anchors (butlast anchors)))
        ('> (setq reverse nil))
        ('< (setq reverse t))
        (_
         (let* ((these (rysco-graph--process from connection-properties entry reverse))
                (tails (rysco-graph--extract-tails these)))
           (setq
            out (car these)
            anchors (append anchors tails)))))

   when out append out into results
   finally return
   (cons results anchors)))

(cl-defun rysco-graph--scope (from connection-properties root data &optional reverse)
  (let ((old-root rysco-graph--scope))
    (setq rysco-graph--scope root)
    (prog1 (rysco-graph--process from connection-properties data reverse)
      (setq rysco-graph--scope old-root))))

(cl-defun rysco-graph--chain (from connection-properties data &optional reverse)
  (loop
   with results
   with anchors = from
   with connection-properties = connection-properties

   for entry in data
   as out = nil
   do (pcase entry
        (:break (setq anchors nil))
        ('> (setq reverse nil))
        ('< (setq reverse t))
        ((pred vectorp) (setq connection-properties (append entry nil)))
        (_
         (let* ((these (rysco-graph--process anchors connection-properties entry reverse))
                (tails (rysco-graph--extract-tails these)))
           (setq
            out (car these)
            anchors tails
            connection-properties nil))))

   when out append out into results
   finally return
   (cons results anchors)))

(defvar rysco-graph--scope nil)

(cl-defun rysco-graph--scope-node (node)
  (let* ((node (format "%s" node))
         (escaped (s-starts-with? "^" node))
         (node (intern (if escaped (substring node 1) node))))
    (if (and rysco-graph--scope (not escaped))
        (intern (format "%s_%s" rysco-graph--scope node))
      node)))

(cl-defun rysco-graph--node (from connection-properties node &optional reverse)
  (let ((scoped-node (rysco-graph--scope-node node)))
    (list
     (loop
      for anchor in from
      as start = (if reverse scoped-node anchor)
      as end = (if reverse anchor scoped-node)
      collect
      `(,start
        ,(if connection-properties
             `(,end ,@connection-properties)
           end)))
     scoped-node)))

(cl-defun rysco-graph--sequence (from connection-properties data &rest rest)
  (-let* (((name columns . entries) data)
          (spans (cl-make-hash-table :test 'equal))
          (headers (cl-make-hash-table :test 'equal))
          (default-node-style '(:shape point :width 0))
          (default-conn-style '(:style invis))
          (active-conn-style '(:arrowsize 0.5 :color black :dir both))
          (arrowhead-style 'halfopen)
          (arrowtail-style 'odot)
          (node-style '())
          (header-style '(:shape box :style filled :fillcolor white))
          (header-index (lambda (el)
                          (--find-index
                           (equal el
                                  (if (listp it)
                                      (car it)
                                    it))
                           columns))))

    (loop
     for head in columns
     for i from 0
     do
     (puthash
      i
      (pcase head
        (`(,id ,label . ,props)
         `(:label ,label ,@props))
        (_
         `(:label ,(upcase (format "%s" head)))))
      headers))

    (loop
     for (from to conn . conn-props) in entries
     for y from 1
     as start = (funcall header-index from)
     as end = (funcall header-index to)
     as backward = (> start end)

     do
     (loop
      for i from (min start end) below (max start end)
      as id = (format "%s_%s_%s" name i y)
      as at-end = (= i (- end (if backward 0 1)))
      as at-start = (= i (- start (if backward 1 0)))

      do
      (puthash
       id
       (append
        active-conn-style
        conn-props
       `(,(if backward
              :arrowtail
            :arrowhead)

         ,(if at-end arrowhead-style 'none)

         ;;
         ,(if (not backward)
              :arrowtail
            :arrowhead)

         ,(if at-start arrowtail-style 'none)

         ,@(if at-end
               `(:label ,conn)
             '())))

       spans)))

    ;; Actual matrix
    `((,@(loop
          for y from 0 below (+ (length entries) 2) ; +Header & Footer
          as lasty = (when (> y 0) (1- y))
          as rank-group = nil
          as at-header = (= y 0)
          as at-footer = (= y (1+ (length entries)))

          append
          (loop
           for x from 0 below (length columns)
           as lastx = (when (> x 0) (1- x))

           as left = (format "%s_%s_%s" name lastx y)
           as above = (format "%s_%s_%s" name x lasty)
           as id = (format "%s_%s_%s" name x y)
           as cell-contents = (gethash id spans)
           as left-cell-contents = (gethash left spans)

           do (push id rank-group)

           ;; Connections
           when lasty collect `(,above (,id "" :color gray :arrowhead none))

           when lastx collect
           `(,left
             (,id "" ,@(or left-cell-contents
                           default-conn-style)))

           ;; Nodes
           do
           (push
            (cons
             id
             (append
              (if (or at-header at-footer)
                  (append header-style (gethash x headers))
                '(:shape point :width 0))
              `(:group ,x)))
            node-style))

          ;;
          collect `(:rank same ,@rank-group))

       ;;
       (:group
        ,name
        ,@(loop
           for (node . node-props) in node-style collect
           `((,node ,@node-props))))))))

(cl-defun rysco-graph--convert-group (data)
  (-let [(type name . rest) data]
    `(,type
      ,name
      ,@(loop
         for entry in rest collect
         (pcase entry
           (`(,(or :group :cluster) . ,_)
            (rysco-graph--convert-group entry))
           (`(:properties . ,_) entry)
           ((pred nlistp) `((,entry)))
           (_ `(,entry)))))))

(cl-defun rysco-graph--process (from connection-properties form &optional reverse)
  (pcase form
    (`(:scope ,root . ,rest) (rysco-graph--scope from connection-properties root rest reverse))
    (`(:chain . ,rest) (rysco-graph--chain from connection-properties rest reverse))
    (`(:fan . ,rest) (rysco-graph--fan from connection-properties rest reverse))
    (`(:sequence . ,rest) (rysco-graph--sequence from connection-properties rest reverse))
    (`(,(or :group :cluster) . ,_)
     `((,(rysco-graph--convert-group form))))
    (`(:node . ,properties)
     `((:group _ ((_ ,@properties)))))

    ;; Pass-through for any :* style key
    (`(,(app (lambda (f) (aref (prin1-to-string f) 0)) ?\:) . ,_)
     `((,form)))
    (_ (rysco-graph--node from connection-properties form reverse))))

(cl-defun rysco-graph--extract-tails (connections)
  (cdr connections))

;;;###autoload
(cl-defun rysco-graph (args forms)
  (apply
   'rysco-graph--render
   (loop
    for f in forms append
    (car (rysco-graph--process nil nil f)))
   args))

;;
(provide 'rysco-graph)
