;;; -*- lexical-binding: t; -*-

(require 'dash)

;;;;;; Graphing utilities
(require 'graphviz-dot-mode)

(cl-defun rysco-simple-graph--generate-color (&optional rand-state)
  (apply
   'color-rgb-to-hex
   `(,@(color-hsl-to-rgb
        (cl-random 1.0 rand-state)
        (cl-random 1.0 rand-state)
        0.5)
     2)))

(defun rysco-simple-graph--layer-visible (all-layers obj-layer)
  (or (or (eq all-layers nil)
          (eq obj-layer nil))
      (-let [current-layers (split-string
                             (format "%s" obj-layer)
                             "[:]"
                             t
                             "[ ]+")]
        (--any? (cl-member it all-layers :test 'string-equal) current-layers))))

(cl-defun rysco-simple-graph--plist-to-settings (data &optional id color-cache rand-state layers)
  (loop
   with out
   with obj-layer = (plist-get data :layer)
   with visible = (rysco-simple-graph--layer-visible layers obj-layer)

   for k in data by 'cddr
   as k = (format "%s" k)
   for v in (cdr data) by 'cddr
   as v = (pcase v
            ('RCOL (rysco-simple-graph--generate-color rand-state))
            ('UCOL
             (when color-cache
               (--if-let (gethash id color-cache)
                   it
                 (puthash
                  id
                  (setq color
                        (rysco-simple-graph--generate-color rand-state))
                  color-cache))))
            (_ v))

   when (and k v)
   concat
   (format "%s=\"%s\"," (substring k 1) v)
   into out

   finally return
   (concat
    out
    (when (not visible)
      "fontcolor=\"#FF000000\", bgcolor=\"#FF000000\", color=\"#FF000000\""))))

(defun rysco-simple-graph--properties (data &optional layers)
  (loop
   with out
   with obj-layer = (plist-get data :layer)
   with visible = (rysco-simple-graph--layer-visible layers obj-layer)

   for (k v) on data by 'cddr
   as k = (format "%s" k)
   as k = (if (equal (substring k 0 1) ":")
              (substring k 1)
            k)
   concat
   (format "%s=%s;\n" k (prin1-to-string v))
   into out

   finally return
   (concat
    out
    (when (not visible)
      "fontcolor=\"#FF000000\"; bgcolor=\"#FF000000\"; color=\"#FF000000\";\n"))))

(cl-defun rysco-simple-graph--nodes (patch &key path name subgraph prefix properties rand-state color-cache layers)
  (loop
   with entry-prefix = (if path (format "%s_" path) "")

   for entry in patch do
   (pcase entry
     (`(,(and (or :group :cluster) type) ,(and (or (pred stringp) (pred symbolp)) name) . ,group-data)
      (insert (format "subgraph cluster_%s%s {\n" entry-prefix name))

      (rysco-simple-graph--nodes
       group-data
       :name name
       :path (if (eq type :cluster)
                 (if path (format "%s_%s" path name) name)
               path)
       :subgraph t
       :prefix (eq type :cluster)
       :layers layers)

      (insert (format "}\n")))

     (`(:properties . ,property-data)
      (insert
       (rysco-simple-graph--properties property-data layers)))

     (`(,(and (or (pred stringp) (pred symbolp)) mod-name) . ,_)
      (insert (format "\"%s%s\";\n" entry-prefix mod-name)))

     (`((,(and (or (pred stringp) (pred symbolp)) mod-name) . ,data) . ,_)
      (insert
       (format
        "\"%s%s\" [%s];\n"
        entry-prefix
        mod-name
        (rysco-simple-graph--plist-to-settings
         data
         mod-name
         color-cache
         rand-state
         layers)))))))

(cl-defun rysco-simple-graph--guess-filename (&optional ext)
  (when (boundp 'out)
    out)
  (when (equal major-mode 'org-mode)
    (concat
     (replace-regexp-in-string
      "[/\\:]" "-"
      (s-join "-" (org-get-outline-path t)))
     (or ext ".png"))))

;;;###autoload
(cl-defun rysco-simple-graph (patch &key filename graph-code rand-seed layers as-code)
  "Calls Graphviz and generates a graph from the provided, simplified graph format.

More Graphviz Dot formatting information at URL `https://graphviz.org/doc/info/attrs.html'

FILENAME will be used to set the output filename and return that as a string.
GRAPH-CODE is a string that will be inserted verbatim into the generated Graphviz Dot code after the default values.

RAND-SEED is the random seed used for color generation.  Specifying this will cause the colors used for any stable graph to be the same.

PATCH is the graph data in the following form.

Nodes and connections can be specified as (node CONNECTION1 CONNECTION2 ... CONNECTION3)
Nodes can be a symbol, string, or a list of the form (ID KEY1 VALUE1 ... KEYN VALUEN)
Node data is specified as a plist.
Connections can be a symbol, a string, or a list of form (DEST LABEL . PROPERTIES)
PROPERTIES is an optional plist of keys/values that are set for the connection entry in the resultant dot output.

Nodes can be grouped into subgraphs using an entry (:group NAME ...)

Dot settings for the current graph can be specified using an entry (:properties KEY1 VALUE1 ... KEYN VALUEN)

Object layers can be set using (... :layer NAME) where NAME is a symbol, string, or number.
NAME can also specify multiple layers as a string with layer names separated by `?:'.
When LAYERS is provided, any objects not specified as being on at least one of the active layers will be drawn transparently such that it is not seen, but the graph layout is stable.

If LAYERS is omitted all layers are drawn.
Objects with no layer specified are always drawn.

Some common properties:
          rankdir=LR
          rank=source
          concentrate=true
          size = \"13\"
          label = \"Test Label\"
          fontname = \"Arial\"
          fontsize = 30.0
          fontcolor = \"#3b3b3b\"
          color = \"#aaaaaa\"
          fillcolor = \"#B0e0e6\"
          style = filled
          style = bold
          penwidth = 0
          weight = 100


Example:
  (rysco-simple-graph
   '(
     (:properties
      label \"Graph Title\"
      labeljust l
      labelloc t
      rankdir LR
      size 13
      concentrate true)

     (other)
     (a)
     (b)
     (:group \"platform\"
             (:properties
              label \"Platform Label\"
              style filled
              penwidth 0
              fillcolor \"#B0e0e6\")

             (tc)
             (server))

     (other
      (a \"tc\")
      (b \"tc\"))

     (a
      (tc \"label\"))

     (tc
      (b \"other label\"))

     (b
      (server \"3rd label\"))

     (server
      (b \"Denied\"))))"

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
                        (rysco-simple-graph--guess-filename)))
          (out-path (format "%s.png" (file-name-sans-extension temp-path)))
          out-code)

    (with-temp-buffer
      (insert "digraph patch {\n"
              "\nnode  [style=\"rounded,filled,bold\", shape=box, fixedsize=true, width=1.3, fontname=\"Arial\"];\n"
              (or graph-code "")
              "\n")

      ;; Insert nodes
      (rysco-simple-graph--nodes
       patch
       :color-cache color-cache
       :rand-state rand-state
       :layers layers)

      ;; Insert connections
      (cl-loop
       for entry in patch
       for (mod . connections) in patch
       as forward = t
       as mod = (if (listp mod)
                    (car mod)
                  mod)

       when (and (listp entry)
                 (not (memq (car entry) '(:group :cluster :properties))))
       do
       (cl-loop
        for dest in connections do
        (insert
         (pcase dest
           ('> (setq forward t) "#")
           ('< (setq forward nil) "#")
           (`(,dest ,comment . ,settings)
            (let ((color (gethash comment color-cache)))
              (unless color
                (setq color
                      (or (plist-get settings :color)
                          (rysco-simple-graph--generate-color rand-state)))
                (puthash comment color color-cache))
              (format
               "\"%s\" -> \"%s\" [label=\"%s\", color=\"%s\", fontcolor=\"%s\",%s]"
               (if forward mod dest)
               (if forward dest mod)
               comment color color
               (or
                (rysco-simple-graph--plist-to-settings
                 settings comment color-cache rand-state layers)
                ""))))
           (dest
            (format
             "\"%s\" -> \"%s\""
             (if forward mod dest)
             (if forward dest mod))))
         ";\n")))

      (insert "\n}\n")

      (if as-code
          (setq out-code (buffer-string))
        (write-file temp-path)))

    (if as-code
        out-code
      (-let ((command-result (string-trim
                              (shell-command-to-string
                               (graphviz-compile-command temp-path)))))
        (if (string-prefix-p "Error:" command-result)
            (message command-result)

          (delete-file temp-path)

          (rename-file out-path filename t)
          filename)))))

(cl-defun rysco-simple-graph-animated (patch frames &key filename graph-code rand-seed delay loop debug)
  "Generates an animated gif from the provided graph.
See `rysco-simple-graph' for more information.

FRAMES is a list of all layers to render in order.
DELAY is the delay between frames (passed to Imagemagick).
LOOP is the number of loop cycles (passed to Imagemagick).
DEBUG set to non-nil will create a single frame gif with all of the specified layers present.  Useful for seeing all of the objects at once.
"
  (loop
   with filename = (or filename (rysco-simple-graph--guess-filename ".gif"))
   with delay = (or delay 100)
   with loop = (or loop 0)
   with frames = (if debug
                     (list (s-join ":" (--map (format "%s" it) frames)))
                   frames)

   with frame-list

   for layer in frames
   as frame-name = (make-temp-file "patch" nil ".png")

   collect
   (rysco-simple-graph patch
                       :filename frame-name
                       :graph-code graph-code
                       :rand-seed rand-seed
                       :layers layer)
   into frame-list

   finally return
   (let* ((cmd (format
                "%s -delay %s -loop %s %s \"%s\""
                rysco-imagemagick-executable delay loop
                (apply 'concat (--map (format "\"%s\" " it) frame-list))
                filename))

          (command-result (string-trim (shell-command-to-string cmd))))

     (loop for temp in frame-list do
           (delete-file temp))

     filename)))

(cl-defun rysco-graph--fan (from connection-properties data)
  (loop
   with results
   with anchors
   with connection-properties = connection-properties

   for entry in data
   as out = nil
   do (pcase entry
        ((pred vectorp) (setq connection-properties (append entry nil)))
        (_
         (let* ((these (rysco-graph--process from connection-properties entry))
                (tails (rysco-graph--extract-tails these)))
           (setq
            out (car these)
            anchors (append anchors tails)))))

   when out append out into results
   finally return
   (cons results anchors)))

(cl-defun rysco-graph--chain (from connection-properties data)
  (loop
   with results
   with anchors = from
   with connection-properties

   for entry in data
   as out = nil
   do (pcase entry
        (:break (setq anchors nil))
        ((pred vectorp) (setq connection-properties (append entry nil)))
        (_
         (let* ((these (rysco-graph--process anchors connection-properties entry))
                (tails (rysco-graph--extract-tails these)))
           (setq
            out (car these)
            anchors tails
            connection-properties nil))))

   when out append out into results
   finally return
   (cons results anchors)))

(cl-defun rysco-graph--node (from connection-properties node)
  (list
   (loop
    for anchor in from collect
    `(,anchor
      ,(if connection-properties
           `(,node ,@connection-properties)
         node)))
   node))

(cl-defun rysco-graph--process (from connection-properties form)
  (pcase form
    (`(:chain . ,rest) (rysco-graph--chain from connection-properties rest))
    (`(:fan . ,rest) (rysco-graph--fan from connection-properties rest))
    (`(,(and (or :group :cluster :properties) type) . ,rest)
     `(((,type . ,rest))))
    (_ (rysco-graph--node from connection-properties form))))

(cl-defun rysco-graph--extract-tails (connections)
  (cdr connections))

;;;###autoload
(cl-defmacro rysco-graph (args &rest forms)
  `(rysco-simple-graph
    (append
     ,@(loop
        for f in forms collect
        `(car (rysco-graph--process nil nil ',f))))
    ,@args))

;;
(provide 'rysco-graph)
