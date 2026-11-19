(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Markdown renderer internals (the Rendering rules in markdown.lisp's
;;; header). Every block renders to at least one line, and a list of blocks
;;; renders to each block's lines joined by one empty line: the stream's
;;; committed prefix relies on both.

(defun %parse-document (string)
  "STRING → (values blocks open-fence); OPEN-FENCE is (char . length) when a
top-level fenced code block runs unclosed to the end."
  (let ((*open-fence* nil)
        (*depth* 0))
    (let ((blocks (values (%parse-blocks (%source-lines string)))))
      (values blocks *open-fence*))))

(defstruct (%ctx (:constructor %make-ctx (width theme highlight show-urls)))
  width theme highlight show-urls)

(defun %ts (role ctx) (theme-style role (%ctx-theme ctx)))

(defun %run (text &optional (style tp:+default-style+))
  (w:text-run text :style style))

(defun %spaces (n) (make-string (max 0 n) :initial-element #\Space))

(defun %with-width (ctx width)
  (let ((c (copy-%ctx ctx)))
    (setf (%ctx-width c) width)
    c))

;;; Inlines → runs

(defun %plain-text (inlines)
  (with-output-to-string (out)
    (labels ((walk (xs)
               (dolist (x xs)
                 (if (stringp x)
                     (write-string x out)
                     (ecase (first x)
                       ((:emph :strong :strike) (walk (second x)))
                       (:code (write-string (second x) out))
                       (:link (walk (third x)))
                       (:soft-break (write-char #\Space out))
                       (:hard-break (write-char #\Newline out)))))))
      (walk inlines))))

(defun %inline-runs (inlines style ctx)
  "INLINES → runs in STYLE with nested styles overlaid; a hard break is a
#\\Newline run (W:WRAP-RUNS splits it)."
  (loop for x in inlines
        nconc (if (stringp x)
                  (list (%run x style))
                  (ecase (first x)
                    (:emph (%inline-runs (second x) (overlay-style style (%ts :emph ctx)) ctx))
                    (:strong (%inline-runs (second x) (overlay-style style (%ts :strong ctx)) ctx))
                    (:strike (%inline-runs (second x) (overlay-style style (%ts :strike ctx)) ctx))
                    (:code (list (%run (second x) (overlay-style style (%ts :code ctx)))))
                    (:soft-break (list (%run " " style)))
                    (:hard-break (list (%run (string #\Newline) style)))
                    (:link
                     (destructuring-bind (url inner) (rest x)
                       (let ((link-style (overlay-style (overlay-style style (%ts :link ctx))
                                                        (tp:make-style :hyperlink url))))
                         (nconc (%inline-runs inner link-style ctx)
                                (when (and (%ctx-show-urls ctx)
                                           (string/= (%plain-text inner) url))
                                  (list (%run (format nil " (~A)" url)
                                              (overlay-style style (%ts :link-url ctx)))))))))))))

;;; Blocks

(defun %render-blocks (blocks ctx &optional (separate t))
  "BLOCKS → lines, one empty line between blocks when SEPARATE."
  (loop for (b . more) on blocks
        append (%render-block b ctx)
        when (and separate more) collect '()))

(defun %frame (lines first rest &key (empty '()) (first-empty first))
  "Prefix LINES: FIRST on line 0 (FIRST-EMPTY when line 0 is empty), REST on
later non-empty lines, EMPTY for later empty lines."
  (loop for line in lines
        for i from 0
        collect (cond ((zerop i) (append (if line first first-empty) line))
                      ((null line) empty)
                      (t (append rest line)))))

(defun %render-block (block ctx)
  (let ((w (%ctx-width ctx)))
    (ecase (first block)
      (:paragraph
       (w:wrap-runs (%inline-runs (second block) tp:+default-style+ ctx) w))
      (:heading
       (destructuring-bind (level inlines) (rest block)
         (let ((base (%ts (if (= level 1) :heading-1 :heading) ctx)))
           (w:wrap-runs (append (when (>= level 3)
                                  (list (%run (format nil "~A " (make-string level :initial-element #\#))
                                              base)))
                                (%inline-runs inlines base ctx))
                        w))))
      (:thematic-break
       (list (list (%run (make-string w :initial-element (code-char #x2500)) (%ts :rule ctx)))))
      (:code-block (%render-code block ctx))
      (:block-quote
       (let* ((inner (%render-blocks (second block) (%with-width ctx (max 1 (- w 2)))))
              (quote-style (%ts :quote ctx))
              (bar-style (%ts :quote-bar ctx))
              (bar (list (%run (format nil "~C " (code-char #x2502)) bar-style)))
              (bar-alone (list (%run (string (code-char #x2502)) bar-style))))
         (if (null inner)
             (list bar-alone)
             (%frame (mapcar (lambda (line)
                               (mapcar (lambda (r)
                                         (%run (w:text-run-text r)
                                               (overlay-style (w:text-run-style r) quote-style)))
                                       line))
                             inner)
                     bar bar :empty bar-alone :first-empty bar-alone))))
      (:bullet-list
       (destructuring-bind (tight items) (rest block)
         (%render-list (loop repeat (length items) collect "- ") tight items ctx)))
      (:ordered-list
       (destructuring-bind (start delimiter tight items) (rest block)
         (%render-list (loop for k from start repeat (length items)
                             collect (format nil "~D~C " k delimiter))
                       tight items ctx)))
      (:table (%render-table block ctx)))))

(defun %render-code (block ctx)
  (destructuring-bind (language lines) (rest block)
    (let* ((w (%ctx-width ctx))
           (hook (%ctx-highlight ctx))
           (style (%ts :code-block ctx))
           (src (or (and hook (funcall hook language lines))
                    (mapcar (lambda (l) (list (%run l style))) lines)))
           (indent (%run "  ")))
      (if (null src)
          (list '())
          (loop for line in src
                nconc (mapcar (lambda (row) (and row (cons indent row)))
                              (break-runs (expand-tabs line) (max 1 (- w 2)))))))))

(defun %render-list (markers tight items ctx)
  (let* ((m (reduce #'max markers :key #'length))
         (iw (max 1 (- (%ctx-width ctx) m)))
         (ictx (%with-width ctx iw))
         (marker-style (%ts :list-marker ctx))
         (pad (%spaces m)))
    (loop for (item . more) on items
          for marker in markers
          for full = (concatenate 'string (%spaces (- m (length marker))) marker)
          for lines = (%render-blocks item ictx (not tight))
          append (if (or (null lines) (null (first lines)))
                     (cons (list (%run (string-right-trim " " full) marker-style))
                           (%frame (rest lines) (list (%run pad)) (list (%run pad))
                                   :first-empty '()))
                     (%frame lines (list (%run full marker-style)) (list (%run pad))))
          when (and more (not tight)) collect '())))

;;; Tables

(defun %pad-runs (runs width align)
  "RUNS padded with default spaces to WIDTH cells per ALIGN."
  (let* ((gap (max 0 (- width (w:runs-width runs))))
         (left (case align
                 (:right gap)
                 (:center (floor gap 2))
                 (t 0)))
         (right (- gap left)))
    (append (when (plusp left) (list (%run (%spaces left))))
            runs
            (when (plusp right) (list (%run (%spaces right)))))))

(defun %render-table (block ctx)
  (destructuring-bind (aligns header rows) (rest block)
    (let* ((w (%ctx-width ctx))
           (n (length aligns))
           (border (%ts :table-border ctx))
           (header-style (overlay-style tp:+default-style+ (%ts :table-header ctx)))
           (all (cons (mapcar (lambda (c) (%inline-runs c header-style ctx)) header)
                      (mapcar (lambda (row)
                                (mapcar (lambda (c) (%inline-runs c tp:+default-style+ ctx)) row))
                              rows)))
           (natural (loop for j below n
                          collect (max 1 (loop for row in all maximize (w:runs-width (nth j row))))))
           (widths (copy-list natural))
           (available (- w (+ (* 3 n) 1))))
      (loop while (> (reduce #'+ widths) available)
            do (let ((best nil))
                 (loop for j from 0
                       for wj in widths
                       for nj in natural
                       when (and (> wj (min nj 3))
                                 (or (null best) (> wj (nth best widths))))
                         do (setf best j))
                 (if best
                     (decf (nth best widths))
                     (return-from %render-table (%table-fallback all ctx)))))
      (flet ((rule (left mid right)
               (list (%run (with-output-to-string (out)
                             (write-char left out)
                             (loop for (wj . more) on widths
                                   do (loop repeat (+ wj 2) do (write-char (code-char #x2500) out))
                                      (when more (write-char mid out)))
                             (write-char right out))
                           border)))
             (row-lines (row)
               (let* ((cells (loop for j below n
                                   for wj in widths
                                   collect (w:wrap-runs (nth j row) wj)))
                      (height (reduce #'max cells :key #'length)))
                 (loop for k below height
                       collect (append
                                (list (%run (string (code-char #x2502)) border))
                                (loop for cell in cells
                                      for wj in widths
                                      for align in aligns
                                      append (append (list (%run " "))
                                                     (%pad-runs (nth k cell) wj align)
                                                     (list (%run " ")
                                                           (%run (string (code-char #x2502)) border)))))))))
        (append (list (rule (code-char #x250C) (code-char #x252C) (code-char #x2510)))
                (row-lines (first all))
                (list (rule (code-char #x251C) (code-char #x253C) (code-char #x2524)))
                (loop for row in (rest all) append (row-lines row))
                (list (rule (code-char #x2514) (code-char #x2534) (code-char #x2518))))))))

(defun %table-fallback (all ctx)
  (let ((sep (%run " | " (%ts :table-border ctx))))
    (loop for row in all
          append (w:wrap-runs (loop for (cell . more) on row
                                    append cell
                                    when more collect sep)
                              (%ctx-width ctx)))))
