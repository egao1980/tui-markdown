(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Diffs → lines. A DIFF-PROTOCOL value is first rendered to unified text
;;; with (DIFF:RENDER-DIFF d :UNIFIED NIL), so values and text (git diff,
;;; diff -u, model output) take one path. Malformed text never signals.
;;;
;;; READING unified text (line by line):
;;;  - "diff --git a/X b/Y" starts a file section (paths from the a/ and b/
;;;    parts). A "--- " line directly followed by "+++ " sets the old / new
;;;    path (text after a TAB dropped, "/dev/null" = none, one leading
;;;    component stripped) and starts a new section unless the current file
;;;    section has no hunks yet.
;;;  - "@@ -a[,b] +c[,d] @@…" starts a hunk (a section without a file
;;;    header when there is none). Its body is read by the counts (default
;;;    1): " " (or an empty line) context, "-" removed, "+" added;
;;;    "\ No newline at end of file" skipped. A line with another first
;;;    character ends the hunk early. Line numbers start at a and c.
;;;  - "index …", "similarity index …", "dissimilarity index …" are
;;;    dropped; "rename from P" / "rename to P" set the paths; every other
;;;    line outside a hunk (mode lines, "new file mode", "Binary files …",
;;;    mail preamble, prose) is a META line of the current section (a
;;;    section without a file header when there is none).
;;;
;;; LAYOUT, per section (sections separated by one empty line):
;;;  - File header (file sections only): the path — "OLD → NEW" when both
;;;    exist and differ, else the one present — in :diff-file, " ", "+A" in
;;;    :diff-added, " ", "-R" in :diff-removed (A / R = added / removed
;;;    lines of the section).
;;;  - Meta lines verbatim in :diff-meta, broken by cells at W.
;;;  - G = digits of the largest line number in the section's hunks
;;;    (old or new), at least 1. "@@" lines are not shown; between two
;;;    hunks of a section one gap row: G + 1 spaces then "⋮" (:diff-meta).
;;;  - A row: number right-aligned in G cells (:diff-line-number), " ",
;;;    the sign ("-" :diff-removed, "+" :diff-added, " " default), then the
;;;    content (removed / added text in :diff-removed / :diff-added,
;;;    context default). A trailing #\Return is not shown; tabs expand
;;;    (EXPAND-TABS, from the content start). The content breaks by cells
;;;    (BREAK-RUNS) at max(1, W − G − 2); continuation rows start with
;;;    G + 2 spaces. Unified numbers: the old one for "-", the new one for
;;;    "+" and context.
;;;  - Change blocks: a maximal run of non-context rows of a hunk shows its
;;;    "-" rows, then its "+" rows. With :WORD-DIFF (default T) the i-th
;;;    "-" row pairs with the i-th "+" row. Tokens are maximal runs of
;;;    alphanumerics and "_", maximal runs of spaces / tabs, and single
;;;    other characters; the changed tokens come from a minimal token diff
;;;    (diff-protocol's Myers text engine, one token per line). When the
;;;    unchanged tokens of the "-" line hold at least half of
;;;    max(length old, length new) characters, changed tokens get
;;;    :diff-removed-word / :diff-added-word; otherwise the pair is plain.
;;;  - :SPLIT: panes LW = ⌊(W − 1)/2⌋ and RW = W − 1 − LW around a "│"
;;;    (:diff-meta). Left: context and "-" rows with old numbers; right:
;;;    context and "+" rows with new numbers; a change block's i-th rows
;;;    face each other, a missing side is blank. Each side breaks at its
;;;    own pane width; a row is as tall as its taller side; the left pane
;;;    is padded with spaces to LW, the right is not. The gap row appears
;;;    on both sides. Headers and meta lines span W.
;;;  - :AUTO is :SPLIT when W ≥ :SPLIT-MIN-WIDTH (default 120), else
;;;    :UNIFIED.

;;; Word diff

(defun %word-char-p (ch) (or (alphanumericp ch) (char= ch #\_)))
(defun %blank-char-p (ch) (member ch '(#\Space #\Tab)))

(defun %tokens (s)
  "S → its tokens: maximal runs of alphanumerics and _, maximal runs of
spaces / tabs, single other characters."
  (let ((out '()) (i 0) (n (length s)))
    (loop while (< i n)
          do (let* ((ch (char s i))
                    (pred (cond ((%word-char-p ch) #'%word-char-p)
                                ((%blank-char-p ch) #'%blank-char-p)))
                    (j (if pred
                           (or (position-if-not pred s :start i) n)
                           (1+ i))))
               (push (subseq s i j) out)
               (setf i j)))
    (coerce (nreverse out) 'simple-vector)))

(defun %token-text (tokens)
  (with-output-to-string (out)
    (loop for tk across tokens do (write-string tk out) (write-char #\Newline out))))

(defun %word-masks (old new)
  "Changed-token masks of the strings OLD and NEW from diff-protocol's text
engine (one token per line). → (values old-tokens old-mask new-tokens
new-mask), or NIL when the pair is to be shown plain."
  (let* ((ot (%tokens old))
         (nt (%tokens new))
         (om (make-array (length ot) :initial-element nil))
         (nm (make-array (length nt) :initial-element nil))
         (d (diff:compute-diff (diff:find-diff-engine :text) (%token-text ot) (%token-text nt)))
         (changes (sort (copy-list (diff:diff-changes d)) #'<
                        :key (lambda (c) (first (diff:change-path c)))))
         (offset 0))
    (dolist (c changes)
      (let* ((i (first (diff:change-path c)))
             (nb (length (diff:change-before c)))
             (na (length (diff:change-after c)))
             (j (+ i offset)))
        (fill om t :start i :end (+ i nb))
        (fill nm t :start j :end (+ j na))
        (incf offset (- na nb))))
    (let ((unchanged (loop for tk across ot for m across om unless m sum (length tk))))
      (when (>= (* 2 unchanged) (max (length old) (length new)))
        (values ot om nt nm)))))

(defun %masked-runs (tokens mask style word-style)
  (loop for tk across tokens
        for m across mask
        collect (%run tk (if m word-style style))))

;;; Rows

(defstruct (%dctx (:constructor %make-dctx (theme word-diff)))
  theme word-diff)

(defun %dts (role dctx) (theme-style role (%dctx-theme dctx)))

(defun %row-style (kind dctx)
  (ecase kind
    (:context tp:+default-style+)
    (:removed (%dts :diff-removed dctx))
    (:added (%dts :diff-added dctx))))

(defun %side (number kind content g pane dctx)
  "One diff row in a pane PANE cells wide with gutter G: number, \" \", sign,
CONTENT runs broken by cells; continuation rows indented G + 2."
  (let* ((style (%row-style kind dctx))
         (sign (%run (ecase kind (:context " ") (:removed "-") (:added "+")) style))
         (pieces (break-runs (expand-tabs content) (max 1 (- pane g 2))))
         (indent (%run (%spaces (+ g 2)))))
    (cons (list* (%run (format nil "~vD" g number) (%dts :diff-line-number dctx))
                 (%run " ")
                 sign
                 (first pieces))
          (mapcar (lambda (p) (cons indent p)) (rest pieces)))))

(defun %row-content (row dctx &optional tokens mask)
  (let ((style (%row-style (drow-kind row) dctx)))
    (if tokens
        (%masked-runs tokens mask style
                      (overlay-style style (%dts (if (eq (drow-kind row) :removed)
                                                     :diff-removed-word
                                                     :diff-added-word)
                                                 dctx)))
        (list (%run (drow-text row) style)))))

(defun %hunk-segments (hunk dctx)
  "HUNK's rows as segments: (:context row content) or (:change pairs), PAIRS
a list of (removed-row removed-content added-row added-content), any side
NIL; the i-th - row faces the i-th + row."
  (let ((rows (dhunk-rows hunk)) (out '()))
    (loop while rows
          do (if (eq (drow-kind (first rows)) :context)
                 (let ((r (pop rows)))
                   (push (list :context r (%row-content r dctx)) out))
                 (let ((removed '()) (added '()))
                   (loop while (and rows (not (eq (drow-kind (first rows)) :context)))
                         do (let ((r (pop rows)))
                              (if (eq (drow-kind r) :removed) (push r removed) (push r added))))
                   (setf removed (nreverse removed) added (nreverse added))
                   (push (list :change
                               (loop for k below (max (length removed) (length added))
                                     for r = (nth k removed)
                                     for a = (nth k added)
                                     collect (multiple-value-bind (ot om nt nm)
                                                 (and r a (%dctx-word-diff dctx)
                                                      (%word-masks (drow-text r) (drow-text a)))
                                               (list r (and r (%row-content r dctx ot om))
                                                     a (and a (%row-content a dctx nt nm))))))
                         out))))
    (nreverse out)))

(defun %gutter (section)
  (let ((m 1))
    (dolist (x (dsection-items section))
      (when (dhunk-p x)
        (setf m (max m
                     (+ (dhunk-old-start x) (dhunk-old-count x) -1)
                     (+ (dhunk-new-start x) (dhunk-new-count x) -1)))
        (dolist (r (dhunk-rows x))
          (setf m (max m (or (drow-old r) 0) (or (drow-new r) 0))))))
    (length (princ-to-string m))))

(defun %section-header (section width dctx)
  (when (dsection-file-p section)
    (let* ((old (dsection-old section))
           (new (dsection-new section))
           (path (cond ((and old new (string/= old new))
                        (format nil "~A ~C ~A" old (code-char #x2192) new))
                       (t (or new old "")))))
      (break-runs (list (%run path (%dts :diff-file dctx))
                        (%run " ")
                        (%run (format nil "+~D" (dsection-added section)) (%dts :diff-added dctx))
                        (%run " ")
                        (%run (format nil "-~D" (dsection-removed section)) (%dts :diff-removed dctx)))
                  width))))

(defun %gap-line (g dctx)
  (list (%run (%spaces (1+ g))) (%run (string (code-char #x22EE)) (%dts :diff-meta dctx))))

(defun %pad-line (line width)
  (let ((gap (- width (w:runs-width line))))
    (if (plusp gap) (append line (list (%run (%spaces gap)))) line)))

(defun %render-section (section width layout dctx)
  (let* ((g (%gutter section))
         (lw (floor (1- width) 2))
         (rw (- width 1 lw))
         (bar (%run (string (code-char #x2502)) (%dts :diff-meta dctx)))
         (first-hunk t)
         (out '()))
    (flet ((emit (lines) (setf out (revappend lines out)))
           (pair (left right)
             ;; LEFT / RIGHT: (number kind content) or NIL
             (let* ((ls (and left (destructuring-bind (num kind content) left
                                    (%side num kind content g lw dctx))))
                    (rs (and right (destructuring-bind (num kind content) right
                                     (%side num kind content g rw dctx))))
                    (h (max (length ls) (length rs))))
               (loop for k below h
                     collect (append (%pad-line (nth k ls) lw) (list bar) (nth k rs))))))
      (emit (%section-header section width dctx))
      (dolist (x (dsection-items section))
        (if (not (dhunk-p x))
            (emit (break-runs (list (%run (second x) (%dts :diff-meta dctx))) width))
            (progn
              (unless first-hunk
                (emit (if (eq layout :split)
                          (list (append (%pad-line (%gap-line g dctx) lw) (list bar) (%gap-line g dctx)))
                          (list (%gap-line g dctx)))))
              (setf first-hunk nil)
              (dolist (seg (%hunk-segments x dctx))
                (ecase (first seg)
                  (:context
                   (destructuring-bind (row content) (rest seg)
                     (emit (if (eq layout :split)
                               (pair (list (drow-old row) :context content)
                                     (list (drow-new row) :context content))
                               (%side (drow-new row) :context content g width dctx)))))
                  (:change
                   (let ((pairs (second seg)))
                     (if (eq layout :split)
                         (dolist (p pairs)
                           (destructuring-bind (r rc a ac) p
                             (emit (pair (and r (list (drow-old r) :removed rc))
                                         (and a (list (drow-new a) :added ac))))))
                         (progn
                           (dolist (p pairs)
                             (destructuring-bind (r rc a ac) p
                               (declare (ignore a ac))
                               (when r (emit (%side (drow-old r) :removed rc g width dctx)))))
                           (dolist (p pairs)
                             (destructuring-bind (r rc a ac) p
                               (declare (ignore r rc))
                               (when a (emit (%side (drow-new a) :added ac g width dctx)))))))))))))))
    (nreverse out)))

(defun render-diff-lines (diff width &key (layout :unified) theme (word-diff t)
                                          (split-min-width 120))
  "Render DIFF — a DIFF-PROTOCOL:DIFF or a string of unified diff text — at
WIDTH cells (an integer ≥ 1) by the rules above. LAYOUT is :UNIFIED,
:SPLIT or :AUTO; THEME is a plist role → TP:STYLE. → a list of lines,
each a list of W:TEXT-RUNs. Bad arguments → TYPE-ERROR; a diff value
DIFF:RENDER-DIFF cannot render as :UNIFIED → DIFF:UNSUPPORTED-CONTENT."
  (check-type diff (or string diff:diff))
  (check-type width (integer 1))
  (check-type layout (member :unified :split :auto))
  (check-type theme list)
  (check-type split-min-width (integer 1))
  (let* ((text (if (stringp diff) diff (diff:render-diff diff :unified nil)))
         (layout (if (eq layout :auto)
                     (if (>= width split-min-width) :split :unified)
                     layout))
         (dctx (%make-dctx theme word-diff)))
    (loop for (section . more) on (%read-diff text)
          append (%render-section section width layout dctx)
          when more collect '())))
