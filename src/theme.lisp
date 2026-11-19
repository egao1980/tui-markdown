(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Themes. A theme is a plist ROLE → TP:STYLE; a role missing from it (or a
;;; NIL theme) falls back to DEFAULT-THEME-STYLE. The defaults use only
;;; attributes and the 16-colour palette, so they read on any terminal.
;;;
;;;   role                default
;;;   :heading-1          bold, underline :single
;;;   :heading            bold                      (levels 2–6)
;;;   :emph               italic
;;;   :strong             bold
;;;   :strike             strikethrough
;;;   :code               foreground 6              (inline code)
;;;   :code-block         foreground 6              (fenced code, no highlighter)
;;;   :link               underline :single, foreground 4
;;;   :link-url           faint                     (the " (url)" after a link)
;;;   :quote              italic                    (overlaid on quoted content)
;;;   :quote-bar          faint                     ("│ ")
;;;   :list-marker        default                   ("- ", "1. ")
;;;   :rule               faint                     (thematic break)
;;;   :table-border       faint
;;;   :table-header       bold                      (overlaid on header cells)
;;;   :diff-file          bold                      (file header path)
;;;   :diff-added         foreground 2              (+ lines, "+N" counts)
;;;   :diff-removed       foreground 1              (- lines, "-N" counts)
;;;   :diff-added-word    foreground 2, inverse     (changed words in + lines)
;;;   :diff-removed-word  foreground 1, inverse
;;;   :diff-line-number   faint
;;;   :diff-meta          faint                     (hunk gap "⋮", git header lines,
;;;                                                  the split separator "│")

(defparameter +theme-roles+
  '(:heading-1 :heading :emph :strong :strike :code :code-block :link :link-url
    :quote :quote-bar :list-marker :rule :table-border :table-header
    :diff-file :diff-added :diff-removed :diff-added-word :diff-removed-word
    :diff-line-number :diff-meta)
  "Every role a theme may set.")

(defun default-theme-style (role)
  "The default style of ROLE (a fresh TP:STYLE), per the table above. A
symbol not in +THEME-ROLES+ → TYPE-ERROR."
  (unless (member role +theme-roles+)
    (%unknown-role role))
  (ecase role
    (:heading-1 (tp:make-style :bold t :underline :single))
    ((:heading :strong :table-header :diff-file) (tp:make-style :bold t))
    ((:emph :quote) (tp:make-style :italic t))
    (:strike (tp:make-style :strikethrough t))
    ((:code :code-block) (tp:make-style :foreground 6))
    (:link (tp:make-style :underline :single :foreground 4))
    ((:link-url :quote-bar :rule :table-border :diff-line-number :diff-meta)
     (tp:make-style :faint t))
    (:list-marker (tp:make-style))
    (:diff-added (tp:make-style :foreground 2))
    (:diff-removed (tp:make-style :foreground 1))
    (:diff-added-word (tp:make-style :foreground 2 :inverse t))
    (:diff-removed-word (tp:make-style :foreground 1 :inverse t))))

(defun %unknown-role (role)
  (error 'type-error :datum role :expected-type (cons 'member +theme-roles+)))

(defparameter *default-styles*
  (loop for role in +theme-roles+
        collect role collect (default-theme-style role))
  "Shared default styles (THEME-STYLE's fallback); never mutated.")

(defun theme-style (role &optional theme)
  "The style of ROLE in THEME (a plist role → TP:STYLE, or NIL): THEME's entry
when present, else DEFAULT-THEME-STYLE. An unknown ROLE or a non-list
THEME → TYPE-ERROR. Callers must not mutate the result."
  (check-type theme list)
  (unless (member role +theme-roles+)
    (%unknown-role role))
  (or (getf theme role) (getf *default-styles* role)))

(defun overlay-style (base over)
  "A fresh style: BASE with every non-default attribute of OVER put on top.
Colours (foreground, background, underline-color) other than :DEFAULT
replace; true booleans set; a non-NIL underline kind and a non-NIL
hyperlink replace. Default attributes of OVER never clear BASE's. Neither
argument is mutated; a non-style argument → TYPE-ERROR.

Nesting in both renderers is overlaying: emphasis inside a heading is the
heading style with :EMPH overlaid, a link's style is :LINK overlaid plus
(:hyperlink url), quoted content has :QUOTE overlaid, and so on."
  (check-type base tp:style)
  (check-type over tp:style)
  (let ((s (tp:copy-style base)))
    (macrolet ((colour (accessor)
                 `(unless (eq (,accessor over) :default)
                    (setf (,accessor s) (,accessor over))))
               (flag (accessor)
                 `(when (,accessor over) (setf (,accessor s) t)))
               (value (accessor)
                 `(when (,accessor over) (setf (,accessor s) (,accessor over)))))
      (colour tp:style-foreground)
      (colour tp:style-background)
      (colour tp:style-underline-color)
      (flag tp:style-bold)
      (flag tp:style-faint)
      (flag tp:style-italic)
      (flag tp:style-blink)
      (flag tp:style-inverse)
      (flag tp:style-invisible)
      (flag tp:style-strikethrough)
      (flag tp:style-overline)
      (value tp:style-underline)
      (value tp:style-hyperlink))
    s))

;;; ------------------------------------------------------------------
;;; Run helpers shared by the markdown and diff renderers.

(defun expand-tabs (runs &key (tab-width 4))
  "RUNS (one line, anything W:AS-RUNS takes) with each #\\Tab replaced by
spaces up to the next multiple of TAB-WIDTH cells, counted from the start
of RUNS (TP:STRING-WIDTH widths: a wide character is 2). The spaces take
the tab's run style. → a list of runs."
  (check-type tab-width (integer 1))
  (let ((col 0))
    (loop for run in (w:as-runs runs)
          for text = (w:text-run-text run)
          collect (if (find #\Tab text)
                      (w:text-run
                       (with-output-to-string (out)
                         (loop for ch across text
                               do (if (char= ch #\Tab)
                                      (let ((n (- tab-width (mod col tab-width))))
                                        (loop repeat n do (write-char #\Space out))
                                        (incf col n))
                                      (progn (write-char ch out)
                                             (incf col (tp:char-width ch))))))
                       :style (w:text-run-style run))
                      (progn (incf col (tp:string-width text))
                             run)))))

(defun break-runs (runs width)
  "Break RUNS into visual lines of at most WIDTH cells (an integer ≥ 1,
else TYPE-ERROR) by cells only, for code and diffs: W:WRAP-RUNS rule 3
without its word rule. #\\Newline splits hard lines; an empty hard line is
one empty line (). A line takes the longest prefix of whole characters
that fits: a combining mark stays with its base, a wide character that
does not fit moves down, a line holds at least one character. Spaces are
kept. Runs are split, never merged. → a list of lines (lists of runs)."
  (check-type width (integer 1))
  (let* ((runs (w:as-runs runs))
         (s (w:runs-string runs))
         (n (length s)))
    (loop for a = 0 then (1+ b)
          for b = (or (position #\Newline s :start a) n)
          nconc (if (= a b)
                    (list '())
                    (loop with pos = a
                          while (< pos b)
                          collect (let ((e (%fit-end s pos b width)))
                                    (when (= e pos)
                                      (setf e (%cluster-end s pos b)))
                                    (prog1 (%slice-runs runs pos e)
                                      (setf pos e)))))
          while (< b n))))

(defun %cluster-end (string start end)
  "Index after the cluster (a base character plus the zero-width characters
after it) that starts at START (< END)."
  (let ((j (1+ start)))
    (loop while (and (< j end) (zerop (tp:char-width (char string j))))
          do (incf j))
    j))

(defun %fit-end (string start end limit)
  "Largest cluster boundary E in [START, END] with STRING[START, E) at most
LIMIT cells wide."
  (let ((w 0) (i start))
    (loop while (< i end)
          do (let* ((e (%cluster-end string i end))
                    (cw (tp:string-width string :start i :end e)))
               (when (> (+ w cw) limit) (return))
               (incf w cw)
               (setf i e)))
    i))

(defun %slice-runs (runs start end)
  "The runs covering [START, END) of (W:RUNS-STRING RUNS): split, never
merged, no empty runs."
  (loop with pos = 0
        for run in runs
        for text = (w:text-run-text run)
        for len = (length text)
        for s = (max start pos)
        for e = (min end (+ pos len))
        when (< s e)
          collect (if (and (= s pos) (= e (+ pos len)))
                      run
                      (w:text-run (subseq text (- s pos) (- e pos))
                                  :style (w:text-run-style run)))
        do (incf pos len)))
