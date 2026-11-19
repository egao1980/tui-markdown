(in-package #:tui-markdown)

;;; H1c contract skeleton: every function body below signals until the
;;; milestone is implemented. The docstrings and file headers are the
;;; contract; the brief is cl-stack docs/capabilities/tui-markdown.md.

(defun %not-implemented (name)
  (error "~S is not implemented yet (tui-markdown H1c contract)" name))

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
  (declare (ignore role))
  (%not-implemented 'default-theme-style))

(defun theme-style (role &optional theme)
  "The style of ROLE in THEME (a plist role → TP:STYLE, or NIL): THEME's entry
when present, else DEFAULT-THEME-STYLE. An unknown ROLE or a non-list
THEME → TYPE-ERROR. Callers must not mutate the result."
  (declare (ignore role theme))
  (%not-implemented 'theme-style))

(defun overlay-style (base over)
  "A fresh style: BASE with every non-default attribute of OVER put on top.
Colours (foreground, background, underline-color) other than :DEFAULT
replace; true booleans set; a non-NIL underline kind and a non-NIL
hyperlink replace. Default attributes of OVER never clear BASE's. Neither
argument is mutated; a non-style argument → TYPE-ERROR.

Nesting in both renderers is overlaying: emphasis inside a heading is the
heading style with :EMPH overlaid, a link's style is :LINK overlaid plus
(:hyperlink url), quoted content has :QUOTE overlaid, and so on."
  (declare (ignore base over))
  (%not-implemented 'overlay-style))

;;; ------------------------------------------------------------------
;;; Run helpers shared by the markdown and diff renderers.

(defun expand-tabs (runs &key (tab-width 4))
  "RUNS (one line, anything W:AS-RUNS takes) with each #\\Tab replaced by
spaces up to the next multiple of TAB-WIDTH cells, counted from the start
of RUNS (TP:STRING-WIDTH widths: a wide character is 2). The spaces take
the tab's run style. → a list of runs."
  (declare (ignore runs tab-width))
  (%not-implemented 'expand-tabs))

(defun break-runs (runs width)
  "Break RUNS into visual lines of at most WIDTH cells (an integer ≥ 1,
else TYPE-ERROR) by cells only, for code and diffs: W:WRAP-RUNS rule 3
without its word rule. #\\Newline splits hard lines; an empty hard line is
one empty line (). A line takes the longest prefix of whole characters
that fits: a combining mark stays with its base, a wide character that
does not fit moves down, a line holds at least one character. Spaces are
kept. Runs are split, never merged. → a list of lines (lists of runs)."
  (declare (ignore runs width))
  (%not-implemented 'break-runs))
