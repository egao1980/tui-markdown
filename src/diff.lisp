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

(defun render-diff-lines (diff width &key (layout :unified) theme (word-diff t)
                                          (split-min-width 120))
  "Render DIFF — a DIFF-PROTOCOL:DIFF or a string of unified diff text — at
WIDTH cells (an integer ≥ 1) by the rules above. LAYOUT is :UNIFIED,
:SPLIT or :AUTO; THEME is a plist role → TP:STYLE. → a list of lines,
each a list of W:TEXT-RUNs. Bad arguments → TYPE-ERROR; a diff value
DIFF:RENDER-DIFF cannot render as :UNIFIED → DIFF:UNSUPPORTED-CONTENT."
  (declare (ignore diff width layout theme word-diff split-min-width))
  (%not-implemented 'render-diff-lines))
