(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Markdown: PARSE-MARKDOWN and its AST come from markdown-protocol
;;; (re-exported from this package); its src/markdown.lisp header has the
;;; parsing rules. A CommonMark 0.31 subset plus GFM tables and ~~strike~~.
;;; The brief (cl-stack docs/capabilities/tui-markdown.md, "H1c — locked")
;;; has the rendering rules below with reasons.
;;;
;;; ------------------------------------------------------------------
;;; Rendering: blocks → lines (lists of W:TEXT-RUNs) at WIDTH cells.
;;;
;;;  - Top-level blocks are separated by one empty line (); no leading or
;;;    trailing empty lines. A line's text never holds #\Newline.
;;;  - Paragraph: inline runs, W:WRAP-RUNS at the width. Soft break → " ",
;;;    hard break → a new line.
;;;  - Inline styles overlay (OVERLAY-STYLE) on the enclosing style: :emph,
;;;    :strong, :strike, :code (no backticks shown). A link's text gets
;;;    :link plus (:hyperlink url); with :SHOW-URLS (default T) and a text
;;;    different from the url, " (url)" follows in the enclosing style
;;;    with :link-url overlaid. An image renders as a link to its url
;;;    whose text is the alt text, or the url when the alt is empty.
;;;  - Heading: base :heading-1 (level 1) or :heading; levels 3–6 start
;;;    with "### " (level × #, then a space) in the base style; wrapped.
;;;  - Code block: each line through the highlight hook or, when the hook
;;;    is NIL or returns NIL, one :code-block run; tabs expanded
;;;    (EXPAND-TABS), broken by cells (BREAK-RUNS) at max(1, W − 2), each
;;;    non-empty row prefixed with two default spaces; empty rows are ().
;;;    No fences shown. An empty block is one empty line.
;;;    Hook: (funcall highlight language lines) → a list of lines (anything
;;;    W:AS-RUNS takes), used as is, or NIL. Called once per rendering of
;;;    the block.
;;;  - Block quote: contents rendered at max(1, W − 2); each non-empty line
;;;    is "│ " (:quote-bar) + the line with :quote overlaid; an empty line
;;;    is "│" alone. Nested quotes nest.
;;;  - Lists: marker "- " for bullets; "N. " / "N) " (the source delimiter)
;;;    numbered from the start, right-aligned to the widest marker M.
;;;    Items rendered at max(1, W − M); the first line gets the marker
;;;    (:list-marker), later non-empty lines M spaces, empty lines stay ().
;;;    An item with no lines is its marker with trailing spaces trimmed.
;;;    Loose lists: one empty line between items and between the blocks of
;;;    an item; tight: none.
;;;  - Thematic break: W × "─" in :rule.
;;;  - Table: cell runs (header cells with :table-header overlaid);
;;;    natural column width = max cell width, at least 1. Available
;;;    A = W − (3n + 1). While the sum exceeds A, the widest column
;;;    (leftmost on ties) above min(natural, 3) loses one cell; when none
;;;    can, fall back. Cells word-wrap (W:WRAP-RUNS) to their column; a
;;;    row is as tall as its tallest cell, top aligned. Lines:
;;;      ┌──┬──┐  "─" × (w + 2) per column
;;;      │ a │ b │  one space each side; padding per alignment (:right
;;;      ├──┼──┤  pads left, :center puts the odd space on the right)
;;;      │ … │ … │  body rows, no separators between them
;;;      └──┴──┘
;;;    Borders and "│" in :table-border. Fallback: each row (header first)
;;;    is its cells joined by " | " (:table-border), wrapped at W.
;;;  - Widths: with W ≥ 10 every line fits W cells; W ≥ 1 never signals.

(defun render-markdown (markdown width &key theme highlight (show-urls t))
  "Render MARKDOWN (a string, or blocks from PARSE-MARKDOWN) at WIDTH cells
(an integer ≥ 1) by the rules above. THEME is a plist role → TP:STYLE
(see THEME-STYLE); HIGHLIGHT is the code block hook or NIL; SHOW-URLS
appends \" (url)\" to links whose text differs. → a list of lines, each a
list of W:TEXT-RUNs. A bad WIDTH or MARKDOWN → TYPE-ERROR."
  (check-type width (integer 1))
  (check-type markdown (or string list))
  (check-type theme list)
  (let ((blocks (if (stringp markdown) (parse-markdown markdown) markdown)))
    (%render-blocks blocks (%make-ctx width theme highlight show-urls))))
