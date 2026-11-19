(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Markdown: a CommonMark 0.31 subset plus GFM tables and ~~strike~~.
;;; Where this header and CommonMark differ, this header wins. The brief
;;; (cl-stack docs/capabilities/tui-markdown.md, "H1c — locked") has the
;;; same rules with reasons.
;;;
;;; BLOCKS (line based; a container takes its lines, strips its marker or
;;; indentation and parses the rest recursively):
;;;  - Lines split at LF; one CR before it is dropped. Tabs in a line's
;;;    leading whitespace become spaces (4-column stops), also inside
;;;    fenced code. Block starts allow 0–3 spaces of indent.
;;;  - ATX headings: 1–6 #, then a space/tab or the end of the line. An
;;;    optional closing run of # preceded by a space is dropped. No setext
;;;    headings: "---" under a paragraph is a thematic break.
;;;  - Thematic break: 3+ of one of * - _ with optional spaces/tabs.
;;;  - Fenced code: 3+ ` or ~; a backtick fence's info may not contain `.
;;;    Language = first word of the info string, NIL if none. Closed by the
;;;    same character, at least as many, 0–3 spaces of indent, only
;;;    whitespace after; unclosed runs to the end of its container. Up to
;;;    the opening fence's indent is removed from each content line.
;;;  - Block quote: ">" plus one optional space; consecutive ">" lines.
;;;    No lazy continuation.
;;;  - Lists: bullets - + *, ordered 1–9 digits then . or ). The content
;;;    column is the marker end plus 1–4 spaces (1 when 5+ spaces follow
;;;    or the rest is blank). Following lines indented to the content
;;;    column belong to the item; blank lines belong when the next
;;;    non-blank line is indented that far. An item may start with one
;;;    blank line only. No lazy continuation. A different bullet character
;;;    or ordered delimiter starts a new list; the first number is the
;;;    start. A thematic break wins over a list item. Loose when items are
;;;    separated by blank lines or an item holds two blocks with a blank
;;;    line between them.
;;;  - Tables (GFM): a line with "|" (0–3 indent, not inside a paragraph)
;;;    followed by a delimiter row (cells :?-+:? split at "|") with the
;;;    same cell count. Rows run to a blank line or a line that would
;;;    interrupt a paragraph; each row is padded / cut to the header's
;;;    count. Cells: outer pipes dropped, split at unescaped "|", "\|" → "|",
;;;    trimmed, parsed inline.
;;;  - Paragraphs: other lines, until a blank line or an interrupting line:
;;;    a heading, fence, block quote, thematic break, non-empty bullet item
;;;    or non-empty ordered item starting at 1 — at most 3 spaces of
;;;    indent. Continuation lines lose their leading whitespace; the
;;;    paragraph's trailing whitespace is dropped.
;;;  - Not supported (left as text): indented code blocks, setext headings,
;;;    HTML blocks, link reference definitions, footnotes, task-list
;;;    markers, math.
;;;
;;; INLINES (CommonMark §6 with its delimiter-run algorithm):
;;;  - Backslash escapes of ASCII punctuation; backslash + newline is a
;;;    hard break; other backslashes are text.
;;;  - Code spans: equal-length backtick strings; newlines → spaces; one
;;;    space stripped from each end when both are spaces and the content
;;;    is not all spaces. Unmatched backticks are text.
;;;  - Emphasis * and _ with left/right-flanking runs, the intraword _
;;;    rule and the multiple-of-3 rule; *** → (:emph ((:strong …))).
;;;    ~~ (exactly two) → :strike under the same flanking rules.
;;;  - Inline links [text](dest "title"): dest bare (balanced parens,
;;;    escapes) or <…>; title dropped; no links inside links; emphasis is
;;;    resolved inside the brackets first. Autolinks <scheme:…> (scheme
;;;    2–32 chars) → (:link url (url)). No reference links, no images
;;;    ("!" stays text), no raw HTML (text), no entities (text).
;;;  - Newline → (:soft-break); two or more spaces before it → (:hard-break).
;;;    Spaces around a newline are dropped.

(defun parse-markdown (string)
  "Parse STRING (the subset above) → a list of blocks. A non-string →
TYPE-ERROR. Never signals on any string.

Blocks:  (:paragraph inlines) (:heading level inlines) (:thematic-break)
         (:code-block language lines)       ; language: string or NIL
         (:block-quote blocks)
         (:bullet-list tight items)          ; items: list of block lists
         (:ordered-list start delimiter tight items)   ; delimiter #\\. or #\\)
         (:table alignments header rows)     ; alignment :left :right :center NIL;
                                             ; header: cells; rows: lists of cells;
                                             ; a cell is a list of inlines
Inlines: string  (:emph inlines) (:strong inlines) (:strike inlines)
         (:code string) (:link url inlines) (:soft-break) (:hard-break)
Adjacent strings are merged and empty strings dropped, so results compare
with EQUAL."
  (declare (ignore string))
  (%not-implemented 'parse-markdown))

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
;;;    with :link-url overlaid.
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
  (declare (ignore markdown width theme highlight show-urls))
  (%not-implemented 'render-markdown))
