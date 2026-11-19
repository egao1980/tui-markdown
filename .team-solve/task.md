---
system: tui-markdown
test-system: tui-markdown/tests
brief: ../cl-stack/docs/capabilities/tui-markdown.md
---

# H1c: markdown and diff rendering

Implement `tui-markdown` 0.1.0 (harness plan Track H, row H1, "markdown and diff rendering"): every exported function in `src/`, replacing each `%not-implemented` body. The brief's "Locked decisions" stay locked.

- Theme roles, defaults and `overlay-style`, `expand-tabs` and `break-runs` (`src/theme.lisp`).
- `parse-markdown`: the CommonMark subset plus GFM tables and `~~strike~~` in the `src/markdown.lisp` header, with the inline delimiter-run algorithm. The AST is the plain-list shape in the docstring.
- `render-markdown`: the layout rules in that header (paragraphs, inline styles by overlay, links with OSC 8 hyperlinks and ` (url)`, headings, lists, quotes, code with the highlight hook, thematic breaks, tables with shrink and fallback).
- `markdown-stream` (`src/stream.lisp`): completion on complete lines only; committed blocks are rendered once; the partial closing fence is hidden.
- `render-diff-lines` (`src/diff.lisp`): read unified text (diff-protocol values go through `diff:render-diff … :unified`); unified and split layouts, line numbers, the hunk gap, word-level highlighting through diff-protocol's text engine, wrapping by cells.

Split the sources into more files when that helps (update the `.asd`), but keep the package and its exports. No new dependencies beyond `tui-protocol/widgets`, `terminal-protocol` and `diff-protocol`. `tests/` is the spec. All 58 deftests fail against the stubs.
