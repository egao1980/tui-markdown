# tui-markdown

Markdown and diff rendering for [`tui-protocol`](https://github.com/egao1980/tui-protocol) programs on [cl-stack](https://github.com/egao1980/cl-stack). There is a streaming CommonMark-subset renderer and a diff renderer for [`diff-protocol`](https://github.com/egao1980/diff-protocol) values and unified text. Both emit lines of `tui-protocol/widgets` styled text runs, with wide characters handled. Harness plan track H, milestone H1c. Brief: [`cl-stack/docs/capabilities/tui-markdown.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/tui-markdown.md).

**Status: implemented** (0.1.0). Since 0.2.0 the markdown parser lives in [`markdown-protocol`](https://github.com/egao1980/markdown-protocol); `parse-markdown` is re-exported from this package, so the API is unchanged.

```lisp
(asdf:load-system "tui-markdown")

;; whole documents
(stack-tui-md:render-markdown "# Title

Some *emphasis*, `code` and a [link](https://example.com)." 60)
;; → lines (lists of text runs) for a viewport or draw-runs

;; streaming model output: completed blocks never re-render
(let ((s (stack-tui-md:make-markdown-stream :width 60)))
  (stack-tui-md:markdown-stream-append s "Hello **wor")
  (stack-tui-md:markdown-stream-lines s))

;; diffs: diff-protocol values or unified text (git diff, diff -u)
(stack-tui-md:render-diff-lines (uiop:run-program '("git" "diff") :output :string) 100
                                :layout :auto)
```

| Piece | What it does |
|-------|--------------|
| `parse-markdown` | CommonMark subset plus GFM tables and `~~strike~~`, parsed to a plain-list AST (from `markdown-protocol`, re-exported): headings, paragraphs, emphasis, code spans, links, autolinks, fenced code, block quotes, lists and tables |
| `render-markdown` | AST → lines at a width. Styles nest by overlay. Links become OSC 8 hyperlinks. A code-highlighting hook. Tables use box drawing and shrink to fit |
| `markdown-stream` | Append chunks as they arrive. Completed blocks are committed once; only the last block re-renders |
| `render-diff-lines` | Unified or side-by-side layout, file headers with +/− counts, line numbers, word-level highlights, wrapping by cells |

Styles come from a theme: a plist of 22 roles, each mapped to a `terminal-protocol` style. The defaults use the 16-colour palette.

## License

MIT
