(defpackage #:tui-markdown
  (:use #:cl)
  (:nicknames #:stack-tui-md)
  (:local-nicknames (#:w #:tui-protocol/widgets) (#:tp #:terminal-protocol)
                    (#:diff #:diff-protocol) (#:mdp #:markdown-protocol))
  ;; The parser lives in markdown-protocol; PARSE-MARKDOWN is the same
  ;; symbol, re-exported so this package's API is unchanged.
  (:import-from #:markdown-protocol #:parse-markdown)
  (:export
   ;; conditions
   #:tui-markdown-error #:markdown-stream-finished #:markdown-stream-finished-stream
   ;; themes
   #:+theme-roles+ #:default-theme-style #:theme-style #:overlay-style
   ;; markdown
   #:parse-markdown #:render-markdown
   ;; streaming
   #:markdown-stream #:make-markdown-stream
   #:markdown-stream-append #:markdown-stream-finish #:markdown-stream-finished-p
   #:markdown-stream-source #:markdown-stream-width
   #:markdown-stream-lines #:markdown-stream-committed-count
   ;; diffs
   #:render-diff-lines
   ;; shared helpers
   #:break-runs #:expand-tabs))
