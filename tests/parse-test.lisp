(in-package #:tui-markdown/tests)

;;; The parser and its tests live in markdown-protocol. Here: the re-export
;;; keeps this package's API, and the AST it returns is what the renderer
;;; takes.

(deftest parse-markdown-is-re-exported
  (ok (eq 'md:parse-markdown 'markdown-protocol:parse-markdown)
      "the same symbol as markdown-protocol's")
  (ok (eq :external (nth-value 1 (find-symbol "PARSE-MARKDOWN" :tui-markdown))))
  (ok (equal '((:heading 1 ("a")) (:paragraph ((:emph ("b")) " " (:code "c"))))
             (md:parse-markdown (nl "# a" "*b* `c`"))))
  (ok (signals (md:parse-markdown 42) 'type-error))
  (ok (equal (md "# a" 20) (texts (md:render-markdown (md:parse-markdown "# a") 20)))
      "a parsed AST renders like its source"))
