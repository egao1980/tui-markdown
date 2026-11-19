(defsystem "tui-markdown"
  :version "0.1.0"
  :description "Markdown and diff rendering for tui-protocol: a streaming CommonMark-subset renderer and a unified / side-by-side diff renderer, both emitting styled text runs"
  :author "egao1980"
  :license "MIT"
  :depends-on ("tui-protocol/widgets" "terminal-protocol" "diff-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "theme")
               (:file "md-inline")
               (:file "md-block")
               (:file "md-render")
               (:file "markdown")
               (:file "stream")
               (:file "diff-read")
               (:file "diff"))
  :in-order-to ((test-op (test-op "tui-markdown/tests"))))

(defsystem "tui-markdown/tests"
  :depends-on ("tui-markdown" "tui-backend-terminal" "terminal-backend-lisp" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "theme-test")
               (:file "block-test")
               (:file "inline-test")
               (:file "render-test")
               (:file "table-test")
               (:file "stream-test")
               (:file "diff-test")
               (:file "e2e-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
