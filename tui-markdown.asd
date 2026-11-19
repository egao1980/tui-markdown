(defsystem "tui-markdown"
  :version "0.1.0"
  :description "Markdown and diff rendering for tui-protocol: a streaming CommonMark-subset renderer and a unified / side-by-side diff renderer, both emitting styled text runs"
  :author "egao1980"
  :license "MIT"
  :depends-on ("tui-protocol/widgets" "terminal-protocol" "diff-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")))
