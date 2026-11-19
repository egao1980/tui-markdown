(in-package #:tui-markdown/tests)

(deftest render-paragraphs
  (ok (equal '("hello world") (md "hello world" 20)))
  (ok (equal '("hello" "world") (md "hello world" 7)) "word wrap (W:WRAP-RUNS rules)")
  (ok (equal '("a b") (md (nl "a" "b") 20)) "a soft break is a space")
  (ok (equal '("a" "b") (md (nl "a  " "b") 20)) "a hard break is a new line")
  (ok (equal '("one" "" "two") (md (nl "one" "" "two") 20)) "one blank line between blocks")
  (ok (equal '("日本語" "テキ") (md "日本語テキ" 6)) "wide characters wrap by cells")
  (ok (null (md "" 20)))
  (ok (all-runs-p (md-lines *corpus* 40)))
  (ok (signals (md:render-markdown "x" 0) 'type-error))
  (ok (signals (md:render-markdown 42 10) 'type-error)))

(deftest render-inline-styles
  (let ((line (first (md-lines "a *b* **c** ~~d~~ `e`" 40))))
    (ok (equal '(("a ") ("b" :italic t) (" ") ("c" :bold t) (" ") ("d" :strike t) (" ") ("e" :fg 6))
               (segments line))
        "inline code has no backticks"))
  (ok (equal '(("x" :bold t :italic t))
             (segments (first (md-lines "***x***" 10))))
      "nested emphasis overlays")
  (ok (equal '(("x" :fg 6 :italic t))
             (segments (first (md-lines "*`x`*" 10))))
      "code inside emphasis")
  (let ((theme (list :strong (tp:make-style :foreground 3))))
    (ok (equal '(("c" :fg 3)) (segments (first (md-lines "**c**" 10 :theme theme)))))))

(deftest render-links
  (ok (equal '(("see ")
               ("docs" :fg 4 :underline :single :link "https://x.io")
               (" (https://x.io)" :faint t))
             (segments (first (md-lines "see [docs](https://x.io)" 40)))))
  (ok (equal '(("docs" :fg 4 :underline :single :link "https://x.io"))
             (segments (first (md-lines "[docs](https://x.io)" 40 :show-urls nil))))
      ":show-urls nil")
  (ok (equal '(("https://x.io" :fg 4 :underline :single :link "https://x.io"))
             (segments (first (md-lines "<https://x.io>" 40))))
      "no suffix when the text is the url")
  (ok (equal '(("a" :fg 4 :underline :single :link "u") ("b" :fg 4 :bold t :underline :single :link "u")
               (" (u)" :faint t))
             (segments (first (md-lines "[a**b**](u)" 40))))))

(deftest render-headings
  (let ((lines (md-lines (nl "# One" "## Two" "### Three" "#### *Four*") 30)))
    (ok (equal '("One" "" "Two" "" "### Three" "" "#### Four") (texts lines))
        "levels 1–2 without markers, 3–6 keep them")
    (ok (equal '(("One" :bold t :underline :single)) (segments (nth 0 lines))))
    (ok (equal '(("Two" :bold t)) (segments (nth 2 lines))))
    (ok (equal '(("### Three" :bold t)) (segments (nth 4 lines))))
    (ok (equal '(("#### " :bold t) ("Four" :bold t :italic t)) (segments (nth 6 lines)))))
  (ok (equal '("A long" "heading") (md "# A long heading" 7)) "headings wrap"))

(deftest render-lists
  (ok (equal '("- a" "- b" "  - c" "- d") (md (nl "- a" "- b" "  - c" "- d") 20)))
  (ok (equal '("- a" "" "- b") (md (nl "- a" "" "- b") 20)) "loose")
  (ok (equal '("- alpha" "  beta" "- c") (md (nl "- alpha beta" "- c") 8))
      "continuation rows indented by the marker width")
  (ok (equal '("3. x" "4. y") (md (nl "3. x" "4. y") 20)))
  (ok (equal '("1) x") (md "1) x" 20)) "the delimiter is kept")
  (ok (equal '(" 9. a" "10. b") (md (nl "9. a" "1. b") 20)) "numbers right-aligned")
  (ok (equal '("- a" "" "  b") (md (nl "- a" "" "  b") 20)) "blocks inside a loose item")
  (ok (equal '("-" "- x") (md (nl "-" "- x") 20)) "empty item: the marker alone, no trailing space")
  (ok (equal '(("- a")) (segments (first (md-lines "- a" 20)))) "the marker is :list-marker (default)"))

(deftest render-lists-blank-in-two-lists
  (ok (equal '("- a" "" "- b") (md (nl "- a" "* b") 20)) "separate lists are separate blocks"))

(deftest render-block-quotes
  (let ((lines (md-lines (nl "> quoted **x**" ">" "> next") 30)))
    (ok (equal (list (u #x2502 " quoted x") (u #x2502) (u #x2502 " next")) (texts lines))
        "a blank row inside the quote is the bar alone")
    (ok (equal (list (list (u #x2502 " ") :faint t) '("quoted " :italic t) '("x" :bold t :italic t))
               (segments (first lines)))))
  (ok (equal (list (u #x2502 " " #x2502 " a")) (md "> > a" 20)) "nested")
  (ok (equal (list (u #x2502 " aaa") (u #x2502 " bbb")) (md "> aaa bbb" 6)) "wrapped at width − 2"))

(deftest render-code-blocks
  (let ((lines (md-lines (nl "```" "(+ 1 2)" "" "x" "```") 20)))
    (ok (equal '("  (+ 1 2)" "" "  x") (texts lines)) "two-cell indent, no fences, empty rows empty")
    (ok (equal '(("  ") ("(+ 1 2)" :fg 6)) (segments (first lines)))))
  (ok (equal '("  abcd" "  efgh" "  ij") (md (nl "```" "abcdefghij" "```") 6))
      "long lines break by cells at width − 2, every row indented")
  (ok (equal '("  a b" "   c") (md (nl "```" "a b c" "```") 5)) "no word wrap in code: the space is kept")
  (ok (equal '("      x") (md (format nil "```~%~Cx~%```~%" #\Tab) 20)) "tabs (4-column stops)")
  (ok (equal '("") (md (nl "```" "```") 20)) "an empty block is one empty line")
  (ok (equal '("p" "" "  x") (md (nl "p" "```" "x") 20)) "unclosed at the end"))

(deftest render-code-highlight-hook
  (let* ((calls '())
         (hook (lambda (language lines)
                 (push (list language lines) calls)
                 (when (equal language "lisp")
                   (mapcar (lambda (l) (list (w:text-run l :foreground 5))) lines))))
         (lines (md-lines (nl "```lisp" "(a)" "(b)" "```" "" "```text" "t" "```") 20
                          :highlight hook)))
    (ok (equal '("  (a)" "  (b)" "" "  t") (texts lines)))
    (ok (equal '(("  ") ("(a)" :fg 5)) (segments (first lines))) "the hook's styles are used")
    (ok (equal '(("  ") ("t" :fg 6)) (segments (fourth lines))) "NIL → the default style")
    (ok (equal '(("text" ("t")) ("lisp" ("(a)" "(b)"))) calls)
        "called once per block with the language and the lines"))
  (let ((lines (md-lines (nl "```" "abc" "```") 20
                         :highlight (lambda (language lines)
                                      (declare (ignore lines))
                                      (and (null language) (list "X" "Y"))))))
    (ok (equal '("  X" "  Y") (texts lines)) "the hook's lines are used as they are")))

(deftest render-thematic-break
  (let ((lines (md-lines (nl "a" "***" "b") 5)))
    (ok (equal (list "a" "" (u #x2500 #x2500 #x2500 #x2500 #x2500) "" "b") (texts lines)))
    (ok (equal (list (list (u #x2500 #x2500 #x2500 #x2500 #x2500) :faint t)) (segments (third lines))))))

(deftest render-widths-bounded
  (dolist (width '(10 17 24 40 80))
    (let ((lines (md-lines *corpus* width)))
      (ok (<= (max-width lines) width) (format nil "every line fits at ~D" width))
      (ok (every (lambda (line) (notany (lambda (r) (find #\Newline (w:text-run-text r))) line)) lines)
          "no newlines inside runs")))
  (ok (md-lines *corpus* 1) "width 1 does not signal"))

(deftest render-blocks-input
  (let ((blocks (md:parse-markdown *corpus*)))
    (ok (equal (texts (md-lines *corpus* 33)) (texts (md:render-markdown blocks 33)))
        "a block list renders like its source")))
