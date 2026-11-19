(in-package #:tui-markdown/tests)

(defparameter *git-diff*
  (nl "diff --git a/src/x.lisp b/src/x.lisp"
      "index 3b18e51..a2c9f4e 100644"
      "--- a/src/x.lisp"
      "+++ b/src/x.lisp"
      "@@ -1,4 +1,4 @@"
      " (defun f (x)"
      "-  (+ x 1))"
      "+  (+ x 2))"
      " ;; end"
      " more"
      "@@ -10,2 +10,3 @@ (defun g ()"
      " a"
      "+b"
      " c"))

(deftest diff-unified-layout
  (ok (equal '("src/x.lisp +2 -1"
               " 1  (defun f (x)"
               " 2 -  (+ x 1))"
               " 2 +  (+ x 2))"
               " 3  ;; end"
               " 4  more"
               "   ⋮"
               "10  a"
               "11 +b"
               "12  c")
             (dl *git-diff* 40))
      "header with counts; number = old for -, new otherwise; ⋮ between hunks; @@ lines hidden")
  (ok (all-runs-p (md:render-diff-lines *git-diff* 40)))
  (ok (null (md:render-diff-lines "" 40))))

(deftest diff-styles
  (let ((lines (md:render-diff-lines *git-diff* 40)))
    (ok (equal '(("src/x.lisp" :bold t) (" ") ("+2" :fg 2) (" ") ("-1" :fg 1))
               (segments (nth 0 lines))))
    (ok (equal '((" 1" :faint t) ("  (defun f (x)")) (segments (nth 1 lines)))
        "context rows: faint number, default text")
    (ok (equal '((" 2" :faint t) (" ") ("-  (+ x " :fg 1) ("1" :fg 1 :inverse t) ("))" :fg 1))
               (segments (nth 2 lines)))
        "a paired - / + line highlights the changed words")
    (ok (equal '((" 2" :faint t) (" ") ("+  (+ x " :fg 2) ("2" :fg 2 :inverse t) ("))" :fg 2))
               (segments (nth 3 lines))))
    (ok (equal '(("   ") ("⋮" :faint t)) (segments (nth 6 lines))))
    (ok (equal '(("11" :faint t) (" ") ("+b" :fg 2)) (segments (nth 8 lines)))
        "an unpaired line has no word highlighting"))
  (let ((lines (md:render-diff-lines *git-diff* 40 :word-diff nil)))
    (ok (equal '((" 2" :faint t) (" ") ("-  (+ x 1))" :fg 1)) (segments (nth 2 lines))) ":word-diff nil"))
  (let ((lines (md:render-diff-lines *git-diff* 40
                                     :theme (list :diff-added (tp:make-style :background 22)))))
    (ok (equal '(("11" :faint t) (" ") ("+b" :bg 22)) (segments (nth 8 lines))) "themes apply")))

(deftest diff-word-pairing
  (let ((lines (md:render-diff-lines (nl "@@ -1,2 +1,1 @@" "-a = 1" "-b = 2" "+a = 3") 40)))
    (ok (equal '("1 -a = 1" "2 -b = 2" "1 +a = 3") (texts lines)) "- rows of a block, then + rows")
    (ok (equal '(("1" :faint t) (" ") ("-a = " :fg 1) ("1" :fg 1 :inverse t)) (segments (first lines)))
        "the i-th - line pairs with the i-th + line")
    (ok (equal '(("2" :faint t) (" ") ("-b = 2" :fg 1)) (segments (second lines))))
    (ok (equal '(("1" :faint t) (" ") ("+a = " :fg 2) ("3" :fg 2 :inverse t)) (segments (third lines)))))
  (let ((lines (md:render-diff-lines (nl "@@ -1 +1 @@" "-old line here" "+new text there") 40)))
    (ok (equal '(("1" :faint t) (" ") ("-old line here" :fg 1)) (segments (first lines)))
        "lines sharing less than half their characters: no word highlighting"))
  (let ((lines (md:render-diff-lines (nl "@@ -1 +1 @@" "-call(a, b)" "+call(a, b, c)") 40)))
    (ok (equal '(("1" :faint t) (" ") ("-call(a, b)" :fg 1)) (segments (first lines)))
        "nothing removed: no inverse on the - line")
    (ok (equal '(("1" :faint t) (" ") ("+call(a, b" :fg 2) (", c" :fg 2 :inverse t) (")" :fg 2))
               (segments (second lines))))))

(deftest diff-wrapping-and-text
  (ok (equal '("1 +abcdefghi" "   jklmnop") (dl (nl "@@ -0,0 +1 @@" "+abcdefghijklmnop") 12))
      "content breaks by cells at width − gutter − 2; continuation rows have no number or sign")
  (ok (equal '("1 +日本語日" "   本") (dl (nl "@@ -0,0 +1 @@" "+日本語日本") 11)) "wide characters")
  (ok (equal '("1 +    x") (dl (format nil "@@ -0,0 +1 @@~%+~Cx~%" #\Tab) 40)) "tabs: 4-column stops")
  (ok (equal '("1 -a" "1 +b") (dl (format nil "@@ -1 +1 @@~%-a~C~%+b~C~%" #\Return #\Return) 40))
      "a CR before the newline is not shown")
  (ok (equal '("1 -a" "1 +a") (dl (nl "@@ -1 +1 @@" "-a" "\\ No newline at end of file" "+a") 40))
      "no-newline markers are skipped")
  (ok (equal '("1 --- a" "1 +++ b") (dl (nl "@@ -1 +1 @@" "--- a" "+++ b") 40))
      "hunk bodies are read by the header's counts")
  (ok (equal '("1  " "2 +x") (dl (nl "@@ -1 +1,2 @@" "" "+x") 40)) "an empty body line is context"))

(deftest diff-values
  (let* ((engine (diff:find-diff-engine :text))
         (d (diff:compute-diff engine (nl "a" "b" "c" "d" "e" "f") (nl "a" "B" "c" "d" "e" "F") :context 1)))
    (ok (equal '("1  a" "2 -b" "2 +B" "3  c" "  ⋮" "5  e" "6 -f" "6 +F")
               (dl d 40))
        "a text diff value: bare hunks, no file header")
    (ok (equal (dl (diff:render-diff d :unified nil) 40) (dl d 40)) "the same as its unified text"))
  (ok (signals (md:render-diff-lines (diff:compute-diff (diff:find-diff-engine :sexp) '(:a 1) '(:a 2)) 40)
               'diff:unsupported-content)
      "values diff-protocol cannot render as :unified"))

(deftest diff-tree-sections
  (let* ((a (diff:make-tree "a.txt" (diff:make-tree-file (nl "x"))
                            "old.txt" (diff:make-tree-file (nl "1" "2" "3"))
                            "b.bin" (diff:make-tree-file (make-array 2 :element-type '(unsigned-byte 8)
                                                                       :initial-contents '(1 2))
                                                         :type :binary)))
         (b (diff:make-tree "a.txt" (diff:make-tree-file (nl "x") :mode #o100755)
                            "new.txt" (diff:make-tree-file (nl "1" "2" "3"))
                            "add.txt" (diff:make-tree-file (nl "n"))
                            "b.bin" (diff:make-tree-file (make-array 2 :element-type '(unsigned-byte 8)
                                                                       :initial-contents '(1 3))
                                                         :type :binary)))
         (d (diff:compute-diff (diff:find-diff-engine :tree) a b))
         (lines (md:render-diff-lines d 60)))
    (ok (equal '("a.txt +0 -0"
                 "old mode 100644"
                 "new mode 100755"
                 ""
                 "add.txt +1 -0"
                 "new file mode 100644"
                 "1 +n"
                 ""
                 "b.bin +0 -0"
                 "Binary files a/b.bin and b/b.bin differ"
                 ""
                 "old.txt → new.txt +0 -0")
               (texts lines))
        "one section per file, a blank line between; git header lines other than index / rename / similarity shown")
    (ok (equal '(("new file mode 100644" :faint t)) (segments (nth 5 lines))))
    (ok (equal '(("old.txt → new.txt" :bold t) (" ") ("+0" :fg 2) (" ") ("-0" :fg 1))
               (segments (nth 11 lines))))))

(deftest diff-split-layout
  (ok (equal '("1  keep        │1  keep"
               "2 -gone one    │2 +new"
               "3 -gone two    │")
             (dl (nl "@@ -1,3 +1,2 @@" " keep" "-gone one" "-gone two" "+new") 31 :layout :split))
      "left = old, right = new; a change block pairs rows; the left pane is padded")
  (let ((lines (md:render-diff-lines (nl "@@ -1 +1 @@" "-a = 1" "+a = 2") 21 :layout :split)))
    (ok (equal '("1 -a = 1  │1 +a = 2") (texts lines)))
    (ok (equal '(("1" :faint t) (" ") ("-a = " :fg 1) ("1" :fg 1 :inverse t) ("  ")
                 ("│1" :faint t) (" ") ("+a = " :fg 2) ("2" :fg 2 :inverse t))
               (segments (first lines)))))
  (ok (equal '("1  abcdef│1  abcdef" "   ghij  │   ghij")
             (dl (nl "@@ -1 +1 @@" " abcdefghij") 19 :layout :split))
      "panes: ⌊(W − 1)/2⌋ and the rest; each wraps on its own")
  (ok (equal '("2 -x   │2 +y" "  ⋮    │  ⋮" "9  z   │9  z")
             (dl (nl "@@ -2 +2 @@" "-x" "+y" "@@ -9 +9 @@" " z") 15 :layout :split))
      "the hunk gap on both sides")
  (ok (equal '("f +1 -0" "                   │1 +x")
             (dl (nl "--- a/f" "+++ b/f" "@@ -0,0 +1 @@" "+x") 40 :layout :split))
      "headers span the width; a side with no line is blank"))

(deftest diff-split-wrap-heights
  (ok (equal '("1 -aaaaaaa│1 +b"
               "   a      │")
             (dl (nl "@@ -1 +1 @@" "-aaaaaaaa" "+b") 21 :layout :split))
      "a row is as tall as its taller side"))

(deftest diff-auto-layout
  (let ((d (nl "@@ -1 +1 @@" "-a" "+b")))
    (ok (equal (dl d 120 :layout :split) (dl d 120 :layout :auto)) ":auto splits at 120 cells")
    (ok (equal (dl d 119 :layout :unified) (dl d 119 :layout :auto)))
    (ok (equal (dl d 60 :layout :split) (dl d 60 :layout :auto :split-min-width 60)))))

(deftest diff-loose-text
  (ok (equal '("hello" "world") (dl (nl "hello" "world") 40)) "lines outside any hunk are meta lines")
  (ok (equal '(("hello" :faint t)) (segments (first (md:render-diff-lines (nl "hello") 40)))))
  (ok (equal '("f +1 -0" "1 +x") (dl (nl "--- a/f	2024-01-01" "+++ b/f	2024-01-02" "@@ -0,0 +1 @@" "+x") 40))
      "diff -u headers: timestamps after a TAB dropped")
  (ok (equal '("p +0 -1" "1 -x" "" "q +1 -0" "1 +y")
             (dl (nl "--- a/p" "+++ b/p" "@@ -1 +0,0 @@" "-x" "--- a/q" "+++ b/q" "@@ -0,0 +1 @@" "+y") 40))
      "a new ---/+++ pair after hunks starts a new section")
  (ok (signals (md:render-diff-lines "" 0) 'type-error))
  (ok (signals (md:render-diff-lines "" 10 :layout :wide) 'type-error))
  (ok (signals (md:render-diff-lines 42 10) 'type-error)))
