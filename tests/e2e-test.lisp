(in-package #:tui-markdown/tests)

;;; The renderers' lines in a W:VIEWPORT (:wrap nil) on the headless driver:
;;; the emulator's screen must show exactly the rendered strings.

(deftest e2e-markdown-on-screen
  (let ((lines (md-lines (nl "# Title" "" "Some **bold** 日本語 text." "" "- a" "  - b" "" "> q") 30)))
    (with-lines-tui (tui lines :cols 30 :rows 9)
      (ok (screen-is tui (list "Title" "" "Some bold 日本語 text." "" "- a" "  - b" ""
                               (u #x2502 " q")))))))

(deftest e2e-table-on-screen
  (let ((lines (md-lines (nl "| 名前 | n |" "|---|--:|" "| 日本 | 10 |") 30)))
    (with-lines-tui (tui lines :cols 30 :rows 6)
      (ok (screen-is tui '("┌──────┬────┐"
                           "│ 名前 │  n │"
                           "├──────┼────┤"
                           "│ 日本 │ 10 │"
                           "└──────┴────┘"))))))

(deftest e2e-diff-on-screen
  (let ((lines (md:render-diff-lines *git-diff* 30)))
    (with-lines-tui (tui lines :cols 30 :rows 10)
      (ok (screen-is tui '("src/x.lisp +2 -1"
                           " 1  (defun f (x)"
                           " 2 -  (+ x 1))"
                           " 2 +  (+ x 2))"
                           " 3  ;; end"
                           " 4  more"
                           "   ⋮"
                           "10  a"
                           "11 +b"
                           "12  c"))))))

(deftest e2e-split-diff-on-screen
  (let ((lines (md:render-diff-lines (nl "@@ -1,2 +1,2 @@" " 日本" "-x = 1" "+x = 2") 21 :layout :split)))
    (with-lines-tui (tui lines :cols 21 :rows 3)
      (ok (screen-is tui '("1  日本   │1  日本"
                           "2 -x = 1  │2 +x = 2"))))))
