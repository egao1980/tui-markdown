(in-package #:tui-markdown/tests)

(deftest table-natural-widths
  (ok (equal '("┌───┬────┬───┐"
               "│ a │ bb │ c │"
               "├───┼────┼───┤"
               "│ 1 │ 2  │ 3 │"
               "│ x │ yy │ z │"
               "└───┴────┴───┘")
             (md (nl "| a | bb | c |" "|---|----|---|" "| 1 | 2 | 3 |" "| x | yy | z |") 30))))

(deftest table-alignment
  (ok (equal '("┌──────┬─────┬───────┐"
               "│ left │ mid │ right │"
               "├──────┼─────┼───────┤"
               "│ a    │  b  │     c │"
               "│ aa   │ bb  │    cc │"
               "└──────┴─────┴───────┘")
             (md (nl "| left | mid | right |" "|:--|:-:|--:|" "| a | b | c |" "| aa | bb | cc |") 40))
      "center puts the odd space on the right"))

(deftest table-styles
  (let ((lines (md-lines (nl "| **h** | k |" "|---|---|" "| `v` | w |") 30)))
    (ok (equal '(("┌───┬───┐" :faint t)) (segments (first lines))))
    (ok (equal '(("│" :faint t) (" ") ("h" :bold t) (" ") ("│" :faint t) (" ") ("k" :bold t) (" ")
                 ("│" :faint t))
               (segments (second lines)))
        "header cells get :table-header overlaid")
    (ok (equal '(("│" :faint t) (" ") ("v" :fg 6) (" ") ("│" :faint t) (" w ") ("│" :faint t))
               (segments (fourth lines))))))

(deftest table-shrinks-and-wraps
  (ok (equal '("┌───┬────────┐"
               "│ k │ v      │"
               "├───┼────────┤"
               "│ x │ some   │"
               "│   │ words  │"
               "│   │ here   │"
               "└───┴────────┘")
             (md (nl "| k | v |" "|---|---|" "| x | some words here |") 14))
      "the widest column loses a cell at a time; cells word-wrap")
  (ok (equal '("┌─────┬─────┐"
               "│ aaa │ bbb │"
               "│ aa  │ bbb │"
               "├─────┼─────┤"
               "│ 1   │ 2   │"
               "└─────┴─────┘")
             (md (nl "| aaaaa | bbbbbb |" "|---|---|" "| 1 | 2 |") 13))
      "ties: the leftmost widest first; words longer than the column break by cells"))

(deftest table-fallback
  (let ((lines (md-lines (nl "| aaa | bbb | ccc |" "|---|---|---|" "| 1 | 2 | 3 |") 10)))
    (ok (equal '("aaa | bbb" "| ccc" "1 | 2 | 3") (texts lines))
        "too narrow for columns of min(natural, 3): rows as wrapped text joined by \" | \"")
    (ok (equal '(("aaa" :bold t) (" | " :faint t) ("bbb" :bold t)) (segments (first lines))))))

(deftest table-wide-characters
  (ok (equal '("┌──────┬───┐"
               "│ 名前 │ v │"
               "├──────┼───┤"
               "│ 日本 │ 1 │"
               "│ a    │   │"
               "└──────┴───┘")
             (md (nl "| 名前 | v |" "|---|---|" "| 日本 | 1 |" "| a |") 20))
      "widths in cells; a missing cell is empty"))
