(defpackage #:tui-markdown/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:md #:tui-markdown) (#:w #:tui-protocol/widgets)
                    (#:tui #:tui-protocol) (#:tp #:terminal-protocol)
                    (#:diff #:diff-protocol) (#:h #:tui-backend-terminal)
                    (#:tl #:terminal-backend-lisp)))
(in-package #:tui-markdown/tests)

;;; Lines are compared as strings (W:RUNS-STRING) and, where styles matter,
;;; as SEGMENTS: maximal stretches of characters with the same style, each
;;; (text . style-keys). Run boundaries themselves are not part of the
;;; contract, so an implementation may split or merge runs freely.

(defun u (&rest parts)
  "Concatenate strings, characters and code points: (u \"e\" #x301) → é."
  (with-output-to-string (s)
    (dolist (p parts)
      (etypecase p
        (string (write-string p s))
        (character (write-char p s))
        (integer (write-char (code-char p) s))))))

(defun nl (&rest lines)
  "LINES joined with #\\Newline, each followed by one."
  (format nil "~{~A~%~}" lines))

(defun texts (lines) (mapcar #'w:runs-string lines))

(defun style-keys (style)
  "The non-default attributes of STYLE as a plist, in a fixed order:
:fg :bg :bold :faint :italic :underline :inverse :strike :link."
  (let ((out '()))
    (flet ((add (k v) (setf out (append out (list k v)))))
      (unless (eq (tp:style-foreground style) :default) (add :fg (tp:style-foreground style)))
      (unless (eq (tp:style-background style) :default) (add :bg (tp:style-background style)))
      (when (tp:style-bold style) (add :bold t))
      (when (tp:style-faint style) (add :faint t))
      (when (tp:style-italic style) (add :italic t))
      (when (tp:style-underline style) (add :underline (tp:style-underline style)))
      (when (tp:style-inverse style) (add :inverse t))
      (when (tp:style-strikethrough style) (add :strike t))
      (when (tp:style-hyperlink style) (add :link (tp:style-hyperlink style))))
    out))

(defun segments (line)
  "LINE (a list of runs) → ((text . style-keys) …), adjacent characters with
equal keys merged."
  (let ((out '()))
    (dolist (run (w:as-runs line))
      (let ((keys (style-keys (w:text-run-style run)))
            (text (w:text-run-text run)))
        (when (plusp (length text))
          (if (and out (equal (cdr (first out)) keys))
              (setf (car (first out)) (concatenate 'string (car (first out)) text))
              (push (cons text keys) out)))))
    (nreverse out)))

(defun md (source width &rest keys)
  "SOURCE rendered at WIDTH → strings."
  (texts (apply #'md:render-markdown source width keys)))

(defun md-lines (source width &rest keys)
  (apply #'md:render-markdown source width keys))

(defun dl (diff width &rest keys)
  "DIFF rendered at WIDTH → strings."
  (texts (apply #'md:render-diff-lines diff width keys)))

(defun all-runs-p (lines)
  (every (lambda (line) (and (listp line) (every #'w:text-run-p line))) lines))

(defun max-width (lines)
  (reduce #'max lines :key #'w:runs-width :initial-value 0))

(defparameter *corpus*
  (nl "# Release notes"
      ""
      "Some *emphasis*, **strong** and `code` with a [link](https://example.com/a)."
      "A second line of the same paragraph, long enough to wrap twice at narrow widths."
      ""
      "## Changes"
      ""
      "- first item"
      "- second item with `inline code`"
      "  - nested item"
      "- third"
      ""
      "1. one"
      "2. two"
      ""
      "> quoted text that is also long enough to wrap"
      "> > nested quote"
      ""
      "```lisp"
      "(defun hello (name)"
      "  (format t \"Hello, ~A!~%\" name))"
      ""
      "(hello \"日本語\")"
      "```"
      ""
      "| name | value | note |"
      "|:-----|------:|:----:|"
      "| a | 1 | first |"
      "| 日本 | 22 | wide |"
      ""
      "***"
      ""
      "Last paragraph with ~~strike~~ and a hard  "
      "break.")
  "A document touching every block kind.")

;;; ------------------------------------------------------------------
;;; End to end: lines in a viewport on tui-backend-terminal's headless
;;; driver with the pure-Lisp emulator.

(defun emulator () (make-instance 'tl:lisp-backend))

(defun pad (rows lines)
  "LINES followed by \"\" up to ROWS rows."
  (append lines (make-list (max 0 (- rows (length lines))) :initial-element "")))

(defmacro with-lines-tui ((tui lines &key (cols 30) (rows 8)) &body body)
  "Show LINES in a non-wrapping viewport on a headless COLS × ROWS screen."
  (let ((program (gensym "PROGRAM")))
    `(let* ((,program (w:make-widget-program
                       (make-instance 'w:viewport :lines ,lines :wrap nil)))
            (,tui (h:make-headless-tui ,program :cols ,cols :rows ,rows
                                                :terminal-backend (emulator))))
       (unwind-protect (progn ,@body)
         (when (h:tui-running-p ,tui) (ignore-errors (h:tui-stop ,tui)))))))

(defun screen-is (tui expected &key (timeout 2))
  "Wait until the screen shows EXPECTED (rows after the given ones blank).
→ T, or NIL on timeout."
  (and (h:tui-wait-until
        tui (lambda ()
              (let ((screen (h:tui-screen tui)))
                (equal screen (pad (length screen) expected))))
        :timeout timeout)
       t))
