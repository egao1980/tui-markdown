(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Streaming: model text arrives in chunks. The rendering is split into
;;; COMMITTED lines, which never change again, and LIVE lines after them.
;;;
;;;  - Completion is decided on complete lines only (the source up to its
;;;    last #\Newline): of the top-level blocks they parse to, every block
;;;    but the last is complete. A partial line never completes a block.
;;;    In this subset (no lazy continuation, no setext headings, no
;;;    reference definitions) a later line cannot change an earlier block.
;;;  - Committed lines = the complete blocks' lines with the empty line
;;;    after each. They are rendered once and kept: appends do not render
;;;    them again (a code block's highlight hook is not called again).
;;;  - Live lines = the last block plus the partial line, parsed and
;;;    rendered together on each query. Inside an open fenced code block,
;;;    a partial line of 0–3 spaces and fewer fence characters than the
;;;    opening fence is not shown (so a closing fence never flickers in as
;;;    code).
;;;  - MARKDOWN-STREAM-FINISH completes everything: the lines are then
;;;    RENDER-MARKDOWN of the source, and all are committed.
;;;  - At any moment, the committed lines are a prefix of the final
;;;    rendering at the same width, and the count never decreases while the
;;;    width stays. A new width renders everything again.

(defclass markdown-stream ()
  ((width :initarg :width :initform 80)
   (theme :initarg :theme :initform nil)
   (highlight :initarg :highlight :initform nil)
   (show-urls :initarg :show-urls :initform t))
  (:documentation "Markdown text that grows by chunks (MAKE-MARKDOWN-STREAM).
Not thread-safe: append and query from one thread (the TUI's update)."))

(defun make-markdown-stream (&key (width 80) theme highlight (show-urls t))
  "A MARKDOWN-STREAM rendering at WIDTH (an integer ≥ 1, else TYPE-ERROR)
with RENDER-MARKDOWN's THEME, HIGHLIGHT and SHOW-URLS."
  (declare (ignore width theme highlight show-urls))
  (%not-implemented 'make-markdown-stream))

(defun markdown-stream-width (stream)
  "The width STREAM renders at."
  (declare (ignore stream))
  (%not-implemented 'markdown-stream-width))

(defun (setf markdown-stream-width) (width stream)
  "Set the width (an integer ≥ 1, else TYPE-ERROR); a different width
renders every line again at the next query. → WIDTH."
  (declare (ignore width stream))
  (%not-implemented '(setf markdown-stream-width)))

(defun markdown-stream-source (stream)
  "All text appended so far (a fresh string)."
  (declare (ignore stream))
  (%not-implemented 'markdown-stream-source))

(defun markdown-stream-append (stream text)
  "Append TEXT (a string, else TYPE-ERROR) to STREAM. → STREAM. After
MARKDOWN-STREAM-FINISH this signals MARKDOWN-STREAM-FINISHED."
  (declare (ignore stream text))
  (%not-implemented 'markdown-stream-append))

(defun markdown-stream-finish (stream)
  "Mark the end of the text: every block is complete. → STREAM. Idempotent."
  (declare (ignore stream))
  (%not-implemented 'markdown-stream-finish))

(defun markdown-stream-finished-p (stream)
  "True after MARKDOWN-STREAM-FINISH."
  (declare (ignore stream))
  (%not-implemented 'markdown-stream-finished-p))

(defun markdown-stream-lines (stream)
  "Every line of STREAM at its width: the committed lines, then the live
ones (one empty line between when both exist). A list of lines (lists of
W:TEXT-RUNs); callers must not mutate it."
  (declare (ignore stream))
  (%not-implemented 'markdown-stream-lines))

(defun markdown-stream-committed-count (stream)
  "How many leading lines of MARKDOWN-STREAM-LINES are final. After FINISH,
all of them."
  (declare (ignore stream))
  (%not-implemented 'markdown-stream-committed-count))
