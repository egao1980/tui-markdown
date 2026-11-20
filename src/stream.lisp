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

;;;
;;; State: SOURCE grows; on a query the complete text is parsed to blocks B,
;;; the first k = |B| − 1 (all of B once finished) are committed. Their lines
;;; (each block's lines plus one ()) are cached with the width they were
;;; rendered at and only blocks beyond the cached count are rendered, so a
;;; committed code block meets the highlight hook once per width. The last
;;; query's result is memoised by (source length, width, finished).

(defclass markdown-stream ()
  ((width :initarg :width :initform 80)
   (theme :initarg :theme :initform nil)
   (highlight :initarg :highlight :initform nil)
   (show-urls :initarg :show-urls :initform t)
   (source :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
   (finished :initform nil)
   (cache-width :initform nil)
   (cache-count :initform 0)            ; committed blocks rendered into CACHE-LINES
   (cache-lines :initform '())          ; their lines, each block followed by ()
   (memo-key :initform nil)
   (memo-lines :initform '())
   (memo-count :initform 0))
  (:documentation "Markdown text that grows by chunks (MAKE-MARKDOWN-STREAM).
Not thread-safe: append and query from one thread (the TUI's update)."))

(defun make-markdown-stream (&key (width 80) theme highlight (show-urls t))
  "A MARKDOWN-STREAM rendering at WIDTH (an integer ≥ 1, else TYPE-ERROR)
with RENDER-MARKDOWN's THEME, HIGHLIGHT and SHOW-URLS."
  (check-type width (integer 1))
  (check-type theme list)
  (make-instance 'markdown-stream :width width :theme theme :highlight highlight
                                  :show-urls show-urls))

(defun markdown-stream-width (stream)
  "The width STREAM renders at."
  (check-type stream markdown-stream)
  (slot-value stream 'width))

(defun (setf markdown-stream-width) (width stream)
  "Set the width (an integer ≥ 1, else TYPE-ERROR); a different width
renders every line again at the next query. → WIDTH."
  (check-type width (integer 1))
  (check-type stream markdown-stream)
  (setf (slot-value stream 'width) width))

(defun markdown-stream-source (stream)
  "All text appended so far (a fresh string)."
  (check-type stream markdown-stream)
  (coerce (slot-value stream 'source) 'simple-string))

(defun markdown-stream-append (stream text)
  "Append TEXT (a string, else TYPE-ERROR) to STREAM. → STREAM. After
MARKDOWN-STREAM-FINISH this signals MARKDOWN-STREAM-FINISHED."
  (check-type stream markdown-stream)
  (when (slot-value stream 'finished)
    (error 'markdown-stream-finished :stream stream))
  (check-type text string)
  (let ((source (slot-value stream 'source)))
    (loop for ch across text do (vector-push-extend ch source)))
  stream)

(defun markdown-stream-finish (stream)
  "Mark the end of the text: every block is complete. → STREAM. Idempotent."
  (check-type stream markdown-stream)
  (setf (slot-value stream 'finished) t)
  stream)

(defun markdown-stream-finished-p (stream)
  "True after MARKDOWN-STREAM-FINISH."
  (check-type stream markdown-stream)
  (slot-value stream 'finished))

(defun %fence-prefix-p (partial fence)
  "True when PARTIAL is 0–3 spaces then fewer than (cdr FENCE) of (car FENCE)."
  (let* ((ind (or (position #\Space partial :test-not #'char=) (length partial)))
         (k (- (or (position (car fence) partial :start ind :test-not #'char=)
                   (length partial))
               ind)))
    (and (<= ind 3)
         (= (+ ind k) (length partial))
         (< k (cdr fence)))))

(defun %stream-ctx (stream)
  (with-slots (width theme highlight show-urls) stream
    (%make-ctx width theme highlight show-urls)))

(defun %commit (stream blocks k ctx)
  "Render the first K of BLOCKS into the cache (only those not yet there)."
  (with-slots (width cache-width cache-count cache-lines) stream
    (unless (eql cache-width width)
      (setf cache-width width cache-count 0 cache-lines '()))
    (when (> k cache-count)
      (setf cache-lines
            (append cache-lines
                    (loop for b in (subseq blocks cache-count k)
                          append (%render-block b ctx)
                          collect '()))
            cache-count k))))

(defun %stream-update (stream)
  "Bring the memo up to date. → (values lines count)."
  (with-slots (source finished width memo-key memo-lines memo-count cache-lines) stream
    (let ((key (list (length source) width finished)))
      (unless (equal key memo-key)
        (let* ((src (coerce source 'simple-string))
               (ctx (%stream-ctx stream))
               (nl (position #\Newline src :from-end t))
               (complete (if finished src (if nl (subseq src 0 (1+ nl)) "")))
               (partial (if nl (subseq src (1+ nl)) src)))
          (multiple-value-bind (blocks open-fence) (mdp:parse-markdown-document complete)
            (let ((k (if finished (length blocks) (max 0 (1- (length blocks))))))
              (%commit stream blocks k ctx)
              (if finished
                  (setf memo-lines (butlast cache-lines))
                  (let* ((visible (if (and open-fence (%fence-prefix-p partial open-fence))
                                      complete
                                      src))
                         (live (%render-blocks (nthcdr k (values (mdp:parse-markdown-document visible))) ctx)))
                    (setf memo-lines (append cache-lines live))))
              (setf memo-count (if finished (length memo-lines) (length cache-lines))
                    memo-key key)))))
      (values memo-lines memo-count))))

(defun markdown-stream-lines (stream)
  "Every line of STREAM at its width: the committed lines, then the live
ones (one empty line between when both exist). A list of lines (lists of
W:TEXT-RUNs); callers must not mutate it."
  (check-type stream markdown-stream)
  (values (%stream-update stream)))

(defun markdown-stream-committed-count (stream)
  "How many leading lines of MARKDOWN-STREAM-LINES are final. After FINISH,
all of them."
  (check-type stream markdown-stream)
  (nth-value 1 (%stream-update stream)))
