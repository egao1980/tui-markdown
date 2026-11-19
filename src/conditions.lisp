(in-package #:tui-markdown)

;;; The renderers are total: malformed markdown or diff text renders as
;;; text, never signals. Bad arguments signal TYPE-ERROR (CHECK-TYPE). A
;;; DIFF-PROTOCOL value that cannot be rendered as :UNIFIED propagates
;;; DIFF:UNSUPPORTED-CONTENT. The only condition of our own:

(define-condition tui-markdown-error (error) ()
  (:documentation "Root of tui-markdown's conditions."))

(define-condition markdown-stream-finished (tui-markdown-error)
  ((stream :initarg :stream :reader markdown-stream-finished-stream))
  (:report (lambda (c s)
             (format s "~S is finished; no more text can be appended"
                     (markdown-stream-finished-stream c))))
  (:documentation "MARKDOWN-STREAM-APPEND after MARKDOWN-STREAM-FINISH."))
