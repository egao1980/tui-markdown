(in-package #:tui-markdown)

;;; ------------------------------------------------------------------
;;; Inline parser (the INLINES rules in markdown.lisp's header).
;;;
;;; One left-to-right scan fills an adjustable vector with strings, finished
;;; inline nodes (lists), DELIM runs (* _ ~~) and BRACKETs ([). Code spans,
;;; escapes and autolinks are resolved during the scan; links when their
;;; "](" tail parses; emphasis by %PROCESS-EMPHASIS (CommonMark §6.3) over
;;; the delimiters above a bottom index (the link's bracket, or the start).

(defstruct (delim (:constructor make-delim (char count can-open can-close
                                            &aux (orig count))))
  char count orig can-open can-close)

(defstruct (bracket (:constructor make-bracket ()))
  (active t))

(declaim (inline %ws-p))
(defun %ws-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char #xA0))))

(defun %punct-p (ch)
  "Unicode-ish punctuation: a graphic character that is neither alphanumeric
nor whitespace."
  (and (graphic-char-p ch) (not (alphanumericp ch)) (not (%ws-p ch))))

(defun %ascii-punct-p (ch)
  (and (< (char-code ch) 128) (graphic-char-p ch) (not (alphanumericp ch))
       (char/= ch #\Space)))

(defun %flanking (prev next)
  "Left- and right-flanking of a delimiter run between PREV and NEXT (chars,
NIL at the ends of the text = whitespace). → (values left right)."
  (let ((pw (or (null prev) (%ws-p prev)))
        (nw (or (null next) (%ws-p next)))
        (pp (and prev (%punct-p prev)))
        (np (and next (%punct-p next))))
    (values (and (not nw) (or (not np) pw pp))
            (and (not pw) (or (not pp) nw np)))))

(defun %merge-inlines (items)
  "ITEMS (strings and nodes) with adjacent strings merged and empty strings
dropped."
  (let ((out '()))
    (dolist (x items (nreverse out))
      (cond ((and (stringp x) (zerop (length x))))
            ((and (stringp x) (stringp (first out)))
             (setf (first out) (concatenate 'string (first out) x)))
            (t (push x out))))))

(defun %element-inline (x)
  (cond ((delim-p x) (make-string (delim-count x) :initial-element (delim-char x)))
        ((bracket-p x) "[")
        (t x)))

(defun %finalize (v from to)
  "Elements FROM..TO of V as inlines: leftover delimiters and brackets as
text, strings merged."
  (%merge-inlines (loop for i from from below to collect (%element-inline (aref v i)))))

(defun %splice (v start end &optional (items '()))
  "Replace V[START, END) by ITEMS, in place."
  (let* ((tail (coerce (subseq v end) 'list))
         (new (append items tail)))
    (setf (fill-pointer v) start)
    (dolist (x new) (vector-push-extend x v))
    v))

(defun %rule-of-3-p (opener closer)
  "True when the multiple-of-3 rule forbids OPENER matching CLOSER."
  (and (or (delim-can-close opener) (delim-can-open closer))
       (zerop (mod (+ (delim-orig opener) (delim-orig closer)) 3))
       (not (and (zerop (mod (delim-orig opener) 3))
                 (zerop (mod (delim-orig closer) 3))))))

(defun %process-emphasis (v bottom)
  "Resolve emphasis among the delimiters of V above index BOTTOM."
  (let ((ci (1+ bottom)))
    (loop
      (setf ci (position-if (lambda (x) (and (delim-p x) (delim-can-close x))) v :start ci))
      (unless ci (return))
      (let* ((c (aref v ci))
             (oi (loop for i from (1- ci) above bottom
                       for o = (aref v i)
                       when (and (delim-p o) (delim-can-open o)
                                 (char= (delim-char o) (delim-char c))
                                 (not (%rule-of-3-p o c)))
                         return i)))
        (if (null oi)
            (progn
              (unless (delim-can-open c)
                (setf (aref v ci) (%element-inline c)))
              (incf ci))
            (let* ((o (aref v oi))
                   (use (if (or (char= (delim-char c) #\~)
                                (and (>= (delim-count o) 2) (>= (delim-count c) 2)))
                            2 1))
                   (kind (cond ((char= (delim-char c) #\~) :strike)
                               ((= use 2) :strong)
                               (t :emph))))
              (%splice v (1+ oi) ci (list (list kind (%finalize v (1+ oi) ci))))
              (setf ci (+ oi 2))
              (decf (delim-count o) use)
              (decf (delim-count c) use)
              (when (zerop (delim-count o))
                (%splice v oi (1+ oi))
                (decf ci))
              (when (zerop (delim-count c))
                (%splice v ci (1+ ci)))))))))

;;; Link tails and autolinks

(defun %skip-ws (s i n)
  (loop while (and (< i n) (%ws-p (char s i))) do (incf i))
  i)

(defun %parse-link-tail (s i n)
  "S[I] should be the char after \"]\". → (values url end) for an inline
link tail \"(dest title)\", END after the \")\"; NIL when there is none."
  (unless (and (< i n) (char= (char s i) #\())
    (return-from %parse-link-tail nil))
  (let ((j (%skip-ws s (1+ i) n))
        (url (make-string-output-stream)))
    (cond
      ((and (< j n) (char= (char s j) #\<))
       (incf j)
       (loop
         (when (>= j n) (return-from %parse-link-tail nil))
         (let ((ch (char s j)))
           (cond ((char= ch #\>) (incf j) (return))
                 ((member ch '(#\< #\Newline)) (return-from %parse-link-tail nil))
                 ((and (char= ch #\\) (< (1+ j) n) (%ascii-punct-p (char s (1+ j))))
                  (write-char (char s (1+ j)) url) (incf j 2))
                 (t (write-char ch url) (incf j))))))
      (t
       (let ((depth 0))
         (loop
           (when (>= j n) (return))
           (let ((ch (char s j)))
             (cond ((or (%ws-p ch) (< (char-code ch) 32)) (return))
                   ((and (char= ch #\\) (< (1+ j) n) (%ascii-punct-p (char s (1+ j))))
                    (write-char (char s (1+ j)) url) (incf j 2))
                   ((char= ch #\() (incf depth) (write-char ch url) (incf j))
                   ((char= ch #\))
                    (when (zerop depth) (return))
                    (decf depth) (write-char ch url) (incf j))
                   (t (write-char ch url) (incf j)))))
         (unless (zerop depth) (return-from %parse-link-tail nil)))))
    (let ((k (%skip-ws s j n)))
      ;; optional title, after at least one whitespace
      (when (and (> k j) (< k n) (member (char s k) '(#\" #\' #\()))
        (let ((close (if (char= (char s k) #\() #\) (char s k)))
              (m (1+ k)))
          (loop
            (when (>= m n) (return-from %parse-link-tail nil))
            (let ((ch (char s m)))
              (cond ((char= ch close) (incf m) (return))
                    ((and (char= ch #\\) (< (1+ m) n)) (incf m 2))
                    (t (incf m)))))
          (setf k (%skip-ws s m n))))
      (if (and (< k n) (char= (char s k) #\)))
          (values (get-output-stream-string url) (1+ k))
          nil))))

(defun %parse-autolink (s i n)
  "S[I] is \"<\". → (values url end) for <scheme:…>, else NIL."
  (let ((j (1+ i)))
    (unless (and (< j n) (alpha-char-p (char s j)) (< (char-code (char s j)) 128))
      (return-from %parse-autolink nil))
    (let ((scheme-start j))
      (incf j)
      (loop while (and (< j n)
                       (let ((ch (char s j)))
                         (and (< (char-code ch) 128)
                              (or (alphanumericp ch) (member ch '(#\+ #\. #\-))))))
            do (incf j))
      (unless (and (<= 2 (- j scheme-start) 32) (< j n) (char= (char s j) #\:))
        (return-from %parse-autolink nil))
      (loop
        (when (>= j n) (return-from %parse-autolink nil))
        (let ((ch (char s j)))
          (cond ((char= ch #\>)
                 (return (values (subseq s (1+ i) j) (1+ j))))
                ((or (char= ch #\<) (%ws-p ch) (< (char-code ch) 32))
                 (return-from %parse-autolink nil))
                (t (incf j))))))))

(defun %code-span-content (raw)
  (let ((c (substitute #\Space #\Newline raw)))
    (if (and (>= (length c) 2)
             (char= (char c 0) #\Space)
             (char= (char c (1- (length c))) #\Space)
             (notevery (lambda (ch) (char= ch #\Space)) c))
        (subseq c 1 (1- (length c)))
        c)))

(defun %run-length (s i n ch)
  (let ((j i))
    (loop while (and (< j n) (char= (char s j) ch)) do (incf j))
    (- j i)))

(defun %find-backtick-run (s start n len)
  "Index of the next run of exactly LEN backticks at or after START, or NIL."
  (let ((j start))
    (loop
      (setf j (position #\` s :start j))
      (unless j (return nil))
      (let ((k (%run-length s j n #\`)))
        (when (= k len) (return j))
        (incf j k)))))

;;; The scan

(defun %parse-inlines (s)
  "STRING → a list of inlines (see PARSE-MARKDOWN)."
  (let* ((n (length s))
         (v (make-array 16 :adjustable t :fill-pointer 0))
         (text (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
         (i 0))
    (labels ((flush ()
               (when (plusp (length text))
                 (vector-push-extend (coerce text 'simple-string) v)
                 (setf (fill-pointer text) 0)))
             (emit (x) (flush) (vector-push-extend x v))
             (add-char (ch) (vector-push-extend ch text))
             (add-string (str) (loop for ch across str do (add-char ch)))
             (trailing-spaces ()
               ;; Trim the spaces before a newline from the pending text.
               (let ((k 0))
                 (loop while (and (plusp (fill-pointer text))
                                  (char= (char text (1- (fill-pointer text))) #\Space))
                       do (decf (fill-pointer text)) (incf k))
                 k)))
      (loop while (< i n)
            do (let ((ch (char s i)))
                 (cond
                   ;; backslash
                   ((char= ch #\\)
                    (cond ((and (< (1+ i) n) (char= (char s (1+ i)) #\Newline))
                           (trailing-spaces)
                           (emit (list :hard-break))
                           (setf i (%skip-spaces s (+ i 2) n)))
                          ((and (< (1+ i) n) (%ascii-punct-p (char s (1+ i))))
                           (add-char (char s (1+ i)))
                           (incf i 2))
                          (t (add-char ch) (incf i))))
                   ;; code span
                   ((char= ch #\`)
                    (let* ((len (%run-length s i n #\`))
                           (close (%find-backtick-run s (+ i len) n len)))
                      (if close
                          (progn (emit (list :code (%code-span-content
                                                    (subseq s (+ i len) close))))
                                 (setf i (+ close len)))
                          (progn (add-string (subseq s i (+ i len)))
                                 (incf i len)))))
                   ;; emphasis delimiters
                   ((member ch '(#\* #\_ #\~))
                    (let* ((len (%run-length s i n ch))
                           (prev (and (> i 0) (char s (1- i))))
                           (next (and (< (+ i len) n) (char s (+ i len)))))
                      (multiple-value-bind (left right) (%flanking prev next)
                        (multiple-value-bind (open close)
                            (if (char= ch #\_)
                                (values (and left (or (not right) (and prev (%punct-p prev))))
                                        (and right (or (not left) (and next (%punct-p next)))))
                                (values left right))
                          (if (and (or open close) (or (char/= ch #\~) (= len 2)))
                              (emit (make-delim ch len open close))
                              (add-string (subseq s i (+ i len))))))
                      (incf i len)))
                   ;; links
                   ((char= ch #\[)
                    (emit (make-bracket))
                    (incf i))
                   ((char= ch #\])
                    (flush)
                    (let ((bi (position-if #'bracket-p v :from-end t)))
                      (cond
                        ((null bi) (add-char ch) (incf i))
                        ((not (bracket-active (aref v bi)))
                         (setf (aref v bi) "[")
                         (add-char ch) (incf i))
                        (t
                         (multiple-value-bind (url end) (%parse-link-tail s (1+ i) n)
                           (if url
                               (progn
                                 (%process-emphasis v bi)
                                 (let ((link (list :link url (%finalize v (1+ bi) (length v)))))
                                   (setf (fill-pointer v) bi)
                                   (loop for x across v
                                         when (bracket-p x) do (setf (bracket-active x) nil))
                                   (vector-push-extend link v))
                                 (setf i end))
                               (progn
                                 (setf (aref v bi) "[")
                                 (add-char ch)
                                 (incf i))))))))
                   ;; autolinks
                   ((char= ch #\<)
                    (multiple-value-bind (url end) (%parse-autolink s i n)
                      (if url
                          (progn (emit (list :link url (list url)))
                                 (setf i end))
                          (progn (add-char ch) (incf i)))))
                   ;; line breaks
                   ((char= ch #\Newline)
                    (let ((spaces (trailing-spaces)))
                      (emit (if (>= spaces 2) (list :hard-break) (list :soft-break)))
                      (setf i (%skip-spaces s (1+ i) n))))
                   (t (add-char ch) (incf i)))))
      (flush)
      (%process-emphasis v -1)
      (%finalize v 0 (length v)))))

(defun %skip-spaces (s i n)
  (loop while (and (< i n) (member (char s i) '(#\Space #\Tab))) do (incf i))
  i)
