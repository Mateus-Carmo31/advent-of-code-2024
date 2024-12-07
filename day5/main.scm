;; Rule datatype
(define rules? table?)
(define (make-page-order-rules) (make-table init: '()))
(define (get-rule rules page)
  (table-ref rules page))
(define (valid-order? rules before after)
  ;; An order is valid only if:
  ;; - There is a rule allowing "before" before "after"
  ;; - There isn't a rule saying that "after" must come before "before"
  (or (memq after (get-rule rules before))
      (not (memq before (get-rule rules after)))))
(define (add-rule! rules before after)
  (if (table? rules)
      (table-set! rules before (cons after (get-rule rules before)))
      (error "Incorrect type passed as RULES!")))

;; Update datatype
(define update? pair?)
(define (middle-page update)
  (if (update? update)
      (list-ref update (quotient (length update) 2))
      (error "Incorrect type passed as UPDATE!")))

;; Verifying correctness of update
(define (is-correct-order? update rules)
  (cond ((null? (cdr update))
         #t)
        ((every identity (map (lambda (el) (valid-order? rules (car update) el)) (cdr update)))
         (is-correct-order? (cdr update) rules))
        (else
         #f)))

;; Building a correct update
;; This swaps pairs of incorrect update pages from left to right
;; frankly I kinda didn't expect this to work, I was sure there would be cases
;; where swapping would introduce new errors. But maybe, since the errors
;; come from precedence (that is, a number being after the one they should be before)
;; swapping from left to right made it impossible to introduce new errors?
(define (fix-update update rules)
  (define (swap lst idx1 idx2) ;; This swap relies on there being no repetitions in an update
    (define (swapper l i e1 e2)
      (cond ((null? l)
             '())
            ((= i idx1)
             (cons e2 (swapper (cdr l) (+ i 1) e1 e2)))
            ((= i idx2)
             (cons e1 (swapper (cdr l) (+ i 1) e1 e2)))
            (else
             (cons (car l) (swapper (cdr l) (+ i 1) e1 e2)))))
    (swapper lst 0 (list-ref lst idx1) (list-ref lst idx2)))
  (define (search-for-invalid-pair first rest start)
    (cond ((null? rest)
           #f)
          ((not (valid-order? rules first (car rest)))
           (+ start 1))
          (else
           (search-for-invalid-pair first (cdr rest) (+ start 1)))))
  (let loop ((i 0))
    (if (= i (length update))
        update
        (let ((idx (search-for-invalid-pair (car (list-tail update i))
                                            (cdr (list-tail update i))
                                            i)))
          (if idx
              (begin
                (set! update (swap update i idx))
                (loop i))
              (loop (+ i 1)))))))

;; Parsing input file
(define (string-split str delim)
  (define (reader str len last i)
    (cond ((= i len)
           `(,(substring str last i)))
          ((char=? (string-ref str i) delim)
           (cons (substring str last i)
                 (reader str len (+ i 1) (+ i 1))))
          (else
           (reader str len last (+ i 1)))))
  (reader str (string-length str) 0 0))

(define (parse-rule-string str)
  (map string->number (string-split str #\|)))

(define (parse-update-string str)
  (map string->number (string-split str #\,)))

(define (parse-rules port)
  (let parse-loop ((rules (make-page-order-rules)))
    (let ((line (read-line port)))
      (when (eof-object? line)
        (error "EOF found too early!"))
      (if (string=? line "")
          rules
          (begin
            (add-rule! rules
                       (car (parse-rule-string line))
                       (cadr (parse-rule-string line)))
            (parse-loop rules))))))

(define (parse-updates port rules)
  (define (-parse port rules sum-correct sum-incorrect)
    (let ((line (read-line port)))
      (if (eof-object? line)
          (cons sum-correct sum-incorrect)
          (let ((update (parse-update-string line)))
            (if (is-correct-order? update rules)
                (-parse port rules (+ sum-correct (middle-page update)) sum-incorrect)
                (-parse port rules sum-correct (+ sum-incorrect
                                                  (middle-page (fix-update update rules)))))))))
  (-parse port rules 0 0))

(define (analyze-data filename)
  (let* ((port (open-input-file filename))
         (rules (parse-rules port)))
    (parse-updates port rules)))

(define (main)
  (let* ((input-file "inputs/day5.txt")
         (result (analyze-data input-file)))
    (display (string-append
              "Sum of middle pages of correct update orders: "
              (number->string (car result))))
    (newline)
    (display (string-append
              "Sum of middle pages of fixed update orders: "
              (number->string (cdr result))))
    (newline)))
