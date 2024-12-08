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

(define (parse-line line)
  (if (string? line)
      (let* ((tokens (string-split line #\ ))
             (result (car tokens))
             (add-list (cdr tokens)))
        (cons (string->number (substring result 0 (- (string-length result) 1)))
              (map string->number add-list)))
      #f))

(define (|| a b)
  (string->number (string-append (number->string a)
                                 (number->string b))))

(define *op-list* '(+ *))

(define (next-op idx)
  (remainder (+ idx 1) (length *op-list*)))

(define (get-op idx)
  (eval (list-ref *op-list* idx)))

(define (calculate operands ops)
  (define (calc-loop operands ops base)
    (cond ((null? operands)
           base)
          (else
           (calc-loop (cdr operands) (cdr ops)
                       ((get-op (car ops)) base (car operands))))))
  (calc-loop (cdr operands) ops (car operands)))

;; Given a list of operator indices, generates a new list
;; Basically works like binary addition
;; The last
(define (make-new-op-list op-list)
  (define (op-shift op-list change)
    (cond ((null? op-list)
           '())
          (change
           (let* ((cur (car op-list))
                  (next (next-op cur)))
             (cons next (op-shift (cdr op-list) (< next cur)))))
          (else
           (let ((cur (car op-list)))
             (cons cur (op-shift (cdr op-list) #f))))))
  (op-shift op-list #t))

(define (valid-operator-combo values)
  (let* ((result (car values))
         (operands (cdr values))
         (op-list (make-list (- (length operands) 1) 0)))
    (let checker-loop ((cur-list op-list))
      (let ((cur-res (calculate operands cur-list))
            (next-list (make-new-op-list cur-list)))
        (cond ((= result cur-res)
               cur-list) 
              ((equal? op-list next-list)
               #f)
              (else
               (checker-loop next-list)))))))

(define (parse-file filename)
  (define (reader port acc)
    (let* ((line (read-line port))
           (values (parse-line line)))
      (if (eof-object? line)
          acc
          (reader port
                  (if (valid-operator-combo values)
                      (+ acc (car values))
                      acc)))))
  (let* ((port (open-input-file filename))
         (total (reader port 0)))
    (close-input-port port)
    total))

(define (main)
  (let* ((input-file "inputs/day7.txt"))
    (display (string-append
              "Total (without ||): "
              (number->string (parse-file input-file))))
    (newline)
    (set! *op-list* '(+ * ||))
    (display (string-append
              "Total (with ||): "
              (number->string (parse-file input-file))))
    (newline)))
