;; USE GAMBIT SCHEME
;; Also try to compile wit gsc. This code is even slower when interpreted

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

;; Parses a line of the form "(number): (number) (number)..."
(define (parse-line line)
  (if (string? line)
      (let* ((tokens (string-split line #\ ))
             (result (car tokens))
             (add-list (cdr tokens)))
        (cons (string->number (substring result 0 (- (string-length result) 1)))
              (map string->number add-list)))
      #f))

;; Concat operation (part 2)
(define (|| a b)
  (string->number (string-append (number->string a)
                                 (number->string b))))

(define *op-list* (vector-map eval #(+ *)))

(define (next-op idx)
  (remainder (+ idx 1) (vector-length *op-list*)))

(define (get-op idx)
  (vector-ref *op-list* idx))

;; Calculating the result of a calculation entry
;; Gambit Scheme doesn't allow nested defines in compilation mode, so this is here?
(define (calc-loop operands ops i len base)
  (if (= i len)
      base
      (calc-loop operands ops (+ i 1) len
                 ((get-op (vector-ref ops (- i 1))) base (vector-ref operands i)))))

(define (calculate operands ops)
   (calc-loop operands ops 1 (vector-length operands) (vector-ref operands 0)))

;; Computing new operator lists.
;; update-op-list! will take a vector of operation indices (that index into *op-list*)
;; and will create a new one with a single operator changed.
;; For example -> given vec = #(0 0 0), (update-op-list! vec) will make it so vec = #(1 0 0)
;; This works like binary addition with carry
;; (0 0 0) -> (1 0 0) -> (0 1 0) -> (1 1 0) ...
(define (op-shift! op-list i len change)
  (cond ((= i len)
         change)
        (change
         (let* ((cur (vector-ref op-list i))
                (next (next-op cur)))
           (vector-set! op-list i next)
           (op-shift! op-list (+ i 1) len (< next cur))))
        (else
         (op-shift! op-list (+ i 1) len #f))))

(define (update-op-list! op-list)
  (op-shift! op-list 0 (vector-length op-list) #t))

;; Tests if a list in the form of (result operand1 operand2...) has a valid set of operators
;; that make it so operand1 op1 operand2 op2 operand3 ... = result
(define (valid-operator-combo values)
  (let* ((result (car values))
         (operands (list->vector (cdr values)))
         (op-list (make-vector (- (vector-length operands) 1) 0)))
    (let checker-loop ((cur-list (vector-copy op-list)))
      (let ((cur-res (calculate operands cur-list))
            (done (update-op-list! cur-list)))
        (cond ((= result cur-res)
               cur-list) 
              (done
               #f)
              (else
               (checker-loop cur-list)))))))

;; Reading and parsing the files
(define (reader port acc)
  (let* ((line (read-line port))
         (values (parse-line line)))
    (if (eof-object? line)
        acc
        (reader port
                (if (valid-operator-combo values)
                    (+ acc (car values))
                    acc)))))

(define (parse-file filename)
  (let* ((port (open-input-file filename))
         (total (reader port 0)))
    (close-input-port port)
    total))

;; Main function
(define (main)
  (let* ((input-file "inputs/day7.txt"))
    (reset-timer)
    (set! *op-list* (vector-map eval #(+ *)))
    (display (string-append
              "Total (without ||): "
              (number->string (parse-file input-file))) (current-output-port))
    (newline)
    (set! *op-list* (vector-map eval #(+ * ||)))
    (display (string-append
              "Total (with ||): "
              (number->string (parse-file input-file))) (current-output-port))
    (newline)
    (get-times)))

;; This call is here can be used during compilation.
;; Either uncomment it or compile with gsc -exe -postlude "(main)" main.scm
;; (main)
