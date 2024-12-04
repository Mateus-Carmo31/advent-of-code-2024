;; USE GAMBIT SCHEME

;; auxiliary
(define do-count 0)
(define dont-count 0)

(define *parse-dos* #f)

;; Finite state machine that parses "mul(number1,number2)"
;; It only cares about the current character. The point is only used to save the
;; positions of the numbers
;; VERY FINICKY! I wish I remembered how to do parse tables...
(define (fsm)
  (let ((state start:)
        (enabled #t)
        (number1 '(-1 . -1))
        (number2 '(-1 . -1)))
    (lambda (point char)
      (cond 
       ;; do detection states
       ((and *parse-dos* (eq? state start:) (char=? char #\d))
        (set! state d:)
        d:)
       ((and *parse-dos* (not (eq? state start:)) (char=? char #\d)) ;; reset by prefix
        (set! state d:)
        (set! number1 '(-1 . -1))
        (set! number2 '(-1 . -1))
        d:)
       ((and *parse-dos* (eq? state d:) (char=? char #\o))
        (set! state o:)
        o:)
       ((and *parse-dos* (eq? state o:) (char=? #\( char))
        (set! state do-paren-open:)
        do-paren-open:)
       ((and *parse-dos* (eq? state do-paren-open:) (char=? #\) char))
        (set! state start:)
        (set! enabled #t)
        (set! do-count (+ do-count 1))
        start:)
       ;; don't detection states
       ((and *parse-dos* (eq? state o:) (char=? char #\n))
        (set! state n:)
        n:)
       ((and *parse-dos* (eq? state n:) (char=? char #\'))
        (set! state apost:)
        apost:)
       ((and *parse-dos* (eq? state apost:) (char=? char #\t))
        (set! state t:)
        t:)
       ((and *parse-dos* (eq? state t:) (char=? #\( char))
        (set! state dont-paren-open:)
        dont-paren-open:)
       ((and *parse-dos* (eq? state dont-paren-open:) (char=? #\) char))
        (set! state start:)
        (set! enabled #f)
        (set! dont-count (+ dont-count 1))
        start:)
       ;; mul detection states
       ((and enabled (eq? state start:) (char=? char #\m))
        (set! state m:)
        m:)
       ((and (not (eq? state start:)) (char=? #\m char)) ;; reset by prefix
        (set! state m:)
        (set! number1 '(-1 . -1))
        (set! number2 '(-1 . -1))
        m:)
       ((and (eq? state m:) (char=? char #\u))
        (set! state u:)
        u:)
       ((and (eq? state u:) (char=? char #\l))
        (set! state l:)
        l:)
       ((and (eq? state l:) (char=? #\( char))
        (set! state paren-open:)
        paren-open:)
       ((and (eq? state paren-open:) (char-numeric? char))
        (set! state num1:)
        (set-car! number1 point)
        num1:)
       ((and (eq? state num1:) (char-numeric? char))
        num1:)
       ((and (eq? state num1:) (char=? #\, char))
        (set! state comma:)
        (set-cdr! number1 point)
        comma:)
       ((and (eq? state comma:) (char-numeric? char))
        (set! state num2:)
        (set-car! number2 point)
        num2:)
       ((and (eq? state num2:) (char-numeric? char))
        num2:)
       ((and (eq? state num2:) (char=? #\) char))
        (set! state start:)
        (set-cdr! number2 point)
        (list number1 number2))
       (else
        (set! state start:)
        (set! number1 '(-1 . -1))
        (set! number2 '(-1 . -1))
        start:)))))

;; Parses a string with "mul(number1,number2)" interspersed in between
;; and extracts the operands to be multiplied in a list of list pairs
(define (parse-string str reader)
  (define (extract-number str bounds)
    (string->number (substring str (car bounds) (cdr bounds))))
  (let loop ((point 0)
             (len (string-length str)))
    (if (= point len)
        '()
        (begin
          (let ((res (reader point (string-ref str point))))
            (cond ((pair? res)
                   (cons (list (extract-number str (car res))
                               (extract-number str (cadr res)))
                         (loop (+ point 1) len)))
                  ;; ((eq? res)
                  ;;  (loop reader (+ point 1) len))
                  ;; ((eq? res)
                  ;;  (loop reader (+ point 1) len))
                  (else
                   (loop (+ point 1) len))))))))

;; Multiply the elements of a list of the form ((number number) (number number) ...)
;; and adds them all together
(define (multiply-and-add ls)
  (fold (lambda (a b) (+ a b)) 0
        (map (lambda (p) (* (car p) (cadr p))) ls)))

(define (parse-file filename)
  (let loop ((port (open-input-file filename))
             (reader (fsm)) ;; THE READER IS USED FOR ALL LINES, TO PERSIST THE EFFECT OF THE don't()
             (total 0))
    (let ((line (read-line port)))
      (if (eof-object? line)
          (begin (close-input-port port) total)
          (loop port
                reader
                (+ total (multiply-and-add (parse-string line reader))))))))

(define (main)
  (let ((input-file "inputs/day3.txt"))
    (display (string-append
              "Total (without do's): "
              (number->string (parse-file input-file))))
    (newline)

    (set! *parse-dos* #t)
    (set! do-count 0)
    (set! dont-count 0)

    (display (string-append
              "Total (with do's): "
              (number->string (parse-file input-file))))
    (newline)))
