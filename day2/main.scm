;; THIS IS MEANT TO BE RUN WITH GAMBIT SCHEME!!!!


(define (count-lines filename)
  (let loop ((port (open-input-file filename))
             (count 0))
    (if (eof-object? (read-line port))
        (begin (close-input-port port) count)
        (loop port (+ count 1)))))

;; Parses a report from the input file
(define (parse-report str)
  (define (-parse str start end)
    (cond
     ((= start end) '())
     ((char-numeric? (string-ref str start))
      (let ((next-ws (or (string-contains str " " start) end)))
        (cons (string->number (substring str start next-ws))
              (-parse str next-ws end))))
     (else
      (-parse str (+ start 1) end))))
  (-parse str 0 (string-length str)))

(define (between? val a b)
  (and (>= val a) (<= val b)))

(define (is-safe? report)
  (define (check report inc?)
    (cond ((null? (cdr report)) #t)
          ((not (between? (abs (- (car report) (cadr report))) 1 3)) #f)
          ((and inc? (<= (car report) (cadr report)))
           (check (cdr report) inc?))
          ((and (not inc?) (>= (car report) (cadr report)))
           (check (cdr report) inc?))
          (else
           #f)))
  (check report (< (car report) (cadr report))))

;; Maps a procedure proc over lists, passing in the index of each element as
;; the first argument of procedure.
(define (map-with-index proc . lists)
  (define (mapper proc idx . lists)
    (if (null? (car lists))
        '()
        (cons
         (apply proc idx (map car lists))
         (apply mapper proc (+ idx 1) (map cdr lists)))))
  (apply mapper proc 0 lists))

(define (is-dampened-safe? report)
  (define (remove-by-idx lst i) ;; Creates a list without element at index i
    (if (null? lst) '()
        (if (= i 0)
            (cdr lst)
            (cons (car lst) (remove-by-idx (cdr lst) (- i 1))))))
  ;; (let ((dampened-lists (map-with-index
  ;;                        (lambda (idx _) (remove-by-idx report idx))))
  ;;       ())))
  ;; I had this idea for a super functional solution but it would involve generating ALL possible
  ;; versions of each report without each element, which is inneficient.
  ;; Could work with streams/delayed evalution? I'm not sure how it works with Gambit though.
  (if (is-safe? report)
      #t
      (let loop ((idx 0) (sz (length report)))
        (cond ((= idx sz) #f)
              ((is-safe? (remove-by-idx report idx))
               #t)
              (else
               (loop (+ idx 1) sz))))))
      

(define (main)
  (let* ((input-file "inputs/day2.txt")
         (port (open-input-file input-file))
         (safe-count 0)
         (dampened-safe-count 0)
         (total 0))
    (let loop ((line (read-line port)))
      (unless (eof-object? line)
        (when (is-safe? (parse-report line))
          (set! safe-count (+ safe-count 1)))
        (when (is-dampened-safe? (parse-report line))
          (set! dampened-safe-count (+ dampened-safe-count 1)))
        (set! total (+ total 1))
        (loop (read-line port))))

    (display (string-append
              "Total reports read: "
              (number->string total)))
    (newline)
    (display (string-append
              "Safe reports found: "
              (number->string safe-count)))
    (newline)
    (display (string-append
              "Safe reports found after remembering the Problem Dampener: "
              (number->string dampened-safe-count)))
    (newline)))
