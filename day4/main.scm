;; USE GAMBIT SCHEME
;; Ugh, I really dislike my code for this one

(define xmas (string->vector "XMAS"))
(define samx (string->vector "SAMX"))

(define (read-puzzle filename)
  (define (scan port len)
    (let ((line (read-line port)))
      (if (eof-object? line)
          (begin (close-input-port port) `(,len))
          (cons (string->vector line) (scan port (+ len 1))))))
  (let* ((res (scan (open-input-file filename) 0))
         (len (last res))
         (puzzle (take res (- (length res) 1))))
    (cons len puzzle)))

;; Functions for working with puzzles
(define puzzle-line-count car)
(define puzzle-data cdr)
(define (puzzle-line-length puzzle) (vector-length (car (puzzle-data puzzle))))

(define (puzzle-get-line pdata idx)
  (with-exception-handler ;; exception handlers to avoid having to include bounds checking for the lines
      (lambda (_) '())
    (lambda ()
      (list-ref pdata idx))))

(define (char-at-line line idx)
  (with-exception-handler
      (lambda (_) #f)
   (lambda () 
     (vector-ref line idx))))

(define (puzzle-get-char-at pdata x y)
  (char-at-line (puzzle-get-line pdata y) x))

;; Check if a sequence of chars (vector) is XMAS or SAMX
(define (is-xmas? seq)
  (or (equal? seq xmas)
      (equal? seq samx)))

;; Check if a sequence of 9 chars (vector) is a 3x3 cross of MAS (or SAM)
;; This is a very hard-coded test and I hate it
(define (is-mas? seq)
  (let ((middle (vector-ref seq 4))
        (tl (vector-ref seq 0))
        (tr (vector-ref seq 2))
        (bl (vector-ref seq 6))
        (br (vector-ref seq 8)))
  (and
   (and middle tl tr bl br) ;; Checks that none of the points are #f (out of bounds)
   (char=? #\A middle) ;; Check middle first
   (or ;; Main Diagonal
    (and (char=? tl #\M) (char=? br #\S))
    (and (char=? tl #\S) (char=? br #\M)))
   (or ;; Left Diagonal
    (and (char=? tr #\M) (char=? bl #\S))
    (and (char=? tr #\S) (char=? bl #\M))))))

;; Apply procedure PROC to every cell in the puzzle
(define (for-each-cell puzzle line-length line-count proc)
  (let y-loop ((y 0))
    (let x-loop ((x 0))
      (cond ((procedure? proc)
             (proc puzzle x y))
            ((symbol? proc)
             (apply (eval proc) (list puzzle x y)))
            (else
             (error "Incorrect type passed as procedure!")))
      (unless (= x (- line-length 1))
        (x-loop (+ x 1))))
    (unless (= y (- line-count 1))
      (y-loop (+ y 1)))))

;; ==================== Solutions ====================
(define (search-xmas puzzle)
  (define (horizontal-scan data line-length line-count)
    (let ((count 0))
      (for-each-cell data line-length line-count
                     (lambda (data x y) ;; Extracts a line of 4 chars after (x,y)
                       (let* ((line (puzzle-get-line data y))
                              (seq (vector-map
                                    (lambda (i) (char-at-line line i))
                                    (list->vector (iota 4 x)))))
                         (when (is-xmas? seq)
                           (set! count (+ count 1))))))
      count))
  (define (vertical-scan data line-length line-count)
    (let ((count 0))
      (for-each-cell data line-length line-count
                     (lambda (data x y) ;; Extracts a vertical line of 4 chars below (x,y)
                       (let* ((seq (vector-map
                                    (lambda (x y) (puzzle-get-char-at data x y))
                                    (make-vector 4 x)
                                    (list->vector (iota 4 y)))))
                         (when (is-xmas? seq)
                           (set! count (+ count 1))))))
      count))
  (define (diagonal-right-scan data line-length line-count)
    (let ((count 0))
      (for-each-cell data line-length line-count
                     (lambda (data x y) ;; Extracts the right diagonal starting from (x,y)
                       (let* ((seq (vector-map
                                    (lambda (x y) (puzzle-get-char-at data x y))
                                    (list->vector (iota 4 x))
                                    (list->vector (iota 4 y)))))
                         (when (is-xmas? seq)
                           (set! count (+ count 1))))))
      count))
  (define (diagonal-left-scan  data line-length line-count)
    (let ((count 0))
      (for-each-cell data line-length line-count
                     (lambda (data x y) ;; Extracts the left diagonal starting from (x,y)
                       (let* ((seq (vector-map
                                    (lambda (x y) (puzzle-get-char-at data x y))
                                    (list->vector (reverse (iota 4 (- x 3)))) ;; iota 4 x-> x x+1 x+2 x+3 ==> this mess -> x-3 x-2 x-1 x
                                    (list->vector (iota 4 y)))))
                         (when (is-xmas? seq)
                           (set! count (+ count 1))))))
      count))

  (let ((data (puzzle-data puzzle))
        (line-length (puzzle-line-length puzzle))
        (line-count (puzzle-line-count puzzle)))
    (+
     (horizontal-scan data line-length line-count)
     (vertical-scan data line-length line-count)
     (diagonal-left-scan data line-length line-count)
     (diagonal-right-scan data line-length line-count))))

(define (search-mas puzzle)
  (define (box-from x y box-size) ;; Generates the indices
    (let* ((x-vals (iota box-size x))
           (y-vals (iota box-size y))
           (x-idxs (apply append (map (lambda (_) x-vals) y-vals)))
           (y-idxs (apply append
                          (map (lambda (y-val) (map (lambda (_) y-val) x-vals)) ;; repeats each y for each individual x value
                           y-vals))))
      (cons
       x-idxs
       y-idxs)))
  (define (x-checker data line-length line-count)
    (let ((count 0))
      (for-each-cell data line-length line-count
                     (lambda (data x y)
                       (let* ((points (box-from x y 3))
                              (seq (vector-map
                                    (lambda (x y) (puzzle-get-char-at data x y))
                                    (list->vector (car points))
                                    (list->vector (cdr points)))))
                         (when (is-mas? seq)
                           (set! count (+ count 1))))))
      count))
  (let ((data (puzzle-data puzzle))
        (line-length (puzzle-line-length puzzle))
        (line-count (puzzle-line-count puzzle)))
    (x-checker data line-length line-count)))

(define (main)
  (let* ((input-file "inputs/day4.txt")
         (puzzle (read-puzzle input-file)))
    (display (string-append
              "XMAS in puzzle: "
              (number->string (search-xmas puzzle))))
    (newline)
    (display (string-append
              "X'd MAS in puzzle: "
              (number->string (search-mas puzzle))))
    (newline)))
