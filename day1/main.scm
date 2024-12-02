;; THIS IS MEANT TO BE RUN WITH GAMBIT SCHEME!!!!

;; Count how many lines a file has
(define (count-lines filename)
  (let loop ((port (open-input-file filename))
             (count 0))
    (if (eof-object? (read-line port))
        (begin (close-input-port port) count)
        (loop port (+ count 1)))))

;; Parses a string in the format "(number) (number)"
(define (parse-ints str)
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

;; insertion sort in u64-vector
(define (insert-ordered! vec count num)
  (define (shift-right vec i count)
    (when (> count i)
      (u64vector-set! vec count (u64vector-ref vec (- count 1)))
      (shift-right vec i (- count 1))))
  (define (ins vec i count num)
    (cond ((or (= i count) (> (u64vector-ref vec i) num))
           (shift-right vec i count)
           (u64vector-set! vec i num))
          ((<= (u64vector-ref vec i) num) (ins vec (+ i 1) count num))))
  (when (< count (u64vector-length vec))
    (ins vec 0 count num)))

;; Parse input file, and sort the values read in-place with insertion sort
(define (parse-and-sort filename)
  (let* ((file-port (open-input-file filename))
         (input-number-count (count-lines filename))
         (counts (cons 0 0))
         (numvectors (cons (make-u64vector input-number-count) (make-u64vector input-number-count))))
    (define (scan port)
      (let ((input (read-line port))
            (newnums '()))
        (unless (eof-object? input)
          (set! newnums (parse-ints input))
          (insert-ordered! (car numvectors) (car counts) (car newnums))
          (insert-ordered! (cdr numvectors) (cdr counts) (cadr newnums))
          (set-car! counts (+ (car counts) 1))
          (set-cdr! counts (+ (cdr counts) 1))
          (scan port))))
    (scan file-port)
    (close-input-port file-port)
    numvectors))

;; Part 1
(define (calculate-total-distance slist1 slist2)
  ;; map the distance subtraction over the two sorted lists, and then fold the values with sum
  (fold (lambda (x y) (+ x y)) 0
        (map (lambda (x y) (abs (- x y))) slist1 slist2)))

;; Count occurrences of num in vec
(define (find-occurrences vec num)
  (define (find-first vec num)
    (let loop ((start 0)
               (end (- (u64vector-length vec) 1))
               (idx #f))
      (if (< end start)
          idx
          (let ((pivot (quotient (+ end start) 2)))
            (cond ((> (u64vector-ref vec pivot) num) ;; middle is higher than num, search left
                   (loop start (- pivot 1) idx))
                  ((< (u64vector-ref vec pivot) num) ;; middle is lower than num, search right
                   (loop (+ pivot 1) end idx))
                  (else ;; num is found, but continue search to the left to find the FIRST appearance of num
                   (loop start (- pivot 1) pivot)))))))
  (define (find-last vec num)
    (let loop ((start 0)
               (end (- (u64vector-length vec) 1))
               (idx #f))
      (if (< end start)
          idx
          (let ((pivot (quotient (+ end start) 2)))
            (cond ((> (u64vector-ref vec pivot) num) ;; middle is higher than num, search left
                   (loop start (- pivot 1) idx))
                  ((< (u64vector-ref vec pivot) num) ;; middle is lower than num, search right
                   (loop (+ pivot 1) end idx))
                  (else ;; num is found, but continue search to the right to find the LAST appearance of num
                   (loop (+ pivot 1) end pivot)))))))
  ;; Difference of indexes gives us the number (because vec is sorted)
  (let ((first (find-first vec num))
        (last (find-last vec num)))
    (if (and first last)
        (- last first -1)
        0)))

;; Part 2
(define (calculate-similarity-score slist1 slist2)
  (let loop ((memoize (make-table size: (u64vector-length slist1) init: #f)) ;; Table for memoization
             (i 0)
             (total 0))
    (cond ((= i (u64vector-length slist1)) ;; Finished list
           total)
          ((table-ref memoize (u64vector-ref slist1 i)) ;; Memoization of numbers in list1 that we've been through
           (loop memoize (+ i 1) (+ total (table-ref memoize (u64vector-ref slist1 i)))))
          (else ;; Calculate the score for a new number, and memoize it
           (let* ((occurrences (find-occurrences slist2 (u64vector-ref slist1 i)))
                  (score (* (u64vector-ref slist1 i) occurrences)))
             (table-set! memoize (u64vector-ref slist1 i) score)
             (loop memoize (+ i 1) (+ total score)))))))

(define (main)
  (let* ((input-file "inputs/day1.txt")
         (numlists (parse-and-sort input-file)))
    (display "Total distance: ")
    (display (calculate-total-distance (u64vector->list (car numlists)) (u64vector->list (cdr numlists))))
    (newline)
    (display "Total similarity score: ")
    (display (calculate-similarity-score (car numlists) (cdr numlists)))
    (newline)))
