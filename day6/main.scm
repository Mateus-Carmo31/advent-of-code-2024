
;; ========== Map data structure ==========
;; Constructor
(define (make-map width height) (cons (cons width height) (make-vector (* width height) #\.)))

;; Accessors
(define (map-data m) (cdr m))
(define (map-width m) (caar m))
(define (map-height m) (cdr (car m)))
(define (map-idx m x y) (+ x (* y (map-width m))))
(define (map-pos m idx) (cons (remainder idx (map-width m))
                              (quotient idx (map-width m))))
(define (map-get-at m x y) (vector-ref (map-data m) (map-idx m x y)))

;; Setters
(define (map-set-at! m x y c) (vector-set! (map-data m) (map-idx m x y) c))
(define (map-set-line! m line data) (vector-copy! (map-data m) (* line (map-width m)) data))

;; Predicates
(define (inside-map? m x y)
  (not (or
        (< x 0)
        (< y 0)
        (>= x (map-width m))
        (>= y (map-height m)))))

;; ========== Guard data structure ==========
(define *guard-dx* #(1 0 -1 0))
(define *guard-dy* #(0 1 0 -1))
(define (make-guard facing x y) (list facing x y))

(define (guard-facing guard) (car guard))
(define (guard-x guard) (cadr guard))
(define (guard-y guard) (caddr guard))

(define (guard-rotate guard)
  (list (remainder (+ (guard-facing guard) 1) 4) ;; Clockwise rotation
        (guard-x guard)
        (guard-y guard)))

;; ========== File parsing ==========
(define (count-lines filename)
  (let loop ((port (open-input-file filename))
             (count 0))
    (if (eof-object? (read-line port))
        (begin (close-input-port port) count)
        (loop port (+ count 1)))))

(define (read-map filename)
  (define (read-loop port)
    (let ((line (read-line port)))
      (if (eof-object? line)
          '()
          (cons (string->vector line)
                (read-loop port)))))
  (let* ((port (open-input-file filename))
         (lines (read-loop port))
         (width (vector-length (car lines)))
         (height (length lines))
         (m (apply vector-append lines)))
    (close-input-port port)
    (cons (cons width height) m)))

;; ========== Solvers ==========

;; Finds the index of the caret (guard starting position) in map
(define (get-start m)
  (let loop ((i 0)
             (len (vector-length (map-data m))))
    (cond ((= i len)
           #f)
          ((char=? (vector-ref (map-data m) i) #\^)
           i)
          (else
           (loop (+ i 1) len)))))

;; Constructs the path of the guard from the start until they're off the map
;; Uses a hash table to avoid double-counting the same tiles
(define (get-guard-path start map)
  (define (build-path x y facing map counter)
    (let* ((idx (map-idx map x y))
           (dx (vector-ref *guard-dx* facing))
           (dy (vector-ref *guard-dy* facing)))
      (cond ((not (inside-map? map x y))
             '())
            ((char=? (map-get-at map x y) #\#)
             (build-path (- x dx) (- y dy) (remainder (+ facing 1) 4) map counter))
            (else
             (if (table-ref counter idx)
                 (build-path (+ x dx) (+ y dy) facing map counter)
                 (begin
                   (table-set! counter idx #t)
                   (cons idx
                         (build-path (+ x dx) (+ y dy) facing map counter))))))))
  (let ((p (map-pos map start)))
    (build-path (car p) (cdr p) 3 map (make-table init: #f))))

;; Tests if a given obstruction at some index will cause a loop for the guard
;; Uses a hash table to store the last facing the guard was in some tile
;; (same spot with same facing = has been here before = is looping)
(define (test-for-loop start map obstruction-idx)
  (define (traversal x y facing map path-taken)
    (let* ((idx (map-idx map x y))
           (last-facing (table-ref path-taken idx))
           (dx (vector-ref *guard-dx* facing))
           (dy (vector-ref *guard-dy* facing)))
      (cond ((not (inside-map? map x y))
             #f)
            ((and last-facing (= facing last-facing))
             #t)
            ((or (= idx obstruction-idx) 
                 (char=? (map-get-at map x y) #\#))
             (traversal (- x dx) (- y dy) (remainder (+ facing 1) 4) map path-taken))
            (else
             (when (not last-facing)
               (table-set! path-taken idx facing))
             (traversal (+ x dx) (+ y dy) facing map path-taken)))))
  (let ((p (map-pos map start)))
    (traversal (+ (car p) (vector-ref *guard-dx* 3))
               (+ (cdr p) (vector-ref *guard-dy* 3))
               3 map (make-table init: #f))))

;; Counts the number of possible loop-inducing obstructions
;; Obstructions that will affect the guard's path will only do so if they are
;; in the guard's path themselves, so we only need to test placing obstructions
;; in the tiles along said path (with the procedure from part 1)
;; NOTE: this is slooooooooooooooooow, yucky
(define (count-looping-obstructions _map path)
  (let ((start (get-start _map))
        (count 0))
    (for-each
     (lambda (o)
       (when (test-for-loop start _map o)
         (set! count (+ count 1))))
     (cdr path))
    count))

(define (main)
  (let* ((input-file "inputs/day6.txt")
         (lab-map (read-map input-file))
         (guard-path (get-guard-path (get-start lab-map) lab-map))
         (path-tile-count (length guard-path))
         (obs-count (count-looping-obstructions lab-map guard-path)))
    (display (string-append
              "Length of guard path: "
              (number->string path-tile-count)))
    (newline)
    (display (string-append
              "Number of possible loop-inducing obstructions: "
              (number->string obs-count)))
    (newline)))
