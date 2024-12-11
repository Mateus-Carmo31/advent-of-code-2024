;; USE GAMBIT SCHEME

(import (srfi 28))

;; ========== Map data structure ==========
;; Constructor
(define (make-map width height) (cons (cons width height) (make-vector (* width height) #\.)))

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

;; ========== Map exploration ==========

;; Travel directions
(define *dx* '(1 0 -1 0)) ;; right, down, left, up
(define *dy* '(0 1 0 -1))

;; Get all cells surrounding (tx, ty) that are still inside map
(define (get-surrounding m tx ty)
  (filter (lambda (cell) (inside-map? m (car cell) (cdr cell)))
          (map (lambda (dx dy)
                 (cons (+ tx dx)
                       (+ ty dy)))
               *dx* *dy*)))

;; Explores a map according to the trail constraints (only moving to cells with 1 height increment)
;; When the end of a trail is found (val = 9), returns it. The ends are recursively collected.
;; If ignore-visited is false, cells already traversed won't be traversed again. This allows to find
;; only the number of unique ends (part 1). When it is true, the same end will be added more than once,
;; one time for each path from (sx, sy). since paths are unique (due to the increment rule), ignoring
;; already visited cells will actually give us a list with as many ends as there are unique paths (part 2)
(define (_traverse _map sx sy ignore-visited)
  (let loop ((tx sx)
             (ty sy)
             (visited (make-table)))
    (let ((cur-val (digit-value (map-get-at _map tx ty))))
      (unless ignore-visited (table-set! visited (cons tx ty) cur-val))
      (cond ((= cur-val 9)
             (list (cons tx ty)))
            (else
             (let ((found-ends '()))
               (for-each (lambda (cell)
                           (when (and (= (digit-value (map-get-at _map (car cell) (cdr cell)))
                                         (+ cur-val 1))
                                      (or ignore-visited
                                          (not (table-ref visited cell #f))))
                             (set! found-ends (append! found-ends (loop (car cell) (cdr cell) visited)))))
                         (get-surrounding _map tx ty))
               found-ends))))))

;; I was going to use backwards collection recursion so this extra call was necessary.
;; Womp womp.
(define (traverse _map trailhead-x trailhead-y ignore-visited)
  (_traverse _map trailhead-x trailhead-y ignore-visited))

;; Collects the sum of the lengths of each list yielded by `traverse'. Remember:
;; length without ignore-visited => number of unique ends (part 1)
;; length with ignore-visited => number of unique paths (part 2)
(define (trailhead-sum _map ignore-visited)
  (let ((i 0)
        (total-score 0))
    (vector-for-each (lambda (x)
                       (when (char=? x #\0)
                         (let ((pos (map-pos _map i)))
                           (set! total-score (+ total-score
                                                (length (traverse _map (car pos) (cdr pos) ignore-visited))))))
                       (set! i (+ i 1))) (map-data _map))
    total-score))

;; ergo
(define (trailhead-score-sum _map)
  (trailhead-sum _map #f))

(define (trailhead-rating-sum _map)
  (trailhead-sum _map #t))

(define (main)
  (let* ((input-file "inputs/day10.txt")
         (m (read-map input-file))
         (score (trailhead-score-sum m))
         (rating (trailhead-rating-sum m)))
    (display (format "Total map score: ~a\nTotal map rating: ~a" score rating))
    (newline)))
