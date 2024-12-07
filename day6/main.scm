
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
;; (define (guard-steps guard) (cadddr guard))

;; (define (guard-move-x guard newx)
;;   (list (guard-facing guard)
;;         newx
;;         (guard-y guard)
;;         (+ (guard-steps guard) (abs (- newx (guard-x guard))))))

;; (define (guard-move-y guard newy)
;;   (list (guard-facing guard)
;;         (guard-x guard)
;;         newy
;;         (+ (guard-steps guard) (abs (- newy (guard-y guard))))))

(define (guard-rotate guard)
  (list (remainder (+ (guard-facing guard) 1) 4) ;; Clockwise rotation
        (guard-x guard)
        (guard-y guard)
        ;; (guard-steps guard)
        ))

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
(define (get-start map)
  (let loop ((i 0)
             (len (* (map-width map) (map-height map))))
    (cond ((= i len)
           #f)
          ((char=? (vector-ref (map-data map) i) #\^)
           i)
          (else
           (loop (+ i 1) len)))))

;; Returns a new position after moving the guard towards the direction they're facing
;; If the return is #f, this means the guard has moved out of the map
(define (move-guard! guard map)
  (define (try-move x y facing map)
    (let* ((idx (map-idx map x y))
           (dx (vector-ref *guard-dx* facing))
           (dy (vector-ref *guard-dy* facing)))
      (cond ((not (inside-map? map x y))
             #f)
            ((char=? (map-get-at map x y) #\#)
             (cons (- x dx) (- y dy)))
            (else
             (map-set-at! map x y #\X)
             (try-move (+ x dx) (+ y dy) facing map)))))
  (try-move (guard-x guard) (guard-y guard) (guard-facing guard) map))

(define (count-traversed map)
  (let loop ((i 0)
             (len (* (map-height map) (map-width map)))
             (count 0))
    (let ((pos (map-pos map i)))
      (cond ((= i len)
             count)
            ((char=? (map-get-at map (car pos) (cdr pos)) #\X)
             (loop (+ i 1) len (+ count 1)))
            (else
             (loop (+ i 1) len count))))))

(define (navigate-map! map)
  (define (move-loop guard map)
    (let ((new-pos (move-guard! guard map)))
      (when new-pos
        (move-loop (guard-rotate
                    (make-guard (guard-facing guard)
                                (car new-pos)
                                (cdr new-pos)))
                   map))))
  (let* ((start-idx (get-start map))
         (x (car (map-pos map start-idx)))
         (y (cdr (map-pos map start-idx)))
         (g (make-guard 3 x y)))
    (move-loop g map)
    (count-traversed map)))
