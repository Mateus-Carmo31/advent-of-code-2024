;; USE GAMBIT SCHEME

;; (import (srfi 1))

;; ========== Helper vector funcs

(define point? pair?)
(define pX car)
(define pY cdr)
(define (point+ p1 p2)
  (cons (+ (pX p1) (pX p2))
        (+ (pY p1) (pY p2))))
(define (point- p1 p2)
  (cons (- (pX p1) (pX p2))
        (- (pY p1) (pY p2))))
(define (scale-point p1 scalar)
  (cons (* (pX p1) scalar)
        (* (pY p1) scalar)))
(define (inside? p bounds)
  (not (or
        (< (pX p) 0)
        (< (pY p) 0)
        (> (pX p) (pX bounds))
        (> (pY p) (pY bounds)))))

;; ========== Antenna Nodes data structure ==========

;; The points of each antenna are stored as a list.
;; (point1, point2, point3...)
;; The antenna table is a table indexed by the character

(define make-antenna-table make-table)

(define (antenna-table-add! table char node)
  (let ((entry (table-ref table char (make-antenna-node-list))))
    (table-set! table char (insert-node entry node))))

(define (make-antenna-node-list)
  '())

(define (node-list-char node-list)
  (car node-list))

(define (node-list-nodes node-list)
  (cdr node-list))

(define (insert-node node-list node)
  (append node-list `(,node)))

(define (antinodes node1 node2)
  (let ((diff (point- node2 node1)))
    (list
     (point- node1 diff)
     (point+ node2 diff))))

;; Calculate antinodes (for part 1)
;; The second arg is not used and is just there for find-antinodes to not need additional conditionas
;; when differentiating part 1 from part 2 (I can just pass the bounds to it)
(define (calculate-antinodes nodes _)
  (cond ((null? nodes)
         '())
        (else
         (append (fold append '() (map (lambda (node) (antinodes (car nodes) node)) (cdr nodes)))
                 (calculate-antinodes (cdr nodes) _)))))

(define (generate-all start diff bounds acc)
  (cond ((not (inside? start bounds))
         acc)
        (else
         (let ((newp (point+ start diff)))
           (generate-all newp diff bounds (cons start acc))))))

;; Calculate antinodes while considering the full line of the antenna pairs (for part 2)
(define (calculate-full-antinodes nodes bounds)
  (cond ((null? nodes)
         '())
        (else
         (append (fold append '() (map (lambda (node)
                                         (let ((diff (point- node (car nodes))))
                                           (generate-all (car nodes) diff bounds '())))
                                       (cdr nodes)))
                 (fold append '() (map (lambda (node)
                                         (let ((diff (point- (car nodes) node)))
                                           (generate-all (car nodes) diff bounds '())))
                                       (cdr nodes)))
                 (calculate-full-antinodes (cdr nodes) bounds)))))

;; ========== File Parsing ==========

;; Parse map file and extract antenna locations (+ map bounds)
(define (parse-file filename)
  (let ((port (open-input-file filename))
        (height 0)
        (width  0)
        (antenna-table (make-antenna-table)))
    (let file-loop ((line (read-line port))
                    (y 0))
      (if (eof-object? line)
          antenna-table
          (begin
            (string-for-each
             (let ((x 0)) (lambda (el)
                            (when (not (char=? (string-ref line x) #\.))
                              (antenna-table-add! antenna-table (string-ref line x) (cons x y)))
                            (set! width (max x width)) ;; setting this before x to avoid overcounting the width
                            (set! x (+ x 1))))
             line)
            (set! height (max y height))
            (file-loop (read-line port) (+ y 1)))))
    (close-input-port port)
    (cons
     (cons width height)
     antenna-table)))

;; Locate all antinodes. Uses a table to avoid counting twice (I should try setting up SRFI 113 later)
(define (find-antinodes antenna-table bounds #!optional use-full)
  (let ((antinodes (make-table))
        (calc (if use-full calculate-full-antinodes calculate-antinodes)))
    (table-for-each (lambda (char nodes) (for-each (lambda (x) (table-set! antinodes x #t))
                                                   (calc nodes bounds)))
                    antenna-table)
    (filter (lambda (p) (inside? p bounds))
            (map car (table->list antinodes)))))

(define (main)
  (let* ((input-file "inputs/day8.txt")
         (data (parse-file input-file))
         (bounds (car data))
         (antenna-table (cdr data))
         (antinodes (find-antinodes antenna-table bounds)))
    (display (string-append
              "Unique antinode locations: "
              (number->string (length antinodes))))
    (newline)
    (set! antinodes (find-antinodes antenna-table bounds #t))
    (display (string-append
              "Unique antinode locations (considering full lines): "
              (number->string (length antinodes))))
    (newline)))
