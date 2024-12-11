;; USE GAMBIT SCHEME

;; Scans a compressed string and returns a list containing the
;; (reversed) file list and the empty slot list
(define (scan-disk-map dm)
  (let read-loop ((i 0)
                  (len (string-length dm))
                  (id 0)
                  (is-file #t)
                  (file-list '()))
    (cond ((= i len)
           file-list)
          (is-file
           (read-loop (+ i 1) len (+ id 1) #f
                      (if (char=? (string-ref dm i) #\0)
                          file-list
                          (cons (make-list (digit-value (string-ref dm i)) id) file-list))))
          (else
           (read-loop (+ i 1) len id #t
                      (if (char=? (string-ref dm i) #\0)
                          file-list
                          (cons (list 'empty (digit-value (string-ref dm i))) file-list)))))))

(define (empty-space? pair)
  (and pair (eq? (car pair) 'empty)))

(define (empty-space-count pair)
  (cadr pair))

;; Checks if there are any interspersed empty spaces left
;; Navigates through the list, if an empty space is found after
;; a normal file, then there is space to be used yet
(define (any-empty-space-left? file-list)
  (let loop ((lst file-list)
             (found-block #f))
    (cond ((null? lst)
           #f)
          ((not (empty-space? (car lst)))
           (loop (cdr lst) #t))
          (found-block
           #t)
          (else
           (loop (cdr lst) found-block)))))

;; Adds a number to an empty space
;; Empty space is (empty (counter) (blocks))
;; Decrements the counter on insertion, and removes the "empty" once counter becomes 0
;; (collapses the empty space)
(define (add-to-empty-space! space num)
  (let ((count (cadr space)))
    (append! space `(,num))
    (if (= count 1)
        (begin
          (set-car! space (caddr space))
          (set-cdr! space (cdddr space)))
        (set-car! (cdr space) (- count 1)))))

;; Part 1
;; Insert individual block into file list backwards (to find first empty space)
(define (insert-into-first-empty-space! file-list num)
  (cond ((null? file-list)
         #f)
        (else
         (let ((inserted (insert-into-first-empty-space! (cdr file-list) num)))
           (if inserted
               #t
               (if (and (empty-space? (car file-list))
                        (not (= (empty-space-count (car file-list)) 0)))
                   (begin
                     (add-to-empty-space! (car file-list) num)
                     #t)
                   #f))))))

;; Part 2
;; Insert whole file into file list backwards (to find first empty space that can fit it)
(define (insert-whole-file! file-list file)
  (if (null? file-list)
      #f
      (let ((inserted (insert-whole-file! (cdr file-list) file)))
        (if inserted
            #t
            (if (and (empty-space? (car file-list))
                     (>= (empty-space-count (car file-list)) (length file)))
                (begin
                  (map (lambda (block) (add-to-empty-space! (car file-list) block))
                       file)
                  #t)
                #f)))))

;; Part 1
;; Pops a block from the last file (rightmost file)
(define (pop-file-block! file-list)
  (cond ((null? file-list)
         #f)
        ((empty-space? (car file-list))
         (pop-file-block! (cdr file-list)))
        ((= (length (car file-list)) 1)
         (let ((id (caar file-list)))
           (set-car! file-list (cadr file-list))
           (set-cdr! file-list (cddr file-list))
           id))
        (else
         (let ((id (caar file-list)))
           (set-car! (car file-list) (cadar file-list))
           (set-cdr! (car file-list) (cddar file-list))
           id))))

;; Part 2
;; Pops the last file from the list
(define (pop-whole-file! file-list file)
  (cond ((null? file-list)
         #f)
        ((empty-space? (car file-list))
         (pop-whole-file! (cdr file-list)))
        (else
         (let ((file (car file-list)))
           (set-car! file-list (cadr file-list))
           (set-cdr! file-list (cddr file-list))
           file))))

;; Part 1
;; Reorganizes all blocks to fit into empty spaces
;; This function modifies the list in place (no need to call set! after)
(define (reshuffle-blocks! file-list)
  (cond ((any-empty-space-left? file-list)
         (let* ((top (pop-file-block! file-list))
                (can-continue (insert-into-first-empty-space! file-list top)))
           (if can-continue
               (reshuffle-blocks! file-list)
               file-list)))
        (else
         file-list)))

;; Part 2
;; Reorganizes all files to fit into empty spaces
;; This function modifies the list and reorganizes it in a way that requires the new version to be returned
;; (must use `set!' after!)
(define (reshuffle-whole-files! file-list)
  (cond ((null? file-list)
         '())
        ((empty-space? (car file-list))
         (cons (car file-list) (reshuffle-whole-files! (cdr file-list))))
        (else
         (let ((inserted (insert-whole-file! file-list (car file-list))))
           (if inserted
               (cons (make-list (length (car file-list))) (reshuffle-whole-files! (cdr file-list)))
               (cons (car file-list) (reshuffle-whole-files! (cdr file-list))))))))

;; Part 1
;; Removes all empty spaces, and collapses partially filled empty spaces into normal files
;; Basically turns '(empty 4) into '() and '(empty 2 1 4) into '(1 4)
(define (collapse-empty! file-list)
  (cond ((null? file-list)
         'done)
        ((and (empty-space? (car file-list)) (not (null? (cddar file-list))))
         (set-car! (car file-list) (caddar file-list))
         (set-cdr! (car file-list) (cdddar file-list))
         (collapse-empty! file-list))
        ((empty-space? (car file-list))
         (set-car! file-list (cadr file-list))
         (set-cdr! file-list (cddr file-list))
         (collapse-empty! file-list))
        (else
         (collapse-empty! (cdr file-list)))))

;; Calculates checksum linearly
;; List mustn't have empty spaces!
(define (checksum file-list)
  (set! file-list (reverse! file-list))
  (let loop ((lst file-list)
             (i 0)
             (sum 0))
    (cond ((null? lst)
           sum)
          (else
           (for-each (lambda (id)
                       (set! sum (+ sum (* i id)))
                       (set! i (+ i 1)))
                     (car lst))
           (loop (cdr lst) i sum)))))

;; Fill out empty spaces with zeroes
;; So basically turns '(empty 2) -> '(0 0) or '(empty 2 1 5) -> '(1 5 0 0)
;; This function only exists to allow for the same `checksum' function to be used in Part 2,
;; as in part 1, the blocks are packed together and i can increase properly, but here,
;; empty spaces can remain naturally and should be counted with i.
;; luckily, (* i 0) doesn't affect the checksum, so we just fill it all out with 0s.
(define (fill-out-empty! file-list)
  (cond ((null? file-list)
         'done)
        ((empty-space? (car file-list))
         (let* ((count (empty-space-count (car file-list)))
                (blocks (cddar file-list))
                (blocks+zeroes (append blocks (make-list count 0))))
           (set-car! (car file-list) (car blocks+zeroes))
           (set-cdr! (car file-list) (cdr blocks+zeroes))
           (fill-out-empty! (cdr file-list))))
        (else
         (fill-out-empty! (cdr file-list)))))


(define (read-file filename)
  (let* ((port (open-input-file filename))
         (file-list (scan-disk-map (read-line port))))
    (close-input-port port)
    file-list))

(define (main)
  (let* ((input-file "inputs/day9.txt")
         (file-list (read-file input-file))
         (final-sum (and (reshuffle-blocks! file-list)
                         (collapse-empty! file-list)
                         (checksum file-list))))
    (display (string-append "Checksum (fragmented): "
                            (number->string final-sum)))
    (newline)
    ;; gotta reset the file list
    (set! file-list (read-file input-file))
    (set! file-list (reshuffle-whole-files! file-list))
    (fill-out-empty! file-list)
    (set! final-sum (checksum file-list))
    (display (string-append "Checksum (whole blocks): "
                            (number->string final-sum)))
    (newline)))
