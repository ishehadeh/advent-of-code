(use-modules (ice-9 rdelim) (srfi srfi-1))


(define (range-overlap r1 r2)
    (let ((x1 (car r1))
          (y1 (cdr r1))
          (x2 (car r2))
          (y2 (cdr r2)))
        (or
            (and (>= x1 x2) (<= y1 y2))
            (and (>= x2 x1) (<= y2 y1)))))

(define (range-overlap-partial r1 r2)
    (let ((x1 (car r1))
        (y1 (cdr r1))
        (x2 (car r2))
        (y2 (cdr r2)))
    (or
        (and (>= x1 x2) (<= x1 y2))
        (and (>= y1 x2) (<= y1 y2))
        (and (>= x2 x1) (<= x2 y1))
        (and (>= y2 x1) (<= y2 y1)))))

(define string-split 
    (case-lambda
        ((s char_pred) (string-split s char_pred 0 '()))
        ((s char_pred i lst)
            (let ((spliti (string-index s char_pred i)))
                (if spliti
                    (string-split s char_pred (+ spliti 1) (append lst (list (substring/read-only s i spliti))))
                    (append lst (list (substring/read-only s i))))))))

(define (decode-range range-str)
    "range strings have the form x-y"
    (let ((numbers (map string->number (string-split range-str #\-))))
        (cons (car numbers) (car (cdr numbers)))))

(define decode-sections
    (case-lambda
        ((port) (decode-sections port '()))
        ((port sections)
            (let ((line (read-line port 'trim)))
                (if (eof-object? line) sections
                    (decode-sections port (append sections (list (map decode-range (string-split line #\,))))))))))

(define (count-section-overlaps sections partial)
    (let ((overlap-fn (if partial range-overlap-partial range-overlap)))
        (fold
            (lambda (ranges n)
                (if (overlap-fn  (car ranges) (cadr ranges))
                    (+ n 1)
                    n))
            0
            sections)))

(define (main-part1)
    (let* ((f        (open-input-file "./data/4.sections.txt"))
           (sections (decode-sections f))
           (overlaps (count-section-overlaps sections #f)))
    (display "Overlaps: ")
    (display overlaps)
    (newline)))

(define (main-part2)
    (let* ((f        (open-input-file "./data/4.sections.txt"))
           (sections (decode-sections f))
           (overlaps (count-section-overlaps sections #t)))
    (display "Overlaps: ")
    (display overlaps)
    (newline)))