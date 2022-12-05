(use-modules (ice-9 rdelim))

(define split
    (case-lambda
        ((list ind)
            (if (zero? ind) (cons '(()) list)
                (split (cdr list) (- ind 1) (cons (car list) '()))))
        ((right ind left)
            (if (zero? ind) (cons left (list right))
                (split (cdr right) (- ind 1) (append left (list (car right))))))))

(define (item->priority c)
    (cond
        ((char>=? c #\a) (+ 1 (- (char->integer c) (char->integer #\a))))
        ((char>=? c #\A) (+ 26 (- (char->integer c) (char->integer #\A))))))

(define decode-rucksacks
    (case-lambda
        ((port) (decode-rucksacks port '()))
        ((port rucksacks)
            (let ((line (read-line port 'trim)))
                (if (eof-object? line) rucksacks
                    (let* ((all-items (map item->priority (string->list line)))
                           (compartments (split all-items (/ (length all-items) 2))))
                        (decode-rucksacks port (append rucksacks (list compartments)))))))))

(let* ((rucksacks-file (open-input-file "data/3.rucksacks.txt"))
       (rucksacks (decode-rucksacks rucksacks-file)))
    (display rucksacks)
    (newline))