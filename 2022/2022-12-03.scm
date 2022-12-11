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
        ((char>=? c #\A) (+ 27 (- (char->integer c) (char->integer #\A))))))

(define (push-unique x lst)
    (if (not (member x lst))
        (append lst (list x))
        lst))

(define decode-rucksacks
    (case-lambda
        ((port) (decode-rucksacks port '()))
        ((port rucksacks)
            (let ((line (read-line port 'trim)))
                (if (eof-object? line) rucksacks
                    (let* ((all-items (map item->priority (string->list line)))
                           (compartments (split all-items (/ (length all-items) 2))))
                        (decode-rucksacks port (append rucksacks (list compartments)))))))))

(define duplicates
    (case-lambda
        ((list0 list1) (duplicates list0 list1 list1 '()))
        ((list0 list1 list1-full dups)
            (cond
                ((eqv? list0 '()) dups)
                ((eqv? list1 '()) (duplicates (cdr list0) list1-full list1-full dups))
                ((eqv? (car list0) (car list1))
                    (duplicates list0 (cdr list1) list1-full (push-unique (car list0) dups)))
                (else
                    (duplicates list0 (cdr list1) list1-full dups))))))

(define (get-rucksacks)
    (decode-rucksacks  (open-input-file "data/3.rucksacks.txt")))

(define (main)
    (let* ((rucksacks-file  (open-input-file "data/3.rucksacks.txt"))
           (rucksacks       (decode-rucksacks rucksacks-file))
           (dups            (map (lambda (x) (apply duplicates x)) (get-rucksacks)))
           (priority-total  (apply + (map (lambda (x) (apply + x)) dups))))
        (display priority-total)
        (newline)))