(use-modules (ice-9 rdelim) (srfi srfi-1))

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
                    (decode-rucksacks port (append rucksacks (list (map item->priority (string->list line))))))))))


(define (split-compartments rucksack)
    (split rucksack (/ (length rucksack) 2)))

(define (get-badge group)
    (find
        (lambda (x)
            (every (lambda (rucksack) (member x rucksack)) (cdr group)))
        (car group)))

(define (group-by lst n)
    (fold-right
        (lambda (x groups)
            (if (< (length (car groups)) n)
                (cons (append (car groups) (list x)) (cdr groups))
                (cons (list x) groups)))
        '(())
        lst))

(define (get-rucksacks)
    (decode-rucksacks  (open-input-file "data/3.rucksacks.txt")))

(define (main-part1)
    (let* ((dups            (map (lambda (x) (apply duplicates x)) (map split-compartments (get-rucksacks))))
           (priority-total  (apply + (map (lambda (x) (apply + x)) dups))))
        (display priority-total)
        (newline)))

(define (main-part2)
    (let* ((groups          (group-by (get-rucksacks) 3))
           (priority-total  (apply + (map get-badge groups))))
        (display priority-total)
        (newline)))
