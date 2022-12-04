(use-modules (ice-9 rdelim))

(define (read-inventories port inventories)
    (let ((line (read-line port 'trim)))
        (cond
            ((eof-object? line) inventories)
            ((zero? (string-length line))
                (read-inventories port (cons '() inventories)))
            (else (read-inventories port (cons (cons (string->number line) (car inventories)) (cdr inventories)))))))

(let* ((inventory-file (open-input-file "data/1.calories.txt"))
      (inventories (read-inventories inventory-file '(())))
      (sums (map (lambda (x) (apply + x)) inventories))
      (max (apply max sums)))
    (display max)
    (newline))
