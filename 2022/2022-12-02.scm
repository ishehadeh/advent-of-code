(use-modules (ice-9 rdelim) (ice-9 match))

(define decode-strategy-guide
    (case-lambda
        ((port) (decode-strategy-guide port '()))
        ((port games)
            (let ((line (read-line port 'trim)))
                (if (eof-object? line) games
                    ; line looks like "A X"
                    (let* ((line-codepoints (map char->integer (string->list line)))
                           (opponent (car line-codepoints))
                           (response (car (cdr (cdr line-codepoints)))))
                        (decode-strategy-guide port (cons (cons (+ 1 (- opponent (char->integer #\A))) (+ 1 (- response (char->integer #\X)))) games))))))))

(define (wrap x min max)
    (cond
        ((< x min) (wrap (+ max (- x min -1)) min max))
        ((> x max)  (wrap (+ min (- x max 1)) min max))
        (else x)))

(define (winner game)
    (match game
        ((3 . 1) 6)
        ((1 . 2) 6)
        ((2 . 3) 6)
        ((x . x) 3)
        (_ 0)))

(define (choice-from-opponent-and-result game)
    (match game
        ((x . 2) x)
        ((x . 1) (wrap (- x 1) 1 3))
        ((x . 3) (wrap (+ x 1) 1 3))))

(define (score-game game)
    (+ (cdr game) (winner game)))

(define (score-from-opponent-and-result game)
    (+  (choice-from-opponent-and-result game)
        (* (- (cdr game) 1) 3)))

(let* ((strategy-file (open-input-file "data/2.games.txt"))
       (strategy (decode-strategy-guide strategy-file))
       (score (apply + (map score-game strategy)))
       (score-part-2 (apply + (map score-from-opponent-and-result strategy))))
    (display score)
    (newline)
    (display score-part-2)
    (newline))
