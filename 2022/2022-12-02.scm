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
    
(define (winner game)
    (match game
        ((3 . 1) 6)
        ((1 . 2) 6)
        ((2 . 3) 6)
        ((x . x) 3)
        (_ 0)))

(define (score-game game)
    (+ (cdr game) (winner game)))

(let* ((strategy-file (open-input-file "data/2.games.txt"))
       (strategy (decode-strategy-guide strategy-file))
       (score (apply + (map score-game strategy))))
    (display strategy)
    (newline)
    (display score)
    (newline))
