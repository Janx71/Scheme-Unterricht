(define alphabet '(#\e #\n #\i #\r #\s #\a #\t #\d #\h #\u #\l #\g #\o #\c #\m #\b #\f #\w #\k #\z #\p #\v #\j #\y #\x #\q))

(define (count char lst temp)
  (if (null? lst) temp
      (cond ((equal? char (car lst)) (count char (cdr lst) (+ temp 1)))
         (else (count char (cdr lst) temp)))))

(define (get-frequent-char str)
  (define (temp-f str-list alphabet-list temp-char temp-number) 
                  (cond ((or (null? str-list) (null? alphabet-list)) temp-char)
                        (else (cond ((> (count (car alphabet-list) str-list 0) temp-number)
                                     (temp-f str-list (cdr alphabet-list) (car alphabet-list) (count (car alphabet-list) str-list 0)))
                                     (else (temp-f str-list (cdr alphabet-list) temp-char temp-number))))))
  (temp-f (string->list str) alphabet (car alphabet) 0))

(define (get-key str freq-char)
  (define (temp-g freq l-freq)
    (cond ((< (- freq l-freq) 0) (+ (- freq l-freq) 26))
          (else (- freq l-freq))))
  (temp-g (char->integer (get-frequent-char str)) (char->integer freq-char)))

(define (remove-char str char)
  (define (helper str result)
    (if (null? str)
        (list->string (reverse result))
        (if (char=? (car str) char)
            (helper (cdr str) result)
            (helper (cdr str) (cons (car str) result)))))
  (helper (string->list str) '()))

(define (get-keys str list)
  (define (temp str key list speicher)
    (cond ((null? list) (most-frequent speicher))
          (else (temp (remove-char str (get-frequent-char str)) (car list) (cdr list) (cons (get-key str key) speicher)))))
  (temp str (car list) (cdr list) '()))


(define (most-frequent lst)
  (if (null? lst)
      (error "List is empty")
      (let loop ((rest lst) (max-num (car lst)) (max-count 0))
        (if (null? rest)
            max-num
            (let* ((num (car rest))
                   (count (length (filter (lambda (x) (= x num)) lst))))
              (if (> count max-count)
                  (loop (cdr rest) num count)
                  (loop (cdr rest) max-num max-count)))))))
