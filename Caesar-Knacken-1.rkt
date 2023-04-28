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
  (define alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (temp-f (string->list str) alphabet (car alphabet) 0))

(define (get-key str freq-char)
  (define (temp-g freq l-freq)
    (cond ((< (- freq  l-freq) 0) (+ (- freq  l-freq) 26))
          (else (- freq  l-freq))))
  (temp-g (char->integer (get-frequent-char str)) (char->integer freq-char)))
  

(get-key "target text" #\e)
