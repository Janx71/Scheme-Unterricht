(define (schlüssel-anpassen str n)
  (cond((>= (string-length str) n) (substring str 0 n))
    (else (schlüssel-anpassen (string-append str str) n))))

(define (char->int c)
  (- (char->integer c) (char->integer #\a)))

(define (int->char i)
  (integer->char (+ i (char->integer #\a))))

(define (wrap n)
  (if (>= n 26) (- n 26) n))

(define (shift-char c shift)
  (int->char (wrap (+ (char->int c) (char->int shift)))))

(define (vigenere klartext key)
  (define (helper text-liste key-liste temp)
    (cond ((null? text-liste) (list->string temp))
          (else (helper (cdr text-liste) (cdr key-liste) (append temp (list (shift-char (car text-liste) (car key-liste))))))))
  (helper (string->list klartext) (string->list (schlüssel-anpassen key (string-length klartext))) '()))
