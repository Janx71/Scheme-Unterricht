(define (verschiebung ascii schlüssel)
  (cond ((and (>= ascii 65) (<= ascii 90))
         (cond ((> (+ ascii schlüssel) 90) (integer->char (- (+ ascii schlüssel) 26)))
               (else (integer->char (+ ascii schlüssel)))))
        (else (cond ((and (>= ascii 97) (<= ascii 122))
               (cond ((> (+ ascii schlüssel) 122) (integer->char (- (+ ascii schlüssel) 26)))
               (else (integer->char (+ ascii schlüssel)))))))))
        

(define (getFirstAsInt input)
  (display (char->integer (car (string->list input))))
  (char->integer (car (string->list input))))

(define (string-tail str)
  (if (or (null? str) (<= (string-length str) 1))
      ""
      (list->string (cdr (string->list str)))))

;
(define (caesar-verschiebung text schlüssel temp)
  (cond ((= (string-length text) 0) temp)
        (else (caesar-verschiebung (string-tail text) schlüssel (string-append  temp (string (verschiebung (getFirstAsInt text) schlüssel)))))))
         
