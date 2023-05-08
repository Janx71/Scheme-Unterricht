;Caesar knacken 1

;Gibt die häufigkeit eines chars in einer Liste von char als Zahl aus.
;(count #\e '(#\e #\a #\e #\w) 0) -> 2
(define (count char lst temp)
  (if (null? lst) temp
      (cond ((equal? char (car lst)) (count char (cdr lst) (+ temp 1)))
         (else (count char (cdr lst) temp)))))

;Findet den häufigsten Buchstaben in einem String mithilfe der Funktion "count" heraus.
;Sucht nur nach Buchstaben im deutschen Alphabet.
;(get-frequent-char "abbcccd") -> #\c
(define (get-frequent-char str)
  (define (temp-f str-list alphabet-list temp-char temp-number) 
                  (cond ((or (null? str-list) (null? alphabet-list)) temp-char)
                        (else (cond ((> (count (car alphabet-list) str-list 0) temp-number)
                                     (temp-f str-list (cdr alphabet-list) (car alphabet-list) (count (car alphabet-list) str-list 0)))
                                     (else (temp-f str-list (cdr alphabet-list) temp-char temp-number))))))
  (define alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (temp-f (string->list str) alphabet (car alphabet) 0))

;Findet den Schlüssel, wenn häufigster Buchstabe im Alphabet und im Text bekannt.
;Input als ascii int.
;(get-key 100 99) -> 25
(define (get-key language-int text-int)
  (cond ((< (- text-int language-int) 0) (+ (- text-int  language-int) 26))
        (else (- text-int language-int))))

;Nutzt obere Funktion um den Schlüssel für den verschlüsselten Text zu ermitteln und auszugeben.
;Wandelt chars in integer um, für die nutzung in "get-key".
;Wandelt alle Buchstaben im String in Kleinbuchstaben um für die Nutzung in "get-frequent-char"
(define (analyse str freq-char)
  (get-key (char->integer freq-char) (char->integer (get-frequent-char (string-downcase str)))))
  

(analyse "{input text}" #\e)
