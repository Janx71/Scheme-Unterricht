;Implementation der Caesar-Verschiebung in beide Richtungen

;Nimmt einen ascii int and und verschiebt um den Schlüssel, wenn er einen Buchstaben repräsentiert. Ergebnis wird in Char umgewandelt.
;Verschiebung: (Integer im ascii bereich) (schlüssel von 0 - 26) (operator (+ für Verschlüsslung, - für Entschlüsslung)).
;(verschiebung 65 2 +) --> #\C ; (verschiebung 65 2 -) --> #\Y ; (verschiebung 97 2 +) --> #\c ; (verschiebung 97 2 -) --> #\y
(define (verschiebung ascii schlüssel operator)
  (cond ((and (>= ascii 65) (<= ascii 90))
         (cond ((or (> (operator ascii schlüssel) 90) (< (operator ascii schlüssel) 65))
                (integer->char ((opposite-op operator) (operator ascii schlüssel) 26)))     
               (else (integer->char (operator ascii schlüssel)))))
        (else (cond ((and (>= ascii 97) (<= ascii 122))
               (cond ((or (> (operator ascii schlüssel) 122) (< (operator ascii schlüssel) 97))
                      (integer->char ((opposite-op operator) (operator ascii schlüssel) 26)))
               (else (integer->char (operator ascii schlüssel)))))
               (else (integer->char (operator ascii schlüssel)))))))

;Dreht den die Operatoren "+" und "-" zum Gegenteil um.
;(opposite-op +) --> - ; (opposite-op -) --> +
(define (opposite-op op)
  (cond ((eq? op +) -)
        ((eq? op -) +)))
        

;Wandelt String in liste<Char> um, wandelt ersten char in integer um.
;(getFirstAsInt "hallo") --> 104
(define (getFirstAsInt input)
  (char->integer (car (string->list input))))

;Entfern ersten char eines Strings
;(string-tail "hallo") --> "allo"
(define (string-tail str)
  (if (or (null? str) (<= (string-length str) 1))
      ""
      (list->string (cdr (string->list str)))))

;Ver- Entschlüsselt einen String mithilfe des Caesar-Verfahrens.
;caesar-verschiebung: (Zu Ver- Entschlüsselnder Text) (Schlüssel: 0-26) (Operator: + für Ver- und - für Entschlüsslung) (temp = "")
(define (caesar-verschiebung text schlüssel operator temp)
  (cond ((= (string-length text) 0) temp)
        (else (caesar-verschiebung (string-tail text) schlüssel operator (string-append  temp (string (verschiebung (getFirstAsInt text) schlüssel operator)))))))
         
         

;(caesar-verschiebung "Hallo mein Name ist Jan" 12 + "") -->"Tmxxa,yquz,Zmyq,uef,Vmz"
;(caesar-verschiebung "Tmxxa,yquz,Zmyq,uef,Vmz" 12 - "") --> "Hallo mein Name ist Jan"
