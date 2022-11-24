;rimuove spazi bianchi
(defun remove-ws (list)
  (if(or(equal(car list) '#\Space)
        (equal(car list) '#\Newline)
        (equal(car list) '#\Tab))
      (remove-ws(cdr list)) list))

;rimuove primo elemento
(defun remove-first(list)
  (cdr list))

;rimuove ultimo elemento
(defun remove-last(list)
  (if(null(cdr list))
      NIL
    (cons(car list)(remove-last(cdr list)))))

(defun creation-ss(json temp)
  (cond
   ((null json)(error "quotes not closed"))
   ((equal (car json) '#\") (error "ds inside sequence"))
   ((not (equal(car json)'#\'))
    (creation-ss(cdr json)(append(temp(list(car json)))))
   (T(append (list (coerce temp 'string))
             (remove-ws(list(cdr json))))))))

(defun creation-ds(json temp)
  (cond
   ((null json)(error "quotes not closed"))
   ((equal(car json)'#\')(error "ss inside ds"))
   ((not(equal(car json) '#\"))
    (creation-ds(cdr json)(append temp (list (car json)))))
   (T(append(list(coerce temp 'string))
            (remove-ws(list(cdr json)))))))

(defun parse-string (json)
  (cond 
   ((char = '#\' (car json))
    (creation-ss(cdr json)NIL))
   ((char = '#\' (car json))
    (creation-ds(cdr json)NIL))))



(defun parse-value(json)
  (let((njson(remove-ws)))
    (cond
     ((or(equal(car njson)'#\")
         (equal(car njson)'#\'))
      (parse-string njson))
     ((and(char<='#\0 (car njson))
          (char>='#\9 (car njson)))
      (parse-number njson NIL))
     ((char='#\- (car njson))
      (parse-number(cdr njson) '(#\-)))
     ((char='#\+ (car njson))
      (parse-number (cdr njson) '(#\+)))
     ((or
       (equal(car njson) '#\{)
       (equal(car njson) '#\[))
      (parse-nested njson))
     (T(error "syntax-error")))))