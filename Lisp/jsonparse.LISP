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

(defun parse-number(json temp)
  (cond
   ((null json)(error "syntax-error"))
   ((and(char<='#\0 (car json))
        (char>='#\9 (car json)))
    (parse-number
     (cdr json)
     (append temp (list(car json)))))
   ((char='#\.(car json))
    (parse-number-float
     (cdr json)
     (append temp(list(car json)))))
   (T(append
      (list(parse-integer(coerce temp 'string)))
      (remove-ws(list json))))))

(defun parse-number-float(json temp)
  (cond
   ((or
     (null json)
     (char='#\. (car json)))
    (error "syntax-error"))
   ((and
     (char<='#\0(car json))
     (char>='#\9(car json)))
    (parse-number-float(cdr json)(append temp(list(car json)))))
   (T(append
      (list(parse-float(coerce temp 'string)))
      (remove-ws(list json))))))

(defun parse-obj-nested(json)
  (let((njson(remove-ws json)))
    (cond
     ((equal(car njson)'#\})
      (append
       (list'(json-obj))
       (list(cdr njson))))
     (T(let((result(parse-members njson NIL)))
         (append
          (list(append'(json-obj)(car result)))
          (list(car(cdr result)))))))))


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


(defun delimiter-for-comma(json)
  (let((njson(remove-ws(car(cdr json))))
       (obj(list(car json))))
    (if(equal(car njson)'#\:)
        (let((result
              (parse-value(remove-ws(cdr njson)))))
          (append
           (list(append
                 obj
                 (list(car result))))
           (list(car(cdr result)))))
      (error "syntax-error-comma"))))


(defun parse-pair(json)
  (let((njson(remove-ws json)))
    (if(or(equal(car njson)'#\")(equal(car njson)'#\'))
        (let((result(parse-string njson)))
          (delimiter-for-comma result))
      (error "syntax-error-pair"))))


(defun parse-members(json obj)
  (let((result(parse-pair json)))
    (delimiter-object result obj)))