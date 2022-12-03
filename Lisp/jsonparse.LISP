(defun json-parse(json)
  (let((json-list
        (remove-ws(coerce json 'list))))
    (cond
     ((equal(first json-list)'#\{)
      (parse-object(cdr json-list)))
     ((equal(first json-list)'#[)
      (parse-array(cdr json-list)))
     (T(error "syntax-error-parse")))))


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

(defun parse-object(json)
  (let((njson(remove-ws json)))
    (cond
     ((and
       (equal(car njson)'#\))
       (null(remove-ws(cdr njson))))
      '(json-obj))
     (T(let((result(parse-members njson NIL)))
         (if(null(remove-ws(car(cdr result))))
             (append '(json-obj)(car result))
           (error "syntax-error-not-empty")))))))

(defun delimiter-array(json obj)
  (let((nobj(append obj(list(car json))))
       (njson(remove-ws(car(cdr json)))))
    (cond
     ((char=(car njson)'#\])
      (append(list nobj)(list(remove-first njson))))
     ((char=(car njson)'#\,)
      (parse-elements(remove-first njson)nobj))
     (T(error "syntax-error-delim-array")))))

(defun delimiter-object (json obj)
  (let((nobj(append obj(list(car json))))
       (njson(remove-ws(car(cdr json)))))
    (cond
     ((char=(car njson)'#\})
      (append(list nobj)(list(remove-first njson))))
     ((char=(car njson)'#\,)
      (parse-members(remove-first njson)nobj))
     (T(error "syntax-error-delim-obj")))))

(defun parse-elements(json obj)
  (let((result (parse-value json)))
    (delimiter-array result obj)))

(defun parse-array(json)
  (let((njson(funcall 'remove-ws json)))
    (cond
     ((and
       (equal(car njson) '#\])
       (null(remove-ws(cdr njson))))
      '(json-array))
     (T(let((result(parse-elements njson NIL)))
         (if(null(remove-ws(car(cdr result))))
             (append '(json-array)(car result))
           (error "syntax-error-not-empty")))))))

(defun parse-array-nested(json)
  (let((njson(remove-ws json)))
    (cond
     ((equal(car njson)'#\])
      (append
       (list '(json-array))
       (list (cdr njson))))
     (T(let((result(parse-elements njson NIL)))
         (append
          (list(append'(json-array)(car result)))
          (list(car(cdr result)))))))))


(defun parse-nested(json)
  (cond
   ((equal(first json)'#\{)
    (let((result(parse-object-nested(cdr json))))
      result))
   ((equal(first json)'#\[)
    (let((result(parse-array-nested(cdr json))))
      result))))

;Lettura e scrittura 
(defun remove-last-comma(JSON)
  (cond
   ((string="" JSON)JSON)
   (T(subseq JSON 0(-(length JSON)1)))))


(defun print-value(value)
  (cond
   ((numberp value)
    (write-to-string value))
   ((stringp value)
    (concatenate 'string "\""value"\""))
   (T(to-string value))))

(defun print-pair(JSON)
  (concatenate 'string "\""
               (car JSON)
               "\"" ":"
               (print-value(car(cdr JSON)))
               ","
               ))




(defun print-array(JSON)
  (cond
   ((NULL JSON)"")
   (T(concatenate 'string
     (print-value(car JSON))
     ","
     (print-array(cdr JSON))))))


(defun print-obj(JSON)
  (cond
   ((NULL JSON)"")
   ((listp(car JSON))
    (concatenate 'string
                 (print-pair(car JSON))
                 (print-obj(cdr JSON))))))


(defun search-by-key(json key)
  (cond
   ((NULL json)(error "key-not-found"))
   ((equal(car(car json))key)(car(cdr(car json))))
   (T(search-by-key(cdr json)key))))


(defun search-by-index(json index)
  (cond
   ((NULL json)(error "index-not-found"))
   ((eq index 0)(car json))
   (T(search-by-index(cdr json)(1-index)))))


(defun json-get-2 (json fields)
  (cond
   ((and(eq(list-length fields)1)
        (listp json)
        (stringp(car fields))
        (eq(car JSON) 'json-obj))
    (search-by-key(cdr json)(car fields)))
   ((and(eq(list-length fields)1)
        (listp json)
        (numberp(car fields))
        (>=(car fields)0)
        (eq(car JSON)'json-array))
    (search-by-index(cdr json)(car fields)))
   ((and(>(list-length fields)1)
        (listp json)
        (stringp(car fields))
        (eq(car JSON)'json-obj))
    (json-get-2
     (search-by-key(cdr json)(car fields))
     (cdr fields)))
   ((and(>(list-length fields)1)
        (listp json)
        (numberp(car fields))
        (>=(car fields)0)
        (eq(car JSON)'json-array))
    (json-get-2
     (search-by-index(cdr json)(car fields))
     (cdr fields)))
   (T(error "Syntax-error"))))


(defun json-get (json&rest fields)
  (if(null fields)
      json
    (json-get-2 json fields)))


(defun jsonread(filename)
  (with-open-file(stream filename
                         :direction :input
                         :if-does-not-exist :error)
    (let((contents(make-string(file-length stream))))
      (let((position(read-sequence contents stream)))
        (json-parse(subseq contents 0 positions))))))

(defun to-string (JSON)
  (cond
   ((eq(car JSON)'json-obj)
    (concatenate 'string
                 "{"
                 (remove-last-comma
                  (print-obj(cdr JSON)))
                 "}"))
   ((eq(car JSON)'json-array)
    (concatenate 'string
                 "["
                 (remove-last-comma
                  (print-array(cdr JSON)))
                 "]"))
   (T(error "Syntax-error"))))

(defun jsondump(JSON filename)
  (with-open-file(stream filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (format stream (to-string JSON))filename))