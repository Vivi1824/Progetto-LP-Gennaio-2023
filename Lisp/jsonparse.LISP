(defun jsonparse (json)
   (let ((json-list 
          (funcall 'remove-ws (coerce json 'list))))
    (cond
     ((equal (first json-list) '#\{) 
      (funcall 'parse-object (cdr json-list)))
     ((equal (first json-list) '#\[) 
      (funcall 'parse-array (cdr json-list)))
     (T (error "syntax-error-parse"))
    )))

;;; jsonparse-array (json)
(defun parse-array (json)
(let ((njson (funcall 'remove-ws json)))
  (cond
   ((and 
     (equal (car njson) '#\])
     (null (remove-ws (cdr njson)))) 
    '(jsonarray))
   (T (let ((result (parse-elements njson NIL)))
        (if (null (remove-ws (car (cdr result))))
            (append '(jsonarray) (car result))
          (error "syntax-error-not-empty")))))))
       

;;; parse-object (json)
(defun parse-object (json)
(let ((njson (remove-ws json)))
  (cond
   ((and 
     (equal (car njson) '#\})
     (null (remove-ws (cdr njson)))) 
    '(json-obj))
   (T (let ((result (parse-members njson NIL)))
        (if (null (remove-ws (car (cdr result))))
            (append '(jsonobj) (car result))
          (error "syntax-error-not-empty")))))))


;;;parse-elements (json obj)
(defun parse-elements (json obj)
  (let ((result (parse-value json))) 
    (delimiter-array result obj)))

;;;parse-members (json obj)
(defun parse-members (json obj)
  (let ((result (parse-pair json)))
  (delimiter-object result obj)))

;;;parse-pair (json obj)
(defun parse-pair (json)
  (let((njson (remove-ws json)))
    (if (or (equal (car njson) '#\") (equal (car njson) '#\'))
        (let ((result (parse-string njson)))
          (delimiter-comma result))
      (error "syntax-error"))))


(defun delimiter-comma (json)
    (let((njson (remove-ws (car (cdr json))))
        (obj (list (car json))))
    (if (equal (car njson) '#\:)
        (let ((result 
               (parse-value 
                (remove-ws (cdr njson)))))
           (append
            (list (append 
                   obj
                   (list (car result))))
            (list (car (cdr result)))))
      (error "syntax-error-comma"))))

;;;parse-value (json)
(defun parse-value (json)
  (let ((njson (remove-ws json)))
    (cond
     ((or (equal (car njson) '#\") 
          (equal (car njson) '#\')) 
      (parse-string njson))
     ((and (char<= '#\0 (car njson)) 
           (char>= '#\9 (car njson))) 
      (parse-number njson NIL))
     ((char= '#\- (car njson)) 
      (parse-number (cdr njson) '(#\-)))
     ((or 
       (equal (car njson) '#\{) 
       (equal (car njson) '#\[)) 
      (parse-nested njson))
     (T (error "syntax-error")))))


(defun parse-number (json temp)
  (cond
   ((null json) (error "syntax-error"))
   ((and (char<= '#\0 (car json)) 
         (char>= '#\9 (car json))) 
    (parse-number 
     (cdr json) 
     (append temp (list (car json)))))
   ((char= '#\. (car json)) 
    (parse-number-float 
     (cdr json) 
     (append temp (list (car json)))))
   (T (append 
       (list (parse-integer (coerce temp 'string)))
       (remove-ws (list json))))
))


(defun parse-number-float (json temp)
  (cond
   ((or 
     (null json)
     (char= '#\. (car json))) 
    (error "syntax-error"))
   ((and 
     (char<= '#\0 (car json)) 
     (char>= '#\9 (car json))) 
    (parse-number-float 
     (cdr json) 
     (append temp (list (car json)))))
   (T (append 
       (list (parse-float (coerce temp 'string)))
       (remove-ws (list json))))
))

;;;parse-string (json)
(defun parse-string (json)
  (cond 
   ((char= '#\' (car json)) 
    (parse-string-sq (cdr json) NIL))
   ((char= '#\" (car json)) 
    (parse-string-dq (cdr json) NIL))))

;;;parse-string-sq (json temp)
(defun parse-string-sq (json temp)
  (cond
   ((null json) (error "quotes-not-closed"))
   ((equal (car json) '#\") (error "dq-inside-sq"))
   ((not (equal (car json) '#\'))
    (parse-string-sq 
     (cdr json) 
     (append temp (list (car json)))))
   (T (append 
        (list (coerce temp 'string))
        (remove-ws (list (cdr json)))))
))


(defun parse-string-dq (json temp)
  (cond
   ((null json) (error "quotes-not-closed"))
   ((equal (car json) '#\') (error "sq-inside-dq"))
   ((not (equal (car json) '#\"))
    (parse-string-dq 
     (cdr json) 
     (append temp (list (car json)))))
   (T (append 
        (list (coerce temp 'string))
        (remove-ws (list (cdr json)))))
))
   

(defun parse-nested (json)
  (cond
   ((equal (first json) '#\{) 
    (let ((result (parse-object-nested (cdr json))))
      result))
   ((equal (first json) '#\[) 
    (let ((result (parse-array-nested (cdr json))))
      result))
    ))


(defun parse-array-nested (json)
  (let ((njson (remove-ws  json)))
   (cond
   ((equal (car njson) '#\]) 
    (append 
     (list '(jsonarray)) 
     (list (cdr njson))))
   (T (let ((result (parse-elements njson NIL)))
        (append 
         (list (append '(jsonarray) (car result))) 
         (list (car (cdr result)))))))))

;;;parse-object-nested (json)
(defun parse-object-nested (json)
  (let ((njson (remove-ws json)))
   (cond
   ((equal (car njson) '#\}) 
    (append 
     (list '(jsonobj)) 
     (list (cdr njson))))
   (T (let ((result (parse-members njson NIL))) 
        (append 
         (list (append '(jsonobj) (car result))) 
         (list (car (cdr result)))))))))

;;;json-delimiter-array (json obj)
(defun delimiter-array (json obj)
  (let ((nobj (append obj (list (car json))))
        (njson (remove-ws (car (cdr json)))))
    (cond
     ((char= (car njson) '#\]) 
      (append (list nobj) (list (remove-first njson))))
     ((char= (car njson) '#\,) 
      (parse-elements (remove-first njson) nobj))  
     (T (error "syntax-error-delim-array")))))


(defun delimiter-object (json obj)
  (let ((nobj (append obj (list (car json))))
        (njson (remove-ws (car (cdr json)))))
    (cond
     ((char= (car njson) '#\}) 
      (append (list nobj) (list (remove-first njson))))
     ((char= (car njson) '#\,) 
      (parse-members (remove-first njson) nobj))  
     (T (error "syntax-error-delim-obj")))))

;;;remove-first (list)
(defun remove-first (list)
  (cdr list))
;;;remove-last (list)
(defun remove-last (list)
  (if (null (cdr list))
      NIL
    (cons (car list) (remove-last (cdr list)))))

;;;remove-ws (list)
(defun remove-ws (list)
  (if (or (equal (car list) '#\Space)
          (equal (car list) '#\Newline)
          (equal (car list) '#\Tab))
      (remove-ws (cdr list))
    list))

;;; json_get(json, &rest fields)
;;; -Follows a chain of keys (iff JSON_obj at current level 
;;;  is an object) or indexes (iff JSON_obj at current level 
;;;  is an array) in order to retrieve a certain value.
;;; -The main idea is to simply go through the list as needed.
;;; -Two different predicates are used since the keyword 
;;;  &rest had a few issues with recursive calls.

(defun jsonaccess (json &rest fields)
  (jsonaccess-2 json fields))

(defun jsonaccess-2 (json fields)
  (cond
   ((and (eq (list-length fields) 1)
         (listp json)
         (stringp (car fields))
         (eq (car JSON) 'jsonobj)) 
    (search-by-key (cdr json) (car fields)))
   ((and (eq (list-length fields) 1) 
         (listp json)
         (numberp (car fields))
         (>= (car fields) 0)
         (eq (car JSON) 'jsonarray)) 
    (search-by-index (cdr json) (car fields)))
   ((and (> (list-length fields) 1) 
         (listp json)
         (stringp (car fields))
         (eq (car JSON) 'jsonobj)) 
    (jsonaccess-2
     (search-by-key (cdr json) (car fields))
     (cdr fields)
     ))
   ((and (> (list-length fields) 1)
         (listp json)
         (numberp (car fields))
         (>= (car fields) 0)
         (eq (car JSON) 'jsonarray)) 
    (jsonaccess-2
     (search-by-index (cdr json) (car fields))
     (cdr fields)
     ))
   (T (error "Syntax-error"))))


(defun search-by-key (json key)
  (cond
   ((NULL json) (error "Key-not-found"))
   ((equal (car (car json)) key) (car (cdr (car json))))
   (T (search-by-key (cdr json) key))
   ))


(defun search-by-index (json index)
  (cond
   ((NULL json) (error "Index-not-found"))
   ((eq index 0) (car json))
   (T (search-by-index (cdr json) (1- index)))
   ))


(defun json-read (filename)
  (with-open-file (stream filename 
                          :direction :input 
                          :if-does-not-exist :error)
    (let ((contents (make-string (file-length stream))))
       (let ((position (read-sequence contents stream))) 
         (jsonparse (subseq contents 0 position))))))

(defun json-dump (JSON filename)
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (format stream (to-string JSON))))

;;; json-to-string (json)
(defun to-string (JSON)
  (cond
   ((eq (car JSON) 'jsonobj) 
    (concatenate 'string 
                 "{" 
                 (remove-last-comma
                  (print-obj (cdr JSON))) 
                 "}"
                 ))
   ((eq (car JSON) 'jsonarray) 
    (concatenate 'string 
                 "[" 
                 (remove-last-comma
                  (print-array (cdr JSON)))
                 "]"
                 ))
   (T (error "Syntax-error"))))

;;; json-print-obj (json)
(defun print-obj (JSON)
  (cond
   ((NULL JSON) "")
   ((listp (car JSON)) 
    (concatenate 'string 
                 (print-pair (car JSON)) 
                 (print-obj (cdr JSON))
                 ))))

;;; json-print-pair (json)
(defun print-pair (JSON)
  (concatenate 'string "\""
               (car JSON)
               "\"" ":" 
               (print-value (car (cdr JSON)))
               ","
               ))

;;; json-print-value (json)
(defun print-value (JSON)
  (cond
   ((numberp JSON) 
    (write-to-string JSON))
   ((stringp JSON) 
    (concatenate 'string "\"" JSON "\""))
   (T (to-string JSON))))

;;; json-print-array (json)
(defun print-array (JSON)
  (cond
   ((NULL JSON) "")
   (T (concatenate 'string 
      (print-value (car JSON))
       ","
      (print-array (cdr JSON))
    ))))

;;; remove-last-comma (json)
(defun remove-last-comma (JSON)
  (cond
    ((string= "" JSON) JSON)
    (T (subseq JSON 0 (- (length JSON) 1)))))





























