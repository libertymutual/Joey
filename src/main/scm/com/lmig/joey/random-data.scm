(module-name random-data)

(import (srfi 69))

(define (random-int n)
  (inexact->exact (floor (* (java.lang.Math:random) n))))

(define
  (random-picker l)
  (let* ((v (list->vector l)))
    (lambda ()
      (vector-ref v (random-int (vector-length v))))))

(define *states-list* '("AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "FL" "GA" "HI" "ID" "IL" "IN"
			"IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV"
			"NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN"
			"TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY" "DC" "MH"))

(define *firstnames-list* '("dade" "kate" "emmanuel" "paul" "joey" "ruth" "lauren" "eugene" "margo" "duke" "leonardo" "richard"))
(define *lastnames-list* '("murphy" "libby" "goldstein" "cook" "pardella" "belford" "wallace" "ellingson" "da vinci" "gill"))

(define random-state
  (random-picker *states-list*))

(define (random-numeric)
  (integer->char (+ 48 (random-int 10))))

(define (random-alphanumeric)
  (let* ((i (random-int 62)))
    (cond
     ((< i 26) (integer->char (+ 65 i)))
     ((< i 52) (integer->char (+ 97 (- i 26))))
     (else (integer->char (+ 48 (- i 52)))))))

(define (random-alphabetic)
  (let* ((i (random-int 52)))
    (cond
     ((< i 26) (integer->char (+ 65 i)))
     (else (integer->char (+ 97 (- i 26)))))))

(define (random-string-gen char-gen)
  (lambda (min-length max-length)
    (let* ((len (+ min-length (random-int (+ 1 (- max-length min-length))))))
      (let loop ((i 0)
		 (s '()))
	(cond ((>= i len) (list->string (reverse! s)))
	      (else (loop (+ i 1) (cons (char-gen) s))))))))

(define random-numeric-string (random-string-gen random-numeric))
(define random-alphabetic-string (random-string-gen random-alphabetic))
(define random-alphanumeric-string (random-string-gen random-alphanumeric))

(define random-last-name (random-picker *lastnames-list*))
(define random-first-name (random-picker *firstnames-list*))
(define (random-name-ln-fn) (string-append (random-last-name) ", " (random-first-name)))
(define (random-name-fn-ln) (string-append (random-first-name) " " (random-last-name)))
(define (random-zip) (random-numeric-string 5 5))
(define (random-city) (random-alphabetic-string 4 15))
(define (random-phone) (random-numeric-string 10 10))
(define (random-email) (string-append (random-alphanumeric-string 6 12)
				      "@"
				      (random-alphanumeric-string 6 12)
				      "."
				      (random-alphabetic-string 3 3)))
(define (random-employee-number)
	 (string-append "n"
			(random-numeric-string 7 7)))

(define *random-generators*
  (alist->hash-table
   `((numeric . ,random-numeric-string)
     (alphabetic . ,random-alphabetic-string)
     (alphanumeric . ,random-alphanumeric-string)
     (last-name . ,random-last-name)
     (first-name . ,random-first-name)
     (name-last-first . ,random-name-ln-fn)
     (name-first-last . ,random-name-fn-ln)
     (zip . ,random-zip)
     (city . ,random-city)
     (state . ,random-state)
     (phone . ,random-phone)
     (pick . ,(lambda x ((random-picker x))))
     (email . ,random-email)
     (employee-number . ,random-employee-number)
     (eval . ,eval))))

(define (template-spec? item)
  (or
   (symbol? item)
   (and (list? item) (not (null? item)) (symbol? (car item)))))

(define (template-spec->random-data item)
  (let* ((is-list? (list? item))
	 (proc (hash-table-ref
		*random-generators*
		(if is-list? (car item) item)
		(lambda ()
		  (error "invalid template spec")))))
    (if is-list?
	(apply proc (cdr item))
	(proc))))

(define (generate-body template)
  (cond ((and (list? template) (not (null? template)) (eq? (car template) 'array))
	  (list->vector
	   (let loop ((l '())
		      (count (cadr template)))
	     (cond
	      ((<= count 0) l)
	      (else (loop (cons (generate-body (caddr template)) l) (- count 1)))))))
	((template-spec? template) (template-spec->random-data template))
	((and (list? template) (not (null? template)) (pair? (car template)))
	 (alist->hash-table
	  (map (lambda (element)
		 (let* ((k (car element))
			(v (cadr element)))
		   (cons k
			 (generate-body v))))
	       template)))
	(else template)))

(export random-numeric-string random-alphabetic-string random-alphanumeric-string
	random-first-name random-last-name random-name-fn-ln random-name-ln-fn
	random-city random-state random-zip random-phone random-picker
	generate-body)
