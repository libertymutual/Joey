(module-name joey)

(import
 (srfi 69)
 (com lmig joey random-data)
 (com lmig joey http)
 (com lmig joey jdbc)
 (com lmig joey salesforce))

(define-record-type test-result
  (make-test-result pass? expected actual exception)
  test-result?
  (pass? test-result-pass?)
  (expected test-result-expected)
  (actual test-result-actual)
  (exception test-result-exception))

(define (test-pass)
  (make-test-result #t '() '() '()))

(define (test-fail e a)
  (make-test-result #f e a '())) 
	 
(define-macro (with-input inp form)
  `(let ((input ,inp)) ,form))

(define-syntax interpret-clause
  (lambda (form)
    (syntax-case form (scheme response status db salesforce)
      ((interpret-clause (scheme form1 ...) url body resp)
		    #'(with-input body '(begin form1 ...)))
      ((interpret-clause (status status-val) url body resp)
       #'(let* ((sv status-val)
		(input body))
	   (if (if (procedure? sv)
		   (sv (response-status resp))
		   (= (response-status resp) sv))
	       (make-test-result #t '() '() '())
	       (make-test-result #f sv (response-status resp) '()))))
      ((interpret-clause (response (k1 v1) ...) url body resp)
		    #'(let ((output (response-body/object resp)))
			(call/cc (lambda (k)
				   (for-each (lambda (x)
					       (if (not
						    (if (procedure? (cdr x))
							((cdr x) (hash-table-ref output (car x)))
							(equal? (hash-table-ref output (car x)) (cdr x))))
						   (k (make-test-result
						       #f
						       (cdr x)
						       (hash-table-ref output (car x))
						       '()))))
					     (list `(k1 . ,(with-input body 'v1)) ...))
				   (make-test-result #t '() '() '())))))
      ((interpret-clause (salesforce (soql param1 ...) ((k1 v1) ...)) url body resp)
       
       #'(let ((output (salesforce-run-query soql (with-input body 'param1) ...)))
	   (call/cc (lambda (k)
		      (for-each (lambda (x)
				  (if (not
				       (if (procedure? (cdr x))
					   ((cdr x) (salesforce-hash-table-ref output (car x)))
					   (equal? (salesforce-hash-table-ref output (car x)) (cdr x))))
				      (k (make-test-result
					  #f
					  (cdr x)
					  (salesforce-hash-table-ref output (car x))
					  '()))))
				(list `(k1 . ,(with-input body 'v1)) ...))
		      (make-test-result #t '() '() '())))))
      ((interpret-clause (db (sql param1 ...) ((k1 v1) ...)) url body resp)
       #'(let ((output (run-sql sql param1 ...)))
	   (call/cc (lambda (k)
		      (for-each (lambda (x)
				  (if (not
				       (if (procedure? (cdr x))
					   ((cdr x) (hash-table-ref output (car x)))
					   (equal? (hash-table-ref output (car x)) (cdr x))))
				      (k (make-test-result
					  #f
					  (cdr x)
					  (result-set-ref output (car x))
					  '()))))
				(list `(k1 . ,(with-input body 'v1)) ...))
		      (make-test-result #t '() '() '()))))))))

(define (report-result descriptor method url result)
  (display method)
  (display " ")
  (display url)
  (newline)
  (display descriptor)
  (if (test-result? result)
      (if (test-result-pass? result)
	  (display " - PASS\n")
	  (begin
	    (display " - FAIL\n")
	    (if (not (null? (test-result-exception result)))
		(begin
		  (display "    exception: ")
		  (display (test-result-exception result))
		  (newline)
		  (java.lang.Exception:printStackTrace (test-result-exception result)))
		(begin
		  (display "    expected ")
		  (if (procedure? (test-result-expected result))
		      (display " to satisfy "))
		  (display (test-result-expected result))
		  (display ", got ")
		  (display (test-result-actual result))))))
      (display " - N/A"))
  (newline))

(define-syntax run-test
  (syntax-rules ()
    ((run-test method url body (clause1 ...) ...)
     (begin
       (guard (condition (else (make-test-result #f '() '() condition)))
	      (let* ((real-body
		      (generate-body `body))
		     (resp (if (null? real-body) (http-send-json *http-client* 'method url)
			       (http-send-json *http-client* 'method url (scheme->json real-body)))))
		(call/cc
		 (lambda (k)
		   (begin
		     (let ((result (interpret-clause (clause1 ...) url real-body resp)))
		       (if (and (test-result? result) (not (test-result-pass? result)))
			   (k result)))
		     ...)
		   (make-test-result #t '() '() '())))))))))

(define (passed? test)
  (or
   (not (test-result? test))
   (test-result-pass? test)))

(define (failed? test)
  (not (passed? test)))

(define-syntax
   do-tests
   (syntax-rules ()
     ((do-tests
       (descriptor method url body (clause1 ...) ...) ...)
      (let*
	  ((results '()))
	(begin
	  (let* ((result (run-test method url body (clause1 ...) ...)))
	    (report-result descriptor 'method url result) (set! results (cons result results)))
	  ...)
	(let ((npassed (length (filter passed? results)))
	      (nfailed (length (filter failed? results))))
	(display npassed)
	(display " passed, ")
	(display nfailed)
	(display " failed")
	(newline)
	(values npassed nfailed))))))
