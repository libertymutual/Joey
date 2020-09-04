(module-name jdbc)
(import
 (class java.sql Connection DriverManager Statement PreparedStatement ResultSet)
 (com lmig joey config))

(define (open-jdbc)
  (DriverManager:getConnection
   (config-ref 'jdbc-url)
   (config-ref 'jdbc-username)
   (config-ref 'jdbc-password)))

(define (close-jdbc conn)
  (Connection:close conn))

(define (run-sql* conn sql . rest)
  (let* ((stmt (Connection:prepareStatement conn sql))
	 (rs
	  (let loop ((i 1)
		     (params rest))
	    (cond
	     ((null? params) (PreparedStatement:executeQuery stmt))
	     (else
	      (PreparedStatement:setObject stmt i (car params))
	      (loop (+ i 1) (cdr params)))))))
    (ResultSet:first rs)
    rs))

(define (run-sql sql . rest)
  (let* ((conn #f))
    (dynamic-wind
	(lambda () (set! conn (open-jdbc)))
	(lambda () (apply run-sql* (cons conn (cons sql rest))))
	(lambda () (close-jdbc conn)))))

(define (result-set-ref rs field-name)
  (ResultSet:getObject rs (string-upcase (symbol->string field-name))))

(export run-sql result-set-ref)
