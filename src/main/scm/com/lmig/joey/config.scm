(module-name config)

(import
 (srfi 69)
 (class java.lang System))

(define *config*
  (make-hash-table))

(define *joey-prop-prefix* "joey.")

(define (symbol->prop-name sym)
  (string-append *joey-prop-prefix*
		 (string-map (lambda (c)
			       (if (or (char=? c #\-)
				       (char=? c #\_))
				   #\.
				   c))
			     (symbol->string sym))))

(define (config-ref key)
  (define (get-property-for-key key)
    (System:getProperty (symbol->prop-name key)))
  (let* ((val (hash-table-ref *config* key (lambda () (get-property-for-key key)))))
    (if (eq? val #!null)
	(error "no config value for: " key)
	val)))

(define (config-set! key value)
  (hash-table-set! *config* key value))

(define-syntax config
  (syntax-rules ()
    ((config (name value) ...)
     (begin
       (config-set! 'name value)
       ...))))

(export config config-ref)
