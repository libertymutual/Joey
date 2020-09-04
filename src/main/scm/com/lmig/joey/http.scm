(module-name http)

(import
 (srfi 1)
 (srfi 69)
 (class org.apache.http.client HttpClient)
 (class org.apache.http.impl.client CloseableHttpClient HttpClients)
 (class org.apache.http.ssl SSLContexts)
 (class org.apache.http.conn.ssl SSLConnectionSocketFactory TrustSelfSignedStrategy)
 (class org.apache.http.entity StringEntity)
 (class org.apache.http.util EntityUtils)
 (class org.apache.http.client.methods HttpPost HttpGet HttpPatch HttpDelete HttpPut)
 (class org.apache.http HttpResponse HttpEntity)
 (class org.json JSONObject JSONArray JSONTokener)
 (class javax.net.ssl SSLContext)
 (class java.nio.charset StandardCharsets Charset))

(define (get-http-client)::HttpClient
  (let* ((sslctx (*:build (*:loadTrustMaterial (SSLContexts:custom) #!null (TrustSelfSignedStrategy))))
	 (sslsf (SSLConnectionSocketFactory sslctx
					    (java.lang.String[] "TLSv1" "TLSv1.2")
					    #!null
					    (SSLConnectionSocketFactory:getDefaultHostnameVerifier)))
	 (httpcl (*:build (*:setSSLSocketFactory (HttpClients:custom) sslsf))))
    httpcl))

(define *http-client* (get-http-client))

(define +token-prefix+ "Bearer ")

(define +method-classes+
  (alist->hash-table
   `((get . ,HttpGet)
     (post . ,HttpPost)
     (put . ,HttpPut)
     (delete . ,HttpDelete)
     (patch . ,HttpPatch))))

(define (get-method-class method-name)
  (hash-table-ref +method-classes+ method-name (lambda () (error "invalid HTTP method name"))))

(define (http-send-json/authenticate client method uri token . optional-obj)
  (let* ((str (if (null? optional-obj) "" (*:toString (car optional-obj))))
	 (ent (StringEntity str))
	 (cls (get-method-class method))
	 (method-obj (cls uri)))
    (if token
	(*:addHeader method-obj "Authorization"
		     (string-append
		      +token-prefix+
		      token)))
    (if (member method '(post put patch))
	(begin
	  (*:addHeader method-obj "Content-Type" "application/json")
	  (*:addHeader method-obj "Accept" "application/json")
	  (*:setEntity method-obj ent)))
    (let* ((response (as HttpResponse (*:execute client method-obj)))
	   (status (*:getStatusCode (*:getStatusLine response)))
	   (body (EntityUtils:toString (as HttpEntity (HttpResponse:getEntity response)))))
      (cons status body))))

(define (http-send-json client method uri . optional-obj)
  (apply http-send-json/authenticate `(,client ,method ,uri #f . ,optional-obj)))

(define (json->scheme obj)
  (cond
   ((instance? obj JSONObject)
    (let ((hash (make-hash-table)))
      (for-each (lambda (x)
		  (hash-table-set! hash (string->symbol x) (json->scheme (JSONObject:get obj x))))
		(JSONObject:keySet obj))
      hash))
   ((instance? obj JSONArray)
    (let loop ((i 0)
	       (l '()))
      (cond
       ((>= i (JSONArray:length obj))
	(list->vector (reverse! l)))
       (else (loop (+ i 1) (cons (json->scheme (JSONArray:get obj i)) l))))))
   ((eq? obj #!null) '())
   ((java.lang.String? obj) (as string obj))
   ((java.lang.Double? obj) (as double obj))
   (else obj)))

(define (alist? obj)
  (and (list? obj)
       (every pair? obj)))

(define (scheme->json obj)
  (cond ((hash-table? obj)
	 (let ((newobj (JSONObject)))
	   (for-each (lambda (x)
		       (let ((y
			      (cond ((string? x) x)
				    ((symbol? x) (symbol->string x))
				    (else (error "must be a string or symbol:" x)))))
			 (JSONObject:put newobj
					 (as java.lang.String y)
					 (as java.lang.Object (scheme->json (hash-table-ref obj x))))))
		     (hash-table-keys obj))
	   newobj))
	((vector? obj)
	 (let ((newobj (JSONArray)))
	   (let loop ((i 0))
	     (cond ((>= i (vector-length obj)) newobj)
		   (else (JSONArray:put newobj (scheme->json (vector-ref obj i)))
			 (loop (+ i 1)))))))
	((string? obj) (as java.lang.String obj))
	((real? obj) (as java.lang.Double obj))
	((boolean? obj) (as java.lang.Boolean obj))
	((null? obj) #!null)
	((alist? obj) (scheme->json (alist->hash-table obj)))
	(else (error "invalid object type for JSON:" obj))))

(define response-status car)
(define response-body/string cdr)
(define (response-body/object response)
  (let* ((tok (JSONTokener (response-body/string response))))
    (json->scheme (JSONTokener:nextValue tok))))

(define (url-encode str)
  (java.net.URLEncoder:encode str (Charset:name StandardCharsets:UTF_8)))
