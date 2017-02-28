#lang racket

(require ffi/unsafe
         (for-syntax 
           racket/syntax
           racket/list
           syntax/srcloc
           "parse.rkt"))

(provide require/foreign
         (all-from-out ffi/unsafe))

(begin-for-syntax

  (define (tt-prim-kind->ffi-prim-kind kind-id)
    (case kind-id
       ((char) #'_int8)
       ((schar) #'_sint8)
       ((uchar) #'_uint8)
       ((short) #'_int16)
       ((sshort) #'_sint16)
       ((ushort) #'_uint16)
       ((int) #'_int32)
       ((sint) #'_sint32)
       ((uint) #'_uint32)
       ((long) #'_long)
       ((ulong) #'_ulong)
       ((slong) #'_slong)
       ((llong) #'_llong)
       ((ullong) #'_ullong)
       ((sllong) #'_sllong)
       ((bool) #'_stdbool)
       ((float) #'_float)
       ((double) #'_double)
       ((ldouble) #'_longdouble)
       ((void) #'_void)
       (else (error 'transit-type->ctype "could not convert primitive type: unrecorgnised id ~a" kind-id))))
  
  (define (dirname path) 
    (let-values ([(base name must-be-dir?) (split-path path)])
      base))

  )

(define-syntax (cstruct-alias stx)
  (syntax-case stx ()
    [(_ name orig (field-name ...))
     #`(begin
         #,@(map (lambda (field-name) 
                   #`(define #,(format-id field-name "~a-~a" (substring (syntax-e #'name) 1) (syntax-e field-name))
                             #,(format-id field-name "~a-~a" (substring (symbol->string (syntax-e #'orig)) 1) (syntax-e field-name)))) 
                 (syntax-e #'(field-name ...)))

         'done)]))

(define-syntax (require/foreign stx)
  (syntax-case stx ()
    [(_ name)
     (string? (syntax-e #'name))
     (let* ((catalog (load-catalog (string-append (path->string (dirname (source-location-source #'name))) (syntax-e #'name))))
            (export-hash (catalog-export-hash catalog))
            (types (remove-duplicates (catalog-types catalog))))

       (define function-export-names (filter (lambda (proc-name) (function-type? (hash-ref export-hash proc-name))) (hash-keys export-hash)))

       (define transformed (make-hash))

       (define (get-enum-symbols h) 
         (define keys (hash-keys h))
         (let loop
           ((symbols (hash-keys h)))
           (if (null? symbols)
              '()
              (list* (string->symbol (car symbols)) '= (hash-ref h (car symbols)) (loop (cdr symbols))))))

       (define (generate-id)
         (datum->syntax #'name (syntax-e (format-id #'name "_~a" (generate-temporary)))))

       (define (transform-type tt)
         (cond
           ((primitive-type? tt)
            (define id (generate-id))
            (hash-set! transformed tt id)
            #`(define #,id #,(tt-prim-kind->ffi-prim-kind (primitive-type-id tt))))
           ((function-type? tt)
            (define id (generate-id))
            (hash-set! transformed tt id)
            #`(define #,id (_cprocedure (list #,@(map (lambda (param-type) (hash-ref transformed param-type)) (function-type-param-types tt)))
                           #,(hash-ref transformed (function-type-result-type tt)))))
           ((struct-type? tt)
            (if (and (not (cyclic-record-type? tt)) (> (record-type-field-count tt) 0))
              (let* ((export-name (find-export-with-type catalog tt))
                    (id (datum->syntax #'name (if export-name (string->symbol (string-append "_" export-name)) (generate-id))))
                    (fields (record-type-fields tt)))
                    (hash-set! transformed tt id)
                  #`(define-cstruct #,id
                            #,(map (lambda (field-name)
                                      (define field-type (hash-ref fields field-name))
                                      #`[#,(string->symbol field-name) #,(hash-ref transformed field-type)])
                                   (hash-keys fields))))
              (let ((id (generate-id)))
                (hash-set! transformed tt id)
                #`(define #,id (_cpointer '#,id)))))
           ((union-type? tt)
            (define id (generate-id))
            (hash-set! transformed tt id)
            #`(define #,id (_cpointer '#,id)))
           ((enum-type? tt)
            (define id (generate-id))
            (hash-set! transformed tt id)
            #`(define #,id (_enum '#,(get-enum-symbols (enum-type-values tt)))))
           ((array-type? tt)
            (define id (generate-id))
            (hash-set! transformed tt id)
            #`(define #,id _pointer))
           ((pointer-type? tt)
            (define id (generate-id))
            (hash-set! transformed tt id)
            ; TODO: make typed pointers
            #`(define #,id _pointer))
           (else (error 'require/foreign "unknown autoffi type ~a" tt))))
       
       (define ffi-lib-name (generate-temporary))
       (define type-defs (map transform-type types))
       (define proc-defs 
         (datum->syntax #'name (map (lambda (proc-name)
                                      (define proc-type (hash-ref export-hash proc-name))
                                      #`(get-ffi-obj #,proc-name #,ffi-lib-name #,(hash-ref transformed proc-type)))
                                    function-export-names)))

       (with-syntax ([proc-names (datum->syntax #'name (map string->symbol function-export-names))]
                     [image-path (datum->syntax #'name (string-append (path->string (dirname (source-location-source #'name))) (syntax-e #'name)))])
         #`(begin
             (define #,ffi-lib-name (ffi-lib image-path))
             #,@type-defs
             (define-values proc-names (values #,@proc-defs)))))]))

