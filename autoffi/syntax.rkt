#lang racket

(require ffi/unsafe
         (for-syntax racket/syntax)
         (for-syntax "parse.rkt"))

(provide require/foreign
         (all-from-out ffi/unsafe))

(define-for-syntax (tt-prim-kind->ffi-prim-kind kind-id)
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

(define-syntax (require/foreign stx)
  (syntax-case stx ()
    [(_ name)
     (string? (syntax-e #'name))
     (let* ((catalog (load-catalog (syntax-e #'name)))
            (export-hash (catalog-export-hash catalog))
            (types (catalog-types catalog)))
       (define function-export-names (filter (lambda (proc-name) (function-type? (hash-ref export-hash proc-name))) (hash-keys export-hash)))
       (define transformed (make-hash))
       (define (transform-type tt)
         (define id (datum->syntax #'name (syntax-e (format-id #'name "_~a" (generate-temporary)))))
         (display tt)(newline)
         (hash-set! transformed tt id)
         (cond
           ((primitive-type? tt)
            #`(define #,id #,(tt-prim-kind->ffi-prim-kind (primitive-type-id tt))))
           ((function-type? tt)
            #`(define #,id (_cprocedure (list #,@(map (lambda (param-type) (hash-ref transformed param-type)) (function-type-param-types tt)))
                           #,(hash-ref transformed (function-type-result-type tt)))))
           ((struct-type? tt)
            (define fields (record-type-fields tt))
            (if (= (hash-count fields) 0)
                #`(define #,id _pointer)
                #`(define-cstruct #,id
                          #,(map (lambda (field-name)
                                    (define field-type (hash-ref fields field-name))
                                    #`[#,(string->symbol field-name) #,(hash-ref transformed field-type)])
                              (hash-keys fields)))));
           ((union-type? tt)
            #`(define #,id _pointer))
           ((enum-type? tt)
            #|#'(_enum #,(|#
            'TODO)
           ((array-type? tt)
            #`(define #,id _pointer))
           ((pointer-type? tt)
            ; TODO: make typed pointers
            #`(define #,id _pointer))
           (else (error 'require/foreign "unknown transit type ~a" tt))))
      (display types)
    (let ((type-defs (map transform-type types)))
      (with-syntax ([proc-names (datum->syntax #'name (map string->symbol function-export-names))]
                    [proc-defs 
                      (datum->syntax #'name (map (lambda (proc-name)
                                                   (define proc-type (hash-ref export-hash proc-name))
                                                   #`(get-ffi-obj #,proc-name lib #,(hash-ref transformed proc-type)))
                                function-export-names))])
        #`(define-values proc-names
          (let ((lib (ffi-lib name)))
            #,@type-defs
            (values . proc-defs))))))]))

