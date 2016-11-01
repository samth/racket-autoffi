#lang racket

(require ffi/unsafe
         (for-syntax "parse.rkt"))

(provide require/foreign
         (all-from-out ffi/unsafe))

(define-syntax (require/foreign stx)
  (syntax-case stx ()
    [(_ name)
     (string? (syntax-e #'name))
     (let ((exports (load-catalog (syntax-e #'name))))
       (define (transform-type tt)
         (cond
           ((function-type? tt)
            #`(_cprocedure (list #,@(map transform-type
                                         (function-type-param-types tt)))
                           #,(transform-type (function-type-result-type tt))))
           ((primitive-type? tt)
            (define kind-id (primitive-type-id tt))
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
             ((void) #'_void)
             (else (error 'transit-type->ctype "could not convert primitive type: unrecorgnised id ~a" kind-id))))
           (else (error 'require/foreign "unknown transit type ~a" tt))))
      (with-syntax ([proc-names (datum->syntax #'name (map string->symbol (hash-keys exports)))]
                    [proc-defs
                      (datum->syntax #'name (map (lambda (proc-name)
                             #`(get-ffi-obj #,proc-name lib
                                            #,(transform-type
                                                (hash-ref exports proc-name))))
                                      (hash-keys exports)))])
        #'(define-values proc-names
          (let ((lib (ffi-lib name)))
            (values . proc-defs)))))]))

