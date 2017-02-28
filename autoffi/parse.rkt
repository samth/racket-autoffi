#lang racket

(require json data/ddict)

(provide load-catalog
         catalog
         catalog?
         catalog-types
         catalog-export-hash
         empty-catalog
         find-export-with-type
         
         type
         cyclic-record-type?
         type?
         primitive-type
         primitive-type?
         primitive-type-id
         function-type
         function-type?
         function-type-result-type
         function-type-param-types
         record-type?
         record-type-fields
         record-type-field-count
         struct-type
         struct-type?
         union-type
         union-type?
         pointer-type?
         enum-type
         enum-type?
         enum-type-values
         pointer-type
         array-type?
         fixed-array-type
         fixed-array-type?
         variadic-array-type
         variadic-array-type?)

(struct catalog (type-ddict export-hash))

(define (catalog-types c)
  (reverse (ddict-values (catalog-type-ddict c))))

(define (find-export-with-type c tt)
  (define h (catalog-export-hash c))
  (define candidates
    (filter (lambda (export-name)
              (eq? (hash-ref h export-name) tt))
            (hash-keys h)))
  (and (> (length candidates) 0) (first candidates)))

(struct type ())
(struct primitive-type type (id))
(struct function-type type ([result-type #:mutable] [param-types #:mutable]))
(struct record-type type (fields))
(struct struct-type record-type ())
(struct enum-type type (values))
(struct union-type record-type ())
(struct array-type type ([element-type #:mutable]))
(struct fixed-array-type array-type (element-count))
(struct variadic-array-type array-type ())
(struct pointer-type type ([referenced-type #:mutable]))
(struct block-pointer-type pointer-type ())

(define (cyclic-record-type? rt)
  (define (has-rt? tt)
    (or (eq? tt rt)
        (cond
          ((function-type? tt)
           (or  (has-rt? (function-type-result-type tt))
                (ormap has-rt? (function-type-param-types tt))))
          ((or (enum-type? tt)
               (primitive-type? tt))
           #f)
          ((array-type? tt)
           (has-rt? (array-type-element-type tt)))
          ((pointer-type? tt)
           (has-rt? (pointer-type-referenced-type tt))))))
  (ormap has-rt? (hash-values (record-type-fields rt))))

(define (record-type-field-count rt)
  (hash-count (record-type-fields rt)))

(define (add-record-field! rt field-name field-t)
  (hash-set! (record-type-fields rt) field-name field-t ))

(define (load-catalog name)
  (parse-catalog (read-json (open-input-file (string-append name ".json")))))

(define empty-catalog (catalog (ddict) (make-hash)))

(define (hash-get h k)
  (and (hash-has-key? h k)
       (hash-ref h k)))

(define (parse-catalog json-like)

  (define type-list (hash-ref json-like 'type))
  (define exports (hash-ref json-like 'export))

  #|(define pre-type-ddict|#
    #|(foldl (lambda (type-spec h)|#
             #|(define type-id (hash-ref type-spec 'id))|#
             #|(hash-set h type-id type-spec))|#
           #|(hash)|#
           #|type-list))|#

  (define ffi-types (mutable-ddict))

  (let loop

    ((type-list type-list))

    (when (not (null? type-list))

      (define type-spec (car type-list))

      (define type-id (hash-ref type-spec 'id))

      (define (get-type-by-id id)
        (ddict-ref ffi-types id))

      (define (add-fields rt type-spec)
        (for-each (lambda (field-spec)
                   (define name (hash-ref field-spec 'name))
                   (define type-id (hash-ref field-spec 'type))
                   (add-record-field! rt name (get-type-by-id type-id)))
               (or (hash-get type-spec 'field) '())))

      (define (get-enum-values type-spec)
        (foldl (lambda (value-spec h)
                 (define name (hash-ref value-spec 'name))
                 (define value (or (hash-get value-spec 'value) 0))
                 (hash-set h name value))
               (hash)
               (or (hash-get type-spec 'value) '())))

      (case (hash-ref type-spec 'kind)
        ; a function type's parameters and return value cannot circularly
        ; reference the function type itself, so we're safe
        (("FUNCTION") 
         (define f (function-type #f '()))
         (ddict-set! ffi-types type-id  f)
         (loop (cdr type-list))
         (set-function-type-result-type! f (get-type-by-id (hash-ref type-spec 'returnType)))
         (set-function-type-param-types! f (map get-type-by-id (or (hash-get type-spec 'paramType) '()))))
        (("POINTER")
         (if (hash-get type-spec 'isBlockPointer)
             (let ((b (block-pointer-type #f)))
               (ddict-set! ffi-types type-id b)
               (loop (cdr type-list))
               (set-pointer-type-referenced-type! b (get-type-by-id (hash-ref type-spec 'referencedType))))
             (let ((p (pointer-type #f)))
               (ddict-set! ffi-types type-id p)
               (loop (cdr type-list))
               (set-pointer-type-referenced-type! p (get-type-by-id (hash-ref type-spec 'referencedType))))))
        (("PRIMITIVE")
         (define primitive-type-kind (hash-ref type-spec 'primitiveKind))
         (ddict-set! ffi-types type-id
           (primitive-type 
             (case primitive-type-kind
               (("Void") 'void)
               (("Char_U" "Char_S") 'char)
               (("SChar") 'schar)
               (("UChar") 'uchar)
               (("short") 'short)
               (("SShort") 'sshort)
               (("UShort") 'ushort)
               (("Int") 'int)
               (("Sint") 'sint)
               (("UInt") 'uint)
               (("long") 'long)
               (("ULong") 'ulong)
               (("SLong") 'slong)
               (("LongLong") 'llong)
               (("ULongLong") 'ullong)
               (("SLongLong") 'sllong)
               (("Float") 'float)
               (("Double") 'double)
               (("LongDouble") 'ldouble)
               (("Bool") 'stdbool)
               (else (error 'parse-catalog "could not determine the kind of primitive type for ~a: ~a" type-id primitive-type-kind)))))
         (loop (cdr type-list)))
        (("STRUCT")
         (define s (struct-type (make-hash)))
         (ddict-set! ffi-types type-id s)
         (loop (cdr type-list))
         (add-fields s type-spec))
        (("UNION")
         (define u (union-type (make-hash)))
         (ddict-set! ffi-types type-id u)
         (loop (cdr type-list)))
        (("ENUM")
         (ddict-set! ffi-types type-id
           (enum-type (get-enum-values type-spec)))
         (loop (cdr type-list)))
        (("ARRAY")
         (if (hash-has-key? type-spec 'count)
             (let ((f (fixed-array-type #f)))
               (ddict-set! ffi-types type-id f (hash-ref type-spec 'count))
               (loop (cdr type-list))
               (set-array-type-element-type! f (get-type-by-id (hash-ref type-spec 'elementType))))
             (let ((v (variadic-array-type #f)))
               (ddict-set! ffi-types type-id v)
               (loop (cdr type-list))
               (set-array-type-element-type! v (get-type-by-id (hash-ref type-spec 'elementType)))))
         (loop (cdr type-list)))
        ; qualified types in and by itself reference nothing
        (("QUALIFIED")
         (ddict-set! ffi-types type-id (get-type-by-id (hash-ref type-spec 'underlyingType)))
         (loop (cdr type-list)))
        (else (error 'parse-catalog "could not parse type spec for ~a: ~a" type-id (hash-ref type-spec 'kind))))))

  (define export-hash
    (foldl (lambda (export-spec h)
             ; FIXME: the compiler shouldn't add nameless exports at all
             (if (hash-has-key? export-spec 'name)
               (let ((export-name (hash-ref export-spec 'name))
                     (export-type-id (hash-ref export-spec 'type)))
                 (hash-set h export-name (ddict-ref ffi-types export-type-id)))
               h))
           (hash)
           exports))

  (catalog ffi-types export-hash))

