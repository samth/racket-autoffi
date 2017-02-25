#lang racket

(require json data/ddict)

(provide load-catalog
         catalog
         catalog?
         catalog-types
         catalog-export-hash
         empty-catalog
         
         type
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
         struct-type
         struct-type?
         union-type
         union-type?
         pointer-type?
         enum-type
         enum-type?
         pointer-type
         array-type?
         fixed-array-type
         fixed-array-type?
         variadic-array-type
         variadic-array-type?)

(struct catalog (type-ddict export-hash))

(define (catalog-types c)
  (reverse (ddict-values (catalog-type-ddict c))))

(struct type ())
(struct primitive-type type (id))
(struct function-type type (result-type param-types));
(struct record-type type (fields))
(struct struct-type record-type ())
(struct union-type record-type ())
(struct array-type type (element-type))
(struct fixed-array-type array-type (element-count))
(struct variadic-array-type array-type ())
(struct pointer-type type (referenced-type))
(struct block-pointer-type pointer-type ())
(struct enum-type type (values))

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

  (define (type-folder type-spec dd)

    (define type-id (hash-ref type-spec 'id));

    (define (get-type-by-id id)
      (ddict-ref dd id))

    (define (get-fields type-spec)
      (foldl (lambda (field-spec h)
               (define name (hash-ref field-spec 'name))
               (define type-id (hash-ref field-spec 'type))
               (hash-set h name (get-type-by-id type-id)))
             (hash)
             (or (hash-get type-spec 'field) '())))

    (define parsed-type
     (case (hash-ref type-spec 'kind)
       (("FUNCTION") 
        (define result-type-id (hash-ref type-spec 'returnType))
        (define params (map get-type-by-id (hash-ref type-spec 'paramType)))
        (function-type (get-type-by-id result-type-id) params))
       (("POINTER")
        (if (hash-get type-spec 'isBlockPointer)
            (block-pointer-type (get-type-by-id (hash-ref type-spec 'referencedType)))
            (pointer-type (get-type-by-id (hash-ref type-spec 'referencedType)))))
       (("PRIMITIVE")
        (define primitive-type-kind (hash-ref type-spec 'primitiveKind))
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
       (("STRUCT")
        (struct-type (get-fields type-spec)))
       (("UNION")
        (union-type (get-fields type-spec)))
       (("ENUM")
        (enum-type (foldl (lambda (value-spec h)
                                  (define name (hash-ref value-spec 'name))
                                  (define value (hash-ref value-spec 'value))
                                  (hash-set h name value))
                                (hash)
                                (or (hash-get type-spec 'value) '()))))
       (("ARRAY")
        (if (hash-has-key? type-spec 'count)
            (fixed-array-type (get-type-by-id (hash-ref type-spec 'elementType))
                                    (hash-ref type-spec 'count))
            (variadic-array-type (get-type-by-id (hash-ref type-spec 'elementType)))))
       (("QUALIFIED")
        (get-type-by-id (hash-ref type-spec 'underlyingType))) ; drops qualifiers
       (else (error 'parse-catalog "could not parse type spec for ~a: ~a" type-id (hash-ref type-spec 'kind)))))
    (ddict-set dd type-id parsed-type))
  (define type-ddict
    (foldl type-folder (ddict) type-list))
  (define export-hash
    (foldl (lambda (export-spec h)
             (define export-name (hash-ref export-spec 'name))
             (define export-type-id (hash-ref export-spec 'type))
             (hash-set h export-name (ddict-ref type-ddict export-type-id)))
           (hash)
           exports))

  (catalog type-ddict export-hash))

