#lang racket

(require json)

(provide load-catalog
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
         struct-type
         struct-type?
         union-type
         union-type?
         array-type?
         fixed-array-type
         fixed-array-type?
         variadic-array-type
         variadic-array-type?)


(struct type ())
(struct primitive-type type (id))
(struct function-type type (result-type param-types));
(struct record-type type (fields))
(struct struct-type record-type ())
(struct union-type record-type ())
(struct array-type type (element-type))
(struct fixed-array-type array-type (element-count))
(struct variadic-array-type array-type ())

(define (load-catalog name)
  (parse-catalog (read-json (open-input-file (string-append name ".json")))))

(define empty-catalog (hash))

(define (parse-catalog json-like)
  (define type-list (hash-ref json-like 'types))
  (define exports (hash-ref json-like 'exports))
  (define pre-type-hash
    (foldl (lambda (type-spec h)
             (define type-id (hash-ref type-spec 'id))
             (hash-set h type-id type-spec))
           (hash)
           type-list))
  (define type-hash (make-hash))
  (define (parse-type type-id)
    (define type-spec (hash-ref pre-type-hash type-id))
    (when (not (hash-has-key? type-hash type-id))
      (define parsed-type
       (case (hash-ref type-spec 'kind)
         (("function") 
          (define result-type-id (hash-ref type-spec 'returnType))
          (define params (map parse-type (hash-ref type-spec 'paramTypes)))
          (function-type (parse-type result-type-id) params))
         (("primitive")
          (primitive-type
            (case (hash-ref type-spec 'name)
              (("void") 'void)
              (("char") 'char)
              (("signed char") 'schar)
              (("unsigned char") 'uchar)
              (("short") 'short)
              (("signed short") 'sshort)
              (("unsigned short") 'ushort)
              (("int") 'int)
              (("signed int") 'sint)
              (("unsigned int") 'uint)
              (("long") 'long)
              (("unsigned long") 'ulong)
              (("signed long") 'slong)
              (("long long") 'llong)
              (("unsigned long long") 'ullong)
              (("signed long long") 'sllong)
              (("bool") 'stdbool)
              (else (error 'parse-catalog "could not determine the kind of primitive type for ~a" type-id)))))
         ((fixed-array) 'todo)
         ((fixed-array) 'todo)
         ((fixed-array) 'todo)
         ((fixed-array) 'todo)
         (else (error 'parse-catalog "could not parse type spec for ~a" type-id))))
     (hash-set! type-hash type-id parsed-type))
    (hash-ref type-hash type-id))
  (foldl (lambda (export-spec h)
           (define export-name (hash-ref export-spec 'name))
           (define export-type-id (hash-ref export-spec 'type))
           (hash-set h export-name (parse-type export-type-id)))
         (hash)
         exports))

