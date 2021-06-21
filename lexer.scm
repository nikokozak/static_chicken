;(module lexer (*)

(import scheme)
(import (chicken base))
(import (chicken format))
(import fs-utils)
(import records)

(define (capitalize-symbol symb)
  (string->symbol (list->string (map char-upcase (string->list (symbol->string symb))))))

(define (downcase-symbol symb)
  (string->symbol (list->string (map char-downcase (string->list (symbol->string symb))))))

(define-for-syntax (accessors-for name struct fields)
  (cond
   ((null? fields) '())
   (else
    (cons
     `(define ,(symbol-append name '- (car fields))
        (getter-with-setter (record-accessor ,struct ',(car fields))
                            (record-modifier ,struct ',(car fields))))
     (accessors-for name struct (cdr fields))))))

;; Defines a new #Record
;; eg. (define rec (Test a b ...))
;; eg. (test-a rec) => a
;; eg. (test-record? rec) => #t
;; eg. (set! (test-a rec) 10) => 10
(define-syntax make-record
  (er-macro-transformer
   (lambda (expr rename compare?)
     (let ((name (cadr expr))
           (upcase (capitalize-symbol (cadr expr)))
           (downcase (downcase-symbol (cadr expr)))
           (fields (cddr expr)))

       `(begin
          (define ,upcase (make-record-type ',downcase '(,@fields)))
          (define ,name (record-constructor ,upcase))
          (define ,(symbol-append downcase '-record?) (record-predicate ,upcase))
          ,@(accessors-for downcase upcase fields)
          (define-record-printer (',downcase x out)
            (fprintf out "#(,downcase ~s ~s ~)"))))))) ;; NOTE: FINISH THIS



#;(define-syntax make-record
  (syntax-rules ()
    ((_ name field1 field2 ...)
     (begin
       (define ,(up name)
         (make-record-type (down name) (list field1 field2 ...)))
       (define name
         (record-constructor (up name)))
       (define ,(symbol-append (down name) '?))
         (record-predicate (up name)))
       (define ,(symbol-append (down name) '- field1)
         (getter-with-setter (record-accessor (up name) field1)
                             (record-mutator (up name) field1)))
       ...)))


;; "Tokens"
(define-record-type dir
  (make-dir name abs-path rel-path contents)
  dir-node?
  (name dir-name dir-name-set!)
  (abs-path dir-abs-path dir-abs-path-set!)
  (rel-path dir-rel-path dir-rel-path-set!)
  (contents dir-contents dir-contents-set!))

(define-record-type img
  (make-img name type abs-path rel-path width height uid)
  img-node?
  (name img-name img-name-set!)
  (type img-type img-type-set!)
  (abs-path dir-abs-path dir-abs-path-set!)
  (rel-path dir-rel-path dir-rel-path-set!)
  (width img-width img-width-set!)
  (height img-height img-height-set!)
  (uid img-uid img-uid-set!))

(set-record-printer! dir (lambda (x out)
                          (fprintf out "(#dir ~S ~S ~S ~S)"
                                    (dir-name x)
                                    (dir-abs-path x)
                                    (dir-rel-path x)
                                    (dir-contents x))))

(define-record file name type path)

;; Returns a tree of dirs, including files and subdirs.
(define (tree path)
  (letrec
      ((helper (lambda (dirs)
                    (cond
                    ((null? dirs) '())
                    (else
                      (let ((dir-name (path-end (car dirs)))
                            (dir-path (car dirs))
                            (files (dir-files (car dirs)))
                            (other-dirs (cdr dirs)))
                        (cons
                          (make-dir
                            dir-name
                            dir-path
                            (helper (dir-dirs dir-path #t)) ;; recurr
                            files)
                          (helper other-dirs))))))))
    (helper (dir-dirs path #t))))


;)
