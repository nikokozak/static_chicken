(module token ((make-token *))

  (import scheme
          (chicken module)
          (chicken base)
          (chicken syntax))

  (reexport records
            (only (chicken format) fprintf))

  ;; ;; Takes a symbol and returns a fully capitalized version.
  ;; (define (capitalize-symbol symb)
  ;;   (string->symbol (list->string (map char-upcase (string->list (symbol->string symb))))))

  ;; ;; Takes a symbol and returns a downcase version.
  ;; (define (downcase-symbol symb)
  ;;   (string->symbol (list->string (map char-downcase (string->list (symbol->string symb))))))

  ;; Creates getter and setter templates for records.
  (define-for-syntax (accessor-defs-for name struct fields)
    (cond
    ((null? fields) '())
    (else
      (cons
      `(define ,(symbol-append name '- (car fields))
          (getter-with-setter (record-accessor ,struct ',(car fields))
                              (record-modifier ,struct ',(car fields))))
      (accessor-defs-for name struct (cdr fields))))))

  ;; Creates accessor templates for records. NOTE: the x in the accessor is hardcoded atm,
  ;; this is cheeky and should be changed pronto.
  (define-for-syntax (accessors-for name fields var)
    (cond
    ((null? fields) '())
    (else
      (cons
      `(,(symbol-append name '- (car fields)) x)
      (accessors-for name (cdr fields) var)))))

  ;; Creates format string template for print function
  (define-for-syntax (format-string name fields)
    (let ((field-length (length fields)))
      (letrec ((draw-s (lambda (fields-left)
                        (if (= 0 fields-left)
                            ""
                            (string-append "~s " (draw-s (sub1 fields-left)))))))
        (string-append "#(" (symbol->string name) " " (draw-s field-length) ")"))))

  ;; Defines a new #Record
  ;; eg. (make-token Test a b)
  ;; eg. (define rec (Test a b ...))
  ;; eg. (test-a rec) => a
  ;; eg. (test-record? rec) => #t
  ;; eg. (set! (test-a rec) 10) => 10
  ;; rec => #(test "a" "b")
  (define-syntax make-token
    (er-macro-transformer
    (lambda (expr replace compare?)
      (let* ((capitalize-symbol (lambda (symb)
                                  (string->symbol (list->string (map char-upcase (string->list (symbol->string symb)))))))
             (downcase-symbol (lambda (symb)
                                (string->symbol (list->string (map char-downcase (string->list (symbol->string symb)))))))
             (name (cadr expr))
             (upcase (capitalize-symbol (cadr expr)))
             (downcase (downcase-symbol (cadr expr)))
             (fields (cddr expr))
             (token-printer `(lambda (x out)
                               (fprintf out ,(format-string downcase fields)
                                        ,@(accessors-for downcase fields 'x)))))

        `(begin
            ;; (define TEST (make-record-type test '(field1 field2 ...)))
            (define ,upcase (make-record-type ',downcase '(,@fields)))
            ;; (define Test (record-constructor TEST))
            (define ,name (record-constructor ,upcase))
            ;; (define test-token? (record-predicate TEST))
            (define ,(symbol-append downcase '-token?) (record-predicate ,upcase))
            ;; (define test-field1 (getter-with-setter TEST (record-accessor TEST 'test-field1) ...))
            ,@(accessor-defs-for downcase upcase fields)
            (set-record-printer! ',downcase ,token-printer))))))

  ;; "Tokens"
  ;; (make-token Dir
  ;;             name
  ;;             abs-path
  ;;             rel-path
  ;;             contents)

  ;; (make-token File
  ;;             name
  ;;             type
  ;;             abs-path
  ;;             rel-path)

  ;; (make-token Img
  ;;             name
  ;;             type
  ;;             abs-path
  ;;             rel-path
  ;;             width
  ;;             height
  ;;             uid)
)
