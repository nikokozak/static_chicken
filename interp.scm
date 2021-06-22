;; Taking a tokenized tree, assigns templates to files, assigns parser

(import (chicken base))
(import fs-utils)
(import token)
(import lowdown)

(define temp-dir (path "tmp"))
(define templates-for '(("content" "content")))
(define parsers-for `(("content" ,markdown->sxml)))

;; Usage: (find-in-tree lowercase-node-type tree)
(define-syntax find-in-tree
  (er-macro-transformer
   (lambda (expr change compare?)
     (let ((tree (caddr expr))
           (thing (cadr expr)))
       `(letrec
            ((helper (lambda (nodes)
                       (cond
                        ((null? nodes) '())
                        ((,(symbol-append (cadr expr) '-token?) (car nodes))
                         (append (list (car nodes)) (helper (cdr nodes))))
                        ((dir-token? (car nodes))
                         (append (helper (dir-contents (car nodes)))
                                 (helper (cdr nodes))))
                        (else
                         (helper (cdr nodes)))))))
          (helper ,tree))))))

(define (get-tree-files tree)
  (find-in-tree file tree))

(define (get-tree-templates tree)
  (find-in-tree template tree))

(define (get-tree-directories tree)
  (find-in-tree dir tree))

(define (get-tree-images tree)
  (find-in-tree img tree))

;; Currently only creates a new tmp folder
(define (make-intermediates tree)
  (rmvrf temp-dir)
  (mkdir temp-dir))

(define (search-pairs key pairs)
  (cond
   ((null? pairs) '())
   ((string-ci=? key (caar pairs)) (cadar pairs))
   (else
    (search-pairs key (cdr pairs)))))

(define (template-for type)
  (search-pairs type templates-for))

(define (parser-for type)
  (search-pairs type parsers-for))

(define (parse-file-node node)
  (let* ((parser (parser-for (file-type node)))
         (parsed (if (not (null? parser))
                     (parser (open-input-file (file-abs-path node)))
                     '())))
    (if (null? parser)
        node
        (PFile
         (file-name node)
         (file-abs-path node)
         (file-rel-path node)
         (file-type node)
         (template-for (file-type node))
         parsed
         node))))

(define (parse tree)
  (cond
   ((null? tree) '())
   ((file-token? (car tree)) (cons (parse-file-node (car tree))
                                   (parse (cdr tree))))
   ((dir-token? (car tree)) (cons (Dir
                                   (dir-name (car tree))
                                   (dir-abs-path (car tree))
                                   (dir-rel-path (car tree))
                                   (parse (dir-contents (car tree)))
                                   (dir-image-dir? (car tree)))
                                  (parse (cdr tree))))
   (else
    (cons (car tree) (parse (cdr tree))))))

(make-token PFile
            name
            abs-path
            rel-path
            type
            template
            parsed
            original)
