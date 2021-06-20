;;(module indexer (make-dir-tree)

  ;;(import scheme)
(import (chicken base))
(import (chicken format))
(import fs-utils)



(define (_capitalize str)
  (let ((chars (string->list str)))
    (list->string
     (cons
      (char-upcase (car chars))
      (cdr chars)))))

(define (_strip-extensions file-list)
  (if (null? file-list)
      '()
      (cons (strip-extension (car file-list)) (strip-extensions (cdr file-list)))))

(define (make-traversal tree)
  (lambda (fun)
    (letrec ((helper (lambda (tree-so-far)
                       (cond
                        ((null? tree-so-far) '())
                        (else
                         (cons (fun (car tree))
                               (helper (cdr tree))))))))
      (helper tree))))

;; (define (make-nav-tree dir-tree)
;;   (letrec ((helper (lambda (dirs)
;;                      (cond
;;                       ((null? dirs) '())
;;                       (else
;;                        (let ((current (car dirs))
;;                              (name (_capitalize (dir-name (car dirs))))
;;                              (files (strip-extensions (dir-contents/files (car dirs))))
;;                              (subdirs (dir-contents/dir (car dirs)))
;;                              (rest (cdr dirs)))
;;                           (cons
;;                            (make-nav-node
;;                             name
;;                             "NA"
;;                             (append files (helper subdirs))
;;                             (dir-path current)
;;                             (dir-name current))
;;                           (helper rest))))))))
;;     (helper dir-tree)))

(define-record node label type children abs-path rel-path images)
(set-record-printer! node (lambda (x out)
                                (fprintf out "(#nav label: ~S type: ~S children: ~S path: ~S filename: ~S)"
                                         (node-label x)
                                         (node-type x)
                                         (node-children x)
                                         (node-path x)
                                         (node-filename x))))

(define-record image label abs-path rel-path uid-name)
(set-record-printer! image (lambda (x out)
                             (fprintf out "(#img label: ~S abs-path: ~S rel-path: ~S uid: ~S)"
                                      (image-label x)
                                      (image-abs-path x)
                                      (image-rel-path x)
                                      (image-uid-name x))))

(define (make-intermediate-tree dir-tree)
  (letrec ((traverser (make-traversal dir-tree))
           (dir-tree-converter (traverser (lambda (d)
                                            (make-node
                                             (dir-name d)
                                             (assign-type d)
                                             (dir-tree-converter )
                                             (dir-path d)
                                             (dir-path d)
                                             '())))))
    )
  ((make-traversal dir-tree)
   (lambda (d)
     (make-node
      (dir-name d)
      (assign-type d)
      (dir-path d)
      (dir-path d) ; make abs->rel path converter
      '() ; make image extractor
      )
     )))

(define (assign-type dir-node)
  (cond
   ((equal? (dir-name dir-node) "home") "home-node")
   ((and (_has-only-one-file dir-node) (_has-only-image-subfolder dir-node)) "single-node")
   ((_has-only-image-subfolder dir-node) "category-node")
   ((_has-multiple-subfolders dir-node) "parent-node")
   (else
    "error-node")))

(define (_has-only-one-file dir-node)
  (= (length (dir-contents/files dir-node)) 1))

(define (_has-only-image-subfolder dir-node)
  (and (= (length (dir-contents/dir dir-node)) 1) (string=? (dir-name (car (dir-contents/dir dir-node))) "images")))

(define (_has-multiple-subfolders dir-node)
  (> (length (dir-contents/dir dir-node)) 1))

;;)
