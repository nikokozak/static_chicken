;;(module indexer (make-dir-tree)

  ;;(import scheme)
  ;;(import (chicken base))
(import fs-utils)

(define (strip-extensions file-list)
  (if (null? file-list)
      '()
      (cons (strip-extension (car file-list)) (strip-extensions (cdr file-list)))))

(define (make-nav-tree dir-tree)
  (letrec ((helper (lambda (dirs)
                     (cond
                      ((null? dirs) '())
                      (else
                       (let ((current (car dirs))
                             (name (dir-name (car dirs)))
                             (files (strip-extensions (dir-contents/files (car dirs))))
                             (subdirs (dir-contents/dir (car dirs)))
                             (rest (cdr dirs)))
                         (cons
                          (cons name (cons files (cons (helper subdirs) '())))
                          (helper rest))))))))
    (helper dir-tree)))


;;)
