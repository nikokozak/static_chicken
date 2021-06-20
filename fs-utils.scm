(module fs-utils (*)

  (import scheme)
  (import (chicken base))
  (import (chicken format))
  (import shell)

  ;; Functions for dealing with the filesystem by invoking sh commands.

  ;; When used with a shell command such as (capture EXPRESSION),
  ;; takes the result of the shell command and turns it into a list of strings.
  ;; If the shell command fails, or does not return, the function returns an empty list.
  (define (shell-command->list shell-command)
    (let* ((command-result (if (eof-object? shell-command)
                              ""
                              shell-command))
          (port (open-input-string command-result)))
      (letrec ((reader (lambda ()
                         (let ((current (read port)))
                          (cond
                            ((eof-object? current) '())
                            (else
                            (cons (prune-end-slash (symbol->string current)) (reader))))))))
        (reader))))

  ;; Removes the final / from a non-empty string.
  (define (prune-end-slash str)
    (let ((penultimate (- (string-length str) 1)))
      (if (equal? (string-ref str penultimate)
                  #\/)
          (substring str 0 penultimate)
          str)))

  ;; When passed no args, returns the full path to the CWD.
  ;; When passed one or more args, concatenates the args onto the CWD.
  ;; NOTE: Args should not use trailing "/"
  ;; (path) => PWD, (path "src") => PWD/src, (path "src" "text") => PWD/src/text
  (define (path . args)
    (let ((cwd (car (shell-command->list (capture (pwd)))))
          (last? (lambda (arglist) (null? (cdr arglist)))))
      (if (not (null? args))
          (letrec ((helper (lambda (arglist)
                            (if (not (null? arglist))
                                (string-append
                                  (if (not last?)
                                      (string-append (car arglist) "/")
                                      (car arglist))
                                  (helper (cdr arglist)))
                                ""))))
            (string-append cwd (string-append "/" (helper args))))
          cwd)))

  (define (_find charlist char index)
    (cond
     ((null? charlist) #f)
     (else
      (if (equal? (car charlist) char)
        index
        (_find (cdr charlist) char (add1 index))))))

  (define (_strip-until-last char str)
    (let ((reversed (reverse (string->list str))))
      (substring str (- (string-length str) (_find reversed char 0)))))

  (define (_strip-after-last char str)
    (let ((reversed (reverse (string->list str))))
      (substring str 0 (- (string-length str) (add1 (_find reversed char 0))))))

  (define (path-end path)
    (_strip-until-last #\/ path))

  (define (strip-extension path)
    (_strip-after-last #\. path))

  ;; List the contents of a given directory
  ;; full? -> use full pathnames
  (define (dir-contents path . full?)
    (if (not (null? full?))
        (shell-command->list (capture (cd ,path && ls -d -1 \"$PWD/\"*)))
        (shell-command->list (capture (cd ,path && ls)))))

  ;; List only the files in a given directory
  ;; full? -> use full pathnames
  (define (dir-files path . full?)
    (if (not (null? full?))
        (shell-command->list (capture (cd ,path && ls -d -1 \"$PWD/\"*\(.\))))
        (shell-command->list (capture (cd ,path && ls -p \| grep -v /)))))

  ;; List only the directories in a given directory
  ;; full? -> use full pathnames
  (define (dir-dirs path . full?)
    (if (not (null? full?))
        (shell-command->list (capture (cd ,path && ls -d -1 \"$PWD/\"*/)))
        (shell-command->list (capture (cd ,path && ls -d */)))))

  ;; Is the given path a directory?
  (define (directory? path)
    (let ((result (shell-command->list (capture (test -d ,path && echo \"true\")))))
      (if (not (null? result))
          (equal? (car result) "true")
          #f)))

  ;; Is the given path a file?
  (define (file? path)
    (let ((result (shell-command->list (capture (test -f ,path && echo \"true\")))))
      (if (not (null? result))
          (equal? (car result) "true")
          #f)))

  ;; Checks whether a given path exists
  (define (exists? path)
    (and (directory? path) (file? path)))



  ;; A simple record structure to hold dir information.
  (define-record dir name path contents/dir contents/files)
  (set-record-printer! dir (lambda (x out)
                            (fprintf out "(#dir ~S ~S ~S ~S)"
                                      (dir-name x)
                                      (dir-path x)
                                      (dir-contents/dir x)
                                      (dir-contents/files x))))

  ;; Returns a tree of dirs, including files and subdirs.
  (define (make-dir-tree path)
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

)
