(module fs-utils (path ;; returns current path or appends args to path
                  dir-contents ;; list contents of given dir as strings
                  dir-dirs ;; list only directories in given dir as strings
                  dir-files ;; list only files in given dir as strings
                  directory? ;; check if given path is a directory
                  file?) ;; check if given path is a file

  (import scheme)
  (import (chicken base))
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
                            ((eof-object? current) (quote ()))
                            (else
                            (cons (symbol->string current) (reader))))))))
        (reader))))

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

  (define (path-end path)
    (let ((reversed (reverse (string->list path))))
      (letrec ((find (lambda (charlist char index)
                       (if (equal? (car charlist) char)
                           index
                           (find (cdr charlist) char (add1 index))))))
        (substring path (- (string-length path) (find reversed #\/ 0))))))

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
)
