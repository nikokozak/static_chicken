;; Functions for dealing with the filesystem by invoking sh commands.
(module fs-utils (*)

  (import scheme)
  (import (chicken base))
  (import shell)
  (import token)


  (define image-formats '("jpg" "jpeg" "png" "tiff"))
  (define image-folders '("images" "imgs"))
  (define template-formats '("template"))


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
                                  (if (not (last? arglist))
                                      (string-append (car arglist) "/")
                                      (car arglist))
                                  (helper (cdr arglist)))
                                ""))))
            (string-append cwd (string-append "/" (helper args))))
          cwd)))

  ;; Returns the proverbial file-name or folder-name of an absolute-path,
  ;; where a folder in an absolute path is not terminated with "/"
  (define (path-end path)
    (_strip-until-last #\/ path))

  ;; Removes any extension information from a filename.
  (define (strip-extension path-or-filename)
    (_strip-after-last #\. path-or-filename))

  ;; List the contents of a given directory
  ;; full? -> use full pathnames
  (define (dir path . full?)
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
  (define (dir? path)
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

  ;; Is the given path an image? compares with image-formats.
  (define (image? path)
    (and (file? path) (contains? (extension path) image-formats)))

  ;; Is the given path a template?
  (define (template? path)
    (and (file? path) (contains? (extension path) template-formats)))

  ;; Checks whether a given path exists
  (define (exists? path)
    (and (dir? path) (file? path)))


  ;; Checks whether a given folder name is within the image-folders
  (define (image-dir? dirname)
    (or (contains? dirname image-folders)
        (contains? (path-end dirname) image-folders)))

;; Checks whether a list contains a given element
  (define (contains? obj ls)
    (foldr (lambda (x seed) (or x seed)) #f
          (map (lambda (x) (or (equal? x obj) (string-ci=? x obj))) ls)))

  ;; Returns a file's extension
  (define (extension filename-or-path)
    (_strip-until-last #\. filename-or-path))

  ;; Returns a pair (w h) or () if unable to read.
  (define (img-dims path)
    (shell-command->list
    (capture (file ,path -b \|
                    grep -Eo \'\[0-9\]+\\\s?x\\\s?\[0-9\]+\'
                    \| grep -Eo \'\[0-9\]+\'))))

  ;; Returns a file's size in kb or -1 if error
  (define (file-size path)
    (let ((result (shell-command->list (capture (du -k ,path)))))
      (if (not (null? result))
          (car result)
          -1)))

  ;; A Directory token
  ;; (define a (Token name abs-path ...))
  ;; (token-name a)
  ;; (set! (token-name a) "new")
  ;; (token-token? a)
  (make-token Dir
              name
              abs-path
              rel-path
              contents
              image-dir?)

  ;; A File token
  (make-token File
              name
              abs-path
              rel-path
              type)

  (make-token Template
              name
              for
              abs-path
              rel-path)

  ;; An image token
  (make-token Img
              name
              abs-path
              rel-path
              type
              width
              height
              size)

  ;; An error token
  (make-token Error
              name
              abs-path
              rel-path)

  ;; Returns a tree of tokens (defined above)
  (define (tree path)
    (let
        ((start-dir path))
      (letrec
          ((travel (lambda (objects rel-path)
                    (if (null? objects)
                        '()
                        (let*
                            ((obj-name (path-end (car objects)))
                              (obj-abs-path (car objects))
                              (obj-rel-path (string-append rel-path "/" obj-name))
                              (obj-is-dir? (dir? obj-abs-path)))
                          (cons
                            (cond
                            (obj-is-dir? (Dir obj-name
                                              obj-abs-path
                                              obj-rel-path
                                              (travel (dir obj-abs-path 'abs-paths) obj-rel-path)
                                              (image-dir? obj-name)))
                            ((image? obj-abs-path) (Img obj-name
                                                        obj-abs-path
                                                        obj-rel-path
                                                        (extension obj-name)
                                                        (car (img-dims obj-abs-path))
                                                        (cadr (img-dims obj-abs-path))
                                                        (file-size obj-abs-path)))
                            ((template? obj-abs-path) (Template obj-name
                                                                obj-abs-path
                                                                obj-rel-path
                                                                (strip-extension obj-name)))
                            ((file? obj-abs-path) (File obj-name
                                                        obj-abs-path
                                                        obj-rel-path
                                                        (extension obj-name)))
                            (else (Error obj-name
                                          obj-abs-path
                                          obj-rel-path)))
                            (travel (cdr objects) rel-path)))))))
        (travel (dir start-dir 'abs-paths) ""))))

  ;; When used with a shell command such as (capture EXPRESSION),
  ;; takes the result of the shell command and turns it into a list of strings.
  ;; Takes care of minor formatting adjustments, like pruning end "/" in abs-paths.
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
                             (cons (if (symbol? current)
                                       (prune-end-slash (symbol->string current))
                                       current)
                                   (reader))))))))
        (reader))))

  ;; Removes the final / from a non-empty string.
  (define (prune-end-slash str)
    (let ((penultimate (- (string-length str) 1)))
      (if (equal? (string-ref str penultimate)
                  #\/)
          (substring str 0 penultimate)
          str)))

  #|--------------------------------------------------------------------|#
  #|------------------------ UTILITY FUNCTIONS -------------------------|#
  #|--------------------------------------------------------------------|#

  ;; Find the index of a given character in a charlist, starting at index.
  (define (_find charlist char index)
    (cond
    ((null? charlist) 0)
    (else
      (if (equal? (car charlist) char)
          index
          (_find (cdr charlist) char (add1 index))))))

  ;; Strip all characters from string up until last occurrence of a given character.
  (define (_strip-until-last char str)
    (let ((reversed (reverse (string->list str))))
      (substring str (- (string-length str) (_find reversed char 0)))))

  ;; Strip all characters from string after last occurrence of a given character.
  (define (_strip-after-last char str)
    (let ((reversed (reverse (string->list str))))
      (substring str 0 (- (string-length str) (add1 (_find reversed char 0))))))
)
