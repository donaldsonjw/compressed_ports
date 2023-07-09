(module configure
   (main main))


(define *native-build* #unspecified)
(define *jvm-build* #unspecified)
(define *remaining-args* #unspecified)
(define *prefix* "/usr/local")

(define (parse-args args)
   (args-parse (cdr args)
      (section "Help")
      ((("--help")
        (help "--help" "This help message"))
       (args-parse-usage #f))
      (section "Backend")
      ((("--disable-native")
        (help "--disable-native" "disable native backend build"))
       (set! *native-build* #f))
      ((("--disable-jvm")
        (help "--disable-jvm" "disable jvm backend build"))
       (set! *jvm-build* #f))
      (("--prefix=?prefix" (help "the installation prefix [default /usr/local]"))
       (set! *prefix* prefix))
      (else
       (print "illegal argument '" else "'. Usage:")
       (args-parse-usage #f)
       (exit -1))))

(define (auto-config)
   (when (eq? *native-build* #unspecified)
      (set!  *native-build* (build-and-exec hello-module)))
   (when (eq? *jvm-build* #unspecified)
      (set! *jvm-build* (build-and-exec hello-module "-jvm"))))

(define (main args)
   (parse-args args)
   (auto-config)
   (with-output-to-file "Makefile.config"
      (lambda ()
         (printf "SUPPORT_NATIVE=~a~%" *native-build*)
         (printf "SUPPORT_JVM=~a~%" *jvm-build*)
         (printf "INSTALL_PREFIX=~a~%" *prefix*)))
   (printf "** Configuration Summary **~%~%")
   (printf "supported backends....... ~(, ) ~%" (append (if *native-build* '(native) '())
                                               (if *jvm-build* '(jvm) '())))
   (printf "install prefix........... ~a ~%" *prefix*)
   (printf "~%"))


(define (ends-with-quote? val::symbol)
   (let ((str (symbol->string val)))
      (char=? #\' (string-ref str
                     (- (string-length str) 1)))))

(define (sh->string obj)
   (match-case obj
      ((quote (and ?a
                   (? symbol?)
                   (? ends-with-quote?)))
       (format "'~a'" (let ((str (symbol->string a)))
                         (substring str 0 (- (string-length str) 1)))))
      ((and ?a (? symbol?))
       (symbol->string a))
      ((and ?a (or (? string?) (? number?)))
       (format "~a" a))
      (else
       (error "sh" "invalid syntax" obj))))

(define (string-trim str)
   (list-ref (pregexp-match "\\s*(.*)\\s*" str) 1))

(define (sh cmd . args)
   (let* ((proc-args (append (map sh->string (cons cmd args))
                        '(:output :pipe :error :pipe :wait #t)))
          (proc (apply run-process proc-args))
          (exit-status (process-exit-status proc))
          (out (process-output-port proc))
          (err (process-error-port proc)))
      (unwind-protect
         (if (and (number? exit-status) (= exit-status 0))
             (string-trim (read-string out))
             #f)
         (begin
            (close-input-port out)
            (close-input-port err)))))


(define (build-and-exec scm-src . args)
   (let* ((tempdir ".autoconf")
          (tempexe  (symbol->string (gensym "autoconfig")))
          (tempsrc (string-append tempexe ".scm")))
      (make-directory tempdir)
      (chdir tempdir)
      (with-output-to-file tempsrc
         (lambda () (display scm-src)))
      (unwind-protect
         (and (apply sh
                 (append (cons* "bigloo" "-s" args)
                    (list "-o" tempexe tempsrc)))
              (sh (format "./~a" tempexe)))
         (begin
            (chdir "..")
            (sh "rm" "-rf" tempdir)))))

(define hello-module
   "(module hello
      (main main))

    (define (main args)
      (print \"true\"))")