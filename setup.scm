(use dbm)
(use dbm.gdbm)
(use file.util)
(use gauche.parseopt)
(use gauche.process)
(use gauche.record)
(use rfc.http)
(use rfc.uri)
(use srfi-1)
(use srfi-13)
(use util.list)
(use util.match)

(define-record-type (topic (pseudo-rtd <vector>)) #t #f
  key href section)

(define *archives*
  '((en . "http://practical-scheme.net/vault/gauche-refe.tgz")
    (ja . "http://practical-scheme.net/vault/gauche-refj.tgz")))

(define (main args)
  (let-args (cdr args)
    ((html-dir  "d|html-dir=s")
     (lang      "l|lang=y" 'ja)
     (help      "h|help" => (cut usage))
     (else  =>  (^(option rest cont)
                  (print "unknown option: " option)
                  (usage))))

    (unless (file-exists? (app-directory))
      (create-app-directory))

    (cond ((and (not html-dir)
                (assq-ref *archives* lang))
           => (lambda (archive-uri)
                (set! html-dir (download-reference archive-uri (app-directory)))))
          ((and html-dir (relative-path? html-dir)
           (set! html-dir
             (sys-normalize-pathname
               html-dir :absolute #t :expand #t :canonicalize #t)))))

    (unless (and html-dir
                 (absolute-path? html-dir)
                 (file-is-directory? html-dir))
      (error "not directory:" html-dir))

    (make-index html-dir)

    (write-config (app-directory)
                  `((html-dir . ,html-dir)
                    (db-path  . ,(db-path))))
    0))

(define (usage)
  (for-each print
    (list #`"usage: gosh ,(sys-basename *program-name*) file|directory|URL"))
            (exit 0))

(define (app-directory)
  (build-path (home-directory) ".w3m-gref"))

(define (db-path)
  (build-path (app-directory) "gref.dbm"))

(define (create-app-directory)
  (make-directory* (app-directory) #o755))

(define (split-uri uri)
  (receive (scheme specific)
    (uri-scheme&specific uri)
    (receive (authority path query fragment)
      (uri-decompose-hierarchical specific)
      (lambda (message)
        (case message
          ((scheme) scheme)
          ((authority) authority)
          ((path) path)
          ((query) query)
          ((fragment) fragment)
          (else (error "split-uri - unknown message:" message)))))))

  (define (path->html-dir path)
    (car (string-split (regexp-replace #/^.*\// path "") ".")))

(define (download-reference uri extract-dir)
  (let ((uri (split-uri uri))
        (proxy (and-let* ((env (sys-getenv "http_proxy")))
                 (split-uri env))))
    (receive (status hdrs body)
      (call-with-output-process `(tar xzf - -C ,extract-dir)
        (lambda (sink)
          (http-get (uri 'authority) (uri 'path)
                    :proxy (and proxy (proxy 'authority))
                    :sink sink
                    :flusher (lambda (sink headers)
                               (close-output-port sink)))))
      (unless (string=? status "200")
        (error "download failed. Server returns a" status))
      (build-path extract-dir (path->html-dir (uri 'path))))))

(define (make-index html-dir)
  (let* ((htmls (get-htmls html-dir))
         (topics (append (append-map get-topics (get-index-htmls htmls))
                         (get-section-topics (toc-file html-dir))))
         (db (dbm-open <gdbm> :path (db-path) :rw-mode :create :value-convert #t)))

           ;; html -> title
           (for-each (^(html)
                       (dbm-put! db (sys-basename html) (html-title html)))
                     htmls)

           ;; topic -> (href ...)
           (for-each (match-lambda
                       ((key . hrefs) (dbm-put! db key hrefs)))
                     (merge-topics topics))

           (dbm-close db)
           )
  )

(define (topic=? t1 t2)
  (and (string=? (topic-key t1) (topic-key t2))
       (topic-section t1)
       (topic-section t2)
       (string=? (topic-section t1) (topic-section t2))))

;; (topic ...) -> ((topic-key . (topic-href ...)) ...)
(define (merge-topics topics)
  (let ((ht (make-hash-table 'string=?)))
    (dolist (t topics)
      (hash-table-update!
        ht (topic-key t)
        (^(lst) (if lst (delete-duplicates (cons t lst) topic=?) (list t)))
        #f))
    (hash-table-map ht (lambda (key topics)
                         (cons key (map topic-href topics))))))

(define (get-htmls dir)
  (glob (build-path dir "gauche-ref[ej]*.html")))

(define (toc-file dir)
  (car (glob (build-path dir "gauche-ref[ej]_toc.html"))))

(define (index-html? html)
  (with-input-from-file html
    (lambda ()
      (let loop ((line (read-line)))
        (cond ((eof-object? line) #f)
              ((string-scan line "class=\"summary-letter\"") #t)
              (else (loop (read-line))))))
    :encoding "*jp"))

(define (get-index-htmls htmls)
  (filter index-html? htmls))

(define (html-title html)
  (with-input-from-file html
    (lambda ()
      (let loop ((line (read-line)))
        (cond [(eof-object? line) #f]
              [(#/<title>(.*?)<\/title>/i line)
               => (lambda (m)
                    (string-trim-both
                      (or (string-scan (m 1) #\: 'after) m)))]
              [else (loop (read-line))])))
    :encoding "*jp"))

(define (html-unescape s)
  (regexp-replace-all* s
    #/&gt\;/   ">"
    #/&lt\;/   "<"
    #/&quot\;/ "\""
    #/&amp\;/  "&"))

(define (tag-remove s)
  (regexp-replace-all #/<\/?\w+?>/ s ""))

(define (grep regexp proc :optional (port (current-input-port)))
  (filter-map (^(line)
                (and-let* ((m (rxmatch regexp line)))
                  (proc m)))
              (port->string-list port)))

(define (get-topics index-html)
  (define rx #/<a href=\"(?<href>.+?)\"><code>(?<key>.+?)<\/code>.*<a href=\".+?\">(?<section>.+?)<\/a>/i)
  (with-input-from-file index-html
    (cut grep rx (^m (make-topic (html-unescape (m 'key))
                                 (m 'href)
                                 (tag-remove (html-unescape (m 'section))))))
    :encoding "*jp"))

(define (get-section-topics toc-html)
  ;; from: http://gauche.svn.sourceforge.net/viewvc/gauche/Gauche-scripts/trunk/gaucheref.cgi
  (define rx #/<a name=\"[^\"]*\" href=\"(?<href>[^\"]*)\">\d\.[.\d]*\s+(?<key>[^<]*)<\/a>/i)
  (with-input-from-file toc-html
    (cut grep rx (^m (make-topic (html-unescape (tag-remove (m 'key)))
                                 (m 'href)
                                 #f)))
    :encoding "*jp"))

(define (write-config dir sexp)
  (with-output-to-file (build-path dir "config") (cute write sexp)))
