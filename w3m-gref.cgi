#!/usr/bin/env gosh
;; vim:set ft=scheme fileencoding=utf-8:

;;;
;;; w3m-gref.cgi - Gaucheのリファレンスを検索するlocal CGI
;;;

(use dbm)
(use file.util :only (home-directory temporary-directory build-path))
(use gauche.record :only (define-record-type))
(use rfc.uri :only (uri-decode-string))
(use srfi-1 :only (append-map))
(use srfi-13 :only (string-null?))
(use text.html-lite)
(use text.tree :only (write-tree))

(define *app-directory* (build-path (home-directory) ".w3m-gref"))

(define *config*
  (let1 alist (with-input-from-file (build-path *app-directory* "config") read)
    (lambda (key)
      (and-let* ((pair (assoc key alist)))
        (cdr pair)))))

(define path->url
  (pa$ string-append "file://"))

(define (root-url)
  (path->url (car (glob (build-path (*config* 'html-dir) "gauche-ref[ej].html")))))

(define with-db
  (let1 db-class (dbm-type->class (*config* 'db-type))
    (lambda (proc)
      (let1 db (dbm-open db-class :path (*config* 'db-path) :rw-mode :read :value-convert #t)
        (unwind-protect
          (proc db)
          (unless (dbm-closed? db)
            (dbm-close db)))))))

(define (eprint content)
  (with-output-to-file (build-path (temporary-directory) "gref-debug")
    (lambda () (write content) (newline) (flush)))
  content)

(define-record-type item  #t #f
  name url section)

;; ---------------------------------------------------------------------
;; Entry point
;;
(define (main _)
  (guard (e (else (write-tree (error-page e))))
    (write-tree ((.$ render sort-by-section itemize search get-query)))))

(define (error-page e)
  (html:html
    (html:head (html:title "Error"))
    (html:body
      (html:h1 "Error")
      (html:p (html-escape-string (~ e 'message))))))

(define (render query items)
  (define html-header "Content-Type: text/html; charset=UTF-8\r\n\r\n")
  (define h html-escape-string)
  (define (w3m-goto url)
    (list #`"w3m-control: GOTO ,|url|\r\n"
            "w3m-control: DELETE_PREVBUF\r\n"))
  (define (single? l)
    (and (not (null? l)) (null? (cdr l))))
  (define (item->tr i)
    (html:tr
      (html:td (html:a :href (item-url i)
                       #`",(h (item-name i)) "))
      (html:td (h (item-section i)))))
  (cond ((null? items)
         (w3m-goto (root-url)))
        ((single? items)
         (w3m-goto (item-url (car items))))
        (else
          `(,html-header
             ,(html:html
                (html:body
                  (html:h1 (h query))
                  (html:table
                    (map item->tr items))))))))

(define (sort-by-section query items)
  (define (section-number sec)
    (or (and-let* ((m (#/^(\d+)\.(\d+)?/ sec))
                   (n (if (and (m 2) (not (string-null? (m 2))))
                        (string->number (m 2))
                        0)))
          (+ (* 1000 (string->number (m 1))) n))
        0))
  (values query
          (sort-by items (^i (vector (section-number (item-section i))
                                     (item-name i)))
                   (^(v1 v2) (let ([nsec1 (~ v1 0)] [nsec2 (~ v2 0)]
                                   [name1 (~ v1 1)] [name2 (~ v2 1)])
                               (if (= nsec1 nsec2)
                                 (string<=? name1 name2)
                                 (< nsec1 nsec2)))))))

(define (itemize query results)
  (define (href->url href)
    (path->url (build-path (*config* 'html-dir) href)))
  (define (href->title href)
    (let1 file (or (string-scan href #\# 'before) href)
      (or (with-db (^(db) (dbm-get db file #f)))
          file)))
  (define (result->item pair)
    (let ([name (car pair)] [hrefs (cdr pair)])
      (map (^h (make-item name (href->url h) (href->title h)))
           hrefs)))
  (values query (append-map result->item results)))

;; query -> (result ...)
;; result : (name href1 href2 ...)
(define (search query)
  (rxmatch-case query
    ;; /regexp/[option] or regexp/[option]
    [#/^\/?(.+)(?<!\\)\/(i)?$/ (#f rx case-fold)
     (regexp-search rx (and case-fold (not (string-null? case-fold))))]
    ;; /regexp
    [#/^\/(.+)$/ (#f rx)
     (regexp-search rx)]
    [else
      (fixed-search query)]))

(define (regexp-search query-rx :optional (case-fold #f))
  (let* ((rx (string->regexp query-rx :case-fold case-fold))
         (results (with-db
                    (cut dbm-fold <>
                         (^(key hrefs seed)
                             (cond ((#/gauche-ref[ej].*?\.html/ key) seed)
                                   ((rxmatch rx key)
                                    (cons (cons key hrefs) seed))
                                   (else seed))) '()))))
    (values rx results)))

(define (fixed-search query)
  (if-let1 hrefs (with-db (^(db) (dbm-get db query #f)))
    (values query (list (cons query hrefs)))
    (values query '())))

(define (get-query)
  (and-let* ((query (string-scan (sys-getenv "QUERY_STRING") ":" 'after)))
    (uri-decode-string query)))
