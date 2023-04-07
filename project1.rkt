#lang racket

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INSERT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define (string) "")
; (define (boolean) #t)
; (define (number) 1)
; (define (symbol) '>)

(define (type a) (cond
                   [(symbol? a) 'symbol]
                   [(string? a) 'string]
                   [(boolean? a) 'boolean]
                   [(number? a) 'number]))

; (define (vector-check row tab)
;  (and (eq?
;        (type (first row))
;        (column-info-type (first (table-schema tab))))
;       (vector-check (rest row) (rest (table-schema tab)))))

(define (row-check row schema) (cond
                                 [(or (and (empty? row) (not (empty? schema)))
                                      (and (not (empty? row)) (empty? schema))) #f]
                                 [(and (empty? row) (empty? schema)) #t]
                                 [else (and (eq?
                                             (type (first row))
                                             (column-info-type (first schema)))
                                            (row-check (rest row) (rest schema)))]))

(define (table-insert row tab) (if (row-check row (table-schema tab))
                                   (table (table-schema tab) (cons row (table-rows tab))) 
                                   (error "Row incompatible")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define (funny row cols ids) (cond
;                               [(empty? row) row]
;                               [(null? cols) '()]
;                               [(eq? (first cols) (first ids)) (cons (first row) (funny (rest row) (rest cols) (rest ids)))]
;                               [else (funny (rest row) cols (rest ids))]))

(define (in x xs) (cond
                    [(empty? xs) #f]
                    [(eq? x (first xs)) #t]
                    [else (in x (rest xs))]))

(define (clear-row row cols ids) (cond
                                   [(empty? row) row]
                                   [(empty? cols) '()]
                                   [(in (first ids) cols) (cons (first row) 
                                                                (clear-row (rest row) cols (rest ids)))]
                                   [else (clear-row (rest row) cols (rest ids))]))

(define (get-ids schema) (cond
                        [(empty? schema) '()]
                        [else (cons (column-info-name (first schema)) 
                                    (get-ids (rest schema)))]))

(define (table-project cols tab) (table
                                  (clear-row (table-schema tab) cols (get-ids (table-schema tab)))
                                  (map (λ (row) (clear-row row cols (get-ids (table-schema tab)))) 
                                  (table-rows tab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RENAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (table-rename col ncol tab) (table
                                     (map (λ (x) (if (eq? (column-info-name x) col) 
                                                          (column-info ncol (column-info-type x)) x)) (table-schema tab)) 
                                    (table-rows tab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SORT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (table-sort cols tab) (table
                               '() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SELECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab)
  (table '() '()))












