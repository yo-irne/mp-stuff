#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

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

(define (typeless< x y) (cond
                            [(and (string? x) (string? y)) (string<? x y)]
                            [(and (symbol? x) (symbol? y)) (symbol<? x y)]
                            [(and (number? x) (number? y)) (< x y)]
                            [(and (boolean? x) (boolean? y)) (eq? x #f)]))

(define (lex< row1 row2 pos ids) (cond
                                    [(empty? ids) #f]
                                    [(eq? (first ids) pos) (typeless< (first row1) (first row2))]
                                    [else (lex< (rest row1) (rest row2) pos (rest ids))]))

(define (lex row1 row2 key ids) (cond
                                  [(empty? key) #t]
                                  [(eq? (at row1 (first key) ids) (at row2 (first key) ids))
                                   (lex row1 row2 (rest key) ids)]
                                  [(lex< row1 row2 (first key) ids) #t]
                                  [else #f]))

(define (insert row rows key ids) (cond
                        [(empty? rows) (list row)]
                        [(lex row (first rows) key ids) (cons row rows)]
                        [else (cons (first rows) (insert row (rest rows) key ids))]))
 
(define (sort rows key ids) (cond
                              [(empty? (cdr rows)) rows]
                              [else (insert (first rows) (sort (rest rows) key ids) key ids)]))

;(define (lexrow row1 row2 key ids) (cond
;                                     [(empty? key) #t]
;                                     [(not (lexat row1 row2 (first key) ids)) #f]
;                                     [(eq? (at row1 (first key) ids) (at row2 (first key) ids))
;                                      (lexrow row1 row2 (rest key) ids)]
;                                     [else (or (and
;                                                (eq? (at row1 (first key) ids) (at row2 (first key) ids))
;                                                (lexrow row1 row2 (rest key) ids))
;                                            )]))

(define (table-sort cols tab) (table
                               (table-schema tab)
                               (sort (table-rows tab) cols (get-ids (table-schema tab)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SELECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (at row id ids) (cond
                          [(empty? row) '()]
                          [(eq? id (first ids)) (first row)]
                          [else (at (rest row) id (rest ids))]))

(define (check-form form row ids) (cond
                                [(and-f? form) (and
                                                (check-form (and-f-l form) row ids)
                                                (check-form (and-f-r form) row ids))]
                                [(or-f? form) (or
                                               (check-form (or-f-l form) row ids)
                                               (check-form (or-f-r form) row ids))]
                                [(not-f? form) (not (check-form (not-f-e form) row ids))]
                                [(eq-f? form) (eq?
                                               (eq-f-val form)
                                               (at row (eq-f-name form) ids))]
                                [(eq2-f? form) (eq?
                                                (at row (eq2-f-name) ids)
                                                (at row (eq2-f-name2 form) ids))]
                                [(lt-f? form) (typeless<
                                               (at row (lt-f-name form) ids)
                                               (lt-f-val form))]
                                [(boolean? form) form]))

;check form
(define (valid-rows form rows ids) (cond
                                 [(empty? rows) '()]
                                 [(check-form form (first rows) ids) (cons (first rows) (valid-rows form (rest rows) ids))]
                                 [else (valid-rows form (rest rows) ids)]))

(define (table-select form tab)
  (table (table-schema tab) (valid-rows form (table-rows tab) (get-ids (table-schema tab)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CROSS JOIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cross-concat rows1 rows2) (cond
                                     [(empty? rows1) '()]
                                     [(empty? rows2) '()]
                                     [else (cons
                                            (map (λ (x) (append (first rows1) x)) rows2)
                                            (cross-concat (rest rows1) rows2))]))

(define (table-cross-join tab1 tab2)
  (table (append (table-schema tab1) (table-schema tab2)) (cross-concat (table-rows tab1) (table-rows tab2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NATURAL JOIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (diff xs ys) (cond
                       [(empty? ys) #t]
                       [else (and (not (in (first ys) xs)) (diff (rest ys) xs))]))

;(define (rename-conflict names1 tab1 names2 tab2) (cond
;                                             [(empty? names2) '()]
;                                             [(empty? names1) names2]
;                                             [(diff names1 names2) names2]
;                                             [(in (first names2) names1) (rename-conflict names1
;                                                                                          tab1
;                                                                                          (get-ids (table-schema (table-rename (first names2) () tab2)))
;                                                                                          (table-rename (first names2) "nazwa" tab2) )]))

(define (table-natural-join tab1 tab2) (table '() '()))










