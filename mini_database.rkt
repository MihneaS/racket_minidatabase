;#lang racket

(require racket/match)

(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

(define (caar l)
  (car (car l)))

(define (safesec l)
  (if (< (length l) 2)
      '()
      (second l)))

;= Funcțiile de acces
(define init-database
  (λ ()
    '() ))

(define create-table
  (λ (table columns-name)
    (list table (map (λ (column-name) (list column-name '())) columns-name))  ))

(define get-name
  (λ (table)
    (car table) ))

(define get-columns
  (λ (table)
    (map (λ (column) (car column)) (safesec table)) ))

(define get-tables
  (λ (db)
    db ))

(define get-table
  (λ (db table-name)
    (findf (λ (table) (equal? (get-name table) table-name)) db) ))

(define add-table
  (λ (db table)
    (cons table db) ))

(define remove-table
  (λ (db table-name)
    (remove (create-table table-name '(())) db (λ (table1 table2) (equal? (get-name table1) (get-name table2)))) ))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db
  '(
    ("Studenți" (
                 ("Număr matricol" (123 124 125 126))
                 ("Nume" ("Ionescu" "Popescu" "Popa" "Georgescu"))
                 ("Prenume" ("Gigel" "Maria" "Ionel" "Ioana"))
                 ("Grupă" ("321CA" "321CB" "321CC" "321CD"))
                 ("Medie" (9.82 9.91 9.99 9.87))
                )
    )
    ("Cursuri" (
                ("Anul" ("I" "II" "III" "IV" "I" "III"))
                ("Semestru" ("I" "II" "I" "I" "II" "II"))
                ("Disciplină" ("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți"
                                                             "Inteligență artificială" "Structuri de date" "Baze de date"))
                ("Număr credite" (5 6 5 6 5 5))
                ("Număr teme" (2 3 3 3 3 0))
               )
    )
   )
)

(define (myapplyif condition proc lst)
  (map (λ (elem) (if (condition elem)
                     (proc elem)
                     elem)) ))

(define (myapplyifelse condition ifproc elseproc lst)
  (map (λ (elem) (if (condition elem)
                     (ifproc elem)
                     (elseproc elem))) ))



;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define (same-name? t1 t2)
  (equal? (get-name t1) (get-name t2)))

(define (select-table db table-name)
  (findf (λ (table) (equal? (get-name table) table-name)) db ))

(define (on-table-simple-select table-content columns)
  (map
   (λ (column)
     (get-content (findf (λ (c-from-table) (equal? (get-name c-from-table) column)) table-content)))
   columns)
  )

(define simple-select
  (λ (db table-name columns)
    (let ([table (select-table db table-name)])
          (on-table-simple-select (second table) columns)
    ) 
  ))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================



(define insert
 (λ (db table-name record)
   (map (λ (table)
   (if (equal? table-name (get-name table))
    (list (get-name table) (map (λ (column)
    (let [(newcell (findf (λ (rec) (equal? (get-name column) (get-name rec))) record))]
     (if newcell
      (list (car column) (append (second column) (list (cdr newcell))))
      (list (car column) (append (second column) (list NULL)))
                                    ))) (second table))
                       )table)) db)
        ))


(require math/statistics)
(define lit-to-func-map
  (list
   (list NULL (λ x x))
   (list 'min min)
   (list 'max max)
   (list 'count (compose length list))
   (list 'sum +)
   (list 'avg (compose mean list))
   (list 'sort-asc (λ lst (sort lst <)))
   (list 'sort-desc (λ lst (sort lst >)))
  )
)

(define fand
  (λ x 
    (if (null? x)
        #t
        (if (car x)
            (apply fand (cdr x))
            #f))))

(define (lit-to-func lit)
  (second (findf (λ (entry) (equal? lit (first entry))) lit-to-func-map)))


(define zip
  (λ (l1 l2)
    (map list l1 l2)))

(define (get-proc cond)
  (first cond))

(define (get-col-name cond)
  (second cond))

(define (get-val cond)
  (third cond))

(define (cond-to-func cond)
  (λ (lhs) ((get-proc cond) lhs (get-val cond))))

(define (get-content table)
  (second table))

(define (get-col-by-name table col-name)
  (findf (λ (c-from-t) (equal? (get-name c-from-t) col-name)) (get-content table)))

(define (conds-to-bools table conditions)
  (if (null? conditions)
      (list (map
             (λ (cell) #t)
             (get-content (first (get-content table)))))
      (map
       (λ (condition)
         (let ([col (get-content (get-col-by-name table (get-col-name condition)))])
           (let ([condf (cond-to-func condition)])
             (map condf col))))
       conditions)))

(define (andcombine bools)
  (apply map fand bools))          

(define select
  (λ (db table-name columns conditions)
    (let ([simple-columns (map
                           (λ (col) (if (pair? col)
                                        (cdr col)
                                        col))
                           columns)])
      (let ([ops (map (λ (col) (if (pair? col)
                                  (car col)
                                  NULL))
                      columns)])
        (let ([table (select-table db table-name)])
          (let ([bools (andcombine (conds-to-bools table conditions))])
            (let ([filtered-table-content (map
                                           (λ (col) (cons (get-name col)
                                                          (list (map
                                                                 (λ (cell-bool) (first cell-bool))
                                                                 (filter
                                                                  (λ (cell-bool) (second cell-bool))
                                                                  (zip (second col) bools))))))
                                           (get-content table))])
              (map
               (λ (pres op) (apply (lit-to-func op) pres))
               (on-table-simple-select filtered-table-content simple-columns) ops)
              )
            )
          )
        )
      )
   ))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define update
  (λ (db table-name values conditions)
    (map
     (λ (table)
       (if (equal? (get-name table) table-name)
           (list
            (get-name table)
            (let ([bools (andcombine (conds-to-bools table conditions))])
              (map
               (λ (col)
                 (let ([col-name_new-val (findf
                                          (λ (col-name_new-val) (equal? (car col-name_new-val) (get-name col)))
                                          values)])
                   (if col-name_new-val
                       (list
                        (get-name col)
                        (map
                         (λ (old-val must-be-replaced)
                           (if must-be-replaced
                               (cdr col-name_new-val)
                               old-val))
                         (get-content col) bools)
                        )
                       col)
                   ))
               (get-content table))))
           table))
     db)
    ))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define filter-bools
  (λ (lst bools)
    (let ([ zipped (map
                    (λ (cell bool)
                      (cons cell bool))
                    lst bools)
                   ])
      (let ([filtered (filter
                       (λ (zipped-cell) (cdr zipped-cell))
                       zipped)
                      ])
        (map
         (λ (filtered-cell)
           (car filtered-cell))
         filtered)
        )
      )
    ))

(define inverse-bools
  (λ (bools)
    (map (λ (bool)
           (if bool
               #f
               #t
               )
           )
         bools)
    ))

(define delete
  (λ (db table-name conditions)
    (map
     (λ (table)
       (if (equal? (get-name table) table-name)
           (list
            (get-name table)
            (let ([bools (inverse-bools (andcombine (conds-to-bools table conditions)))])
              (map
               (λ (col)
                 (list
                  (get-name col)
                  (filter-bools (get-content col) bools)
                  )
                 )
               (get-content table))
              )
            )
           table))
     db)
    ))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))
