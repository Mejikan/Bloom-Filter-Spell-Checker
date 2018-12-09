; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2018                              *
; *  Student Version                          *
; *********************************************
#lang racket
;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains dictionary definition
(require "test-dictionary.rkt")
;;(require "dictionary.rkt")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;;(define getkey
;;  (lambda (w)
;;    (if (pair? w)
;;      (+ (* 31 (getkey (cdr w))) (getkey (car w)))
;;      (if (null? w)
;;        0
;;        (ctv w)
;;      )
;;    )
;;))

(define my-op
  (lambda (a b)
    (+ (ctv a) (* 31 b))
  )
)

(define getkey
  (lambda (w)
    (reduce my-op w 0)
))


(define rev
  (lambda (l)
    (if (null? l)
      '()
      (if (pair? l)
          (append (rev (cdr l)) (list (rev (car l))))
          l
      )
    )
  )
)

(define gen-bitvector
  (lambda (hashfunc dict)
    (if (null? dict)
      '()
      (cons (hashfunc (car dict)) (gen-bitvector hashfunc (cdr dict)))
    )
  )
)

(define list-contains-num
  (lambda (l x)
    (if (null? l)
      #f
      (if (= (car l) x)
        #t
        (list-contains-num (cdr l) x)
      )
    )
  )
)

(define list-contains-num-list
  (lambda (a b)
    (if (null? b)
      #t
      (if (list-contains-num a (car b))
        (list-contains-num-list a (cdr b))
        #f
      )
    )
  )
)

(define gen-bitvector-multi
  (lambda (hashfunctionlist dict)
    (if (null? hashfunctionlist)
      '()
      (append (gen-bitvector (car hashfunctionlist) dict) (gen-bitvector-multi (cdr hashfunctionlist) dict))
    )
  )
)

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (getkey (rev w))
))


;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
      (modulo (key w) size)
    )
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
      (floor (* (- (* (key w) A) (floor (* (key w) A))) size))
    )
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 701))
(define hash-2 (gen-hash-division-method 899))
(define hash-3 (gen-hash-multiplication-method 700))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))


;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o)) ==> 674
;;  (hash-1 '(d a y))     ==> 395
;;  (hash-1 '(c l a s s)) ==> 360
;;
;;  (hash-2 '(h e l l o)) ==> 139
;;  (hash-2 '(d a y))     ==> 304
;;  (hash-2 '(c l a s s)) ==> 205
;;
;;  (hash-3 '(h e l l o)) ==> 552.0
;;  (hash-3 '(d a y))     ==> 501.0
;;  (hash-3 '(c l a s s)) ==> 247.0
;;
;;  (hash-4 '(h e l l o)) ==> 710.0
;;  (hash-4 '(d a y))     ==> 644.0
;;  (hash-4 '(c l a s s)) ==> 317.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (lambda (w)
      (list-contains-num-list (gen-bitvector-multi hashfunctionlist dict) (gen-bitvector-multi hashfunctionlist (list w)))
    )
))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

;;((gen-hash-division-method 4) 13)
;;(key '(h e l l o))
;;((gen-hash-multiplication-method 10) '(h e l l o))

;;(checker-1 '(a b i l i t y))

;;(gen-bitvector hash-1 dictionary)
;;(list-contains-num-list (gen-bitvector-multi hashfl-1 dictionary) (gen-bitvector-multi hashfl-1 (list '(s c h e m e))))

;;(list-contains-num '(-1 5 7) -1)
