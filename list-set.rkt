#lang typed/racket

(require "../include/cs151-core.rkt")


;; Possible relationship between two values
;; from a type with a total ordering.
(define-type Order (U '< '= '>))

;; A comparison function on values of type T.
(define-type (Cmp T) (T T -> Order))

;; A set of items represented as a list
(define-struct (Set T)
  ([cmp : (Cmp T)] ;; defines total order on T
   [items : (Listof T)])) ;; the items in the set

(: empty-set : All (T) (Cmp T) -> (Set T))
;; Construct an empty set
(define (empty-set cmp)
  (Set cmp '()))

(: is-empty? : All (T) (Set T) -> Boolean)
;; Is the set empty?
(define (is-empty? set)
  (match set
    [(Set _ '()) #t]
    [_ #f]))

(: add : All (T) T (Set T) -> (Set T))
;; Add the item x to the set.
(define (add x set)
  (if (member? x set)
      set
      (match set
        [(Set cmp items) (Set cmp (cons x items))])))

(: member? : All (T) T (Set T) -> Boolean)
;; Is the item x a member of the set?
(define (member? x set)
  (match set
    [(Set cmp items)
     (ormap
      (lambda ([y : T]) (symbol=? '= (cmp x y)))
      items)]))

(: make-list->set : All (T) (Cmp T) -> (Listof T) -> (Set T))
;; Given a comparison function for the type T, make
;; a list->set function that converts a list to a set.
(define (make-list->set cmp)
  (lambda ([items : (Listof T)])
    (Set cmp items)))

(: gen-random-element : All (T) (Set T) -> T)
;; Selects a uniformly random element from the set.
(define (gen-random-element set)
  (match set
    [(Set _ items)
     (list-ref items (random (length items)))]))


;; Provide access to some functions in other files.

(provide Set
         Order
         Cmp)

(provide empty-set
         is-empty?
         member?
         add
         make-list->set
         gen-random-element)