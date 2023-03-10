#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Possible relationship between two values
;; from a type with a total ordering.
(define-type Order (U '< '= '>))

;; A comparison function on values of type T.
(define-type (Cmp T) (T T -> Order))

;; A pair of values
(define-struct (Pair A B)
  ([fst : A]
   [snd : B]))

;; A binary search tree is either the empty tree
;; or a node.
(define-type (BST T) (U 'Empty (Node T)))

;; A node has an item attached to it, has a size (the
;; number of nodes in the tree), and has has two children
;; which are themselves binary search trees.
(define-struct (Node T)
  ([item : T]
   [size : Natural]
   [left-child : (BST T)]
   [right-child : (BST T)]))

;; A set of items represented as a binary search tree
(define-struct (Set T)
  ([cmp : (Cmp T)] ;; defines total order on T
   [bst : (BST T)])) ;; binary search tree of items

(: empty-set : All (T) (Cmp T) -> (Set T))
;; Construct an empty set
(define (empty-set cmp)
  (Set cmp 'Empty))

(: is-empty? : All (T) (Set T) -> Boolean)
;; Is the set empty?
(define (is-empty? set)
  (match set
    [(Set _ 'Empty) #t]
    [_ #f]))

(: add : All (T) T (Set T) -> (Set T))
;; Add the item new-item to the set.
(define (add new-item set)
  (match set
    [(Set cmp tr)
     (local
       {(: bst-insert : (BST T) -> (BST T))
        ;; insert an item into a BST
        (define (bst-insert bst)
          (match bst
            ['Empty (Node new-item 1 'Empty 'Empty)]
            [(Node item size left-child right-child)
             (match (cmp new-item item)
               ['< (Node item (+ size 1) (bst-insert left-child) right-child)]
               ['> (Node item (+ size 1) left-child (bst-insert right-child))]
               ['= bst])]))}
       (Set cmp (bst-insert tr)))]))

(: member? : All (T) T (Set T) -> Boolean)
;; Is the item x a member of the set?
(define (member? search-item set)
  (match set
    [(Set cmp bst)
     (match bst
       ['Empty #f]
       [(Node item size left-child right-child)
        (match (cmp search-item item)
          ['< (member? search-item (Set cmp left-child))]
          ['> (member? search-item (Set cmp right-child))]
          ['= #t])])]))

;;tests for member? function
(: cmp-int : Integer Integer -> Order)
;;comparison for two integers
(define (cmp-int int1 int2)
  (cond
    [(> int1 int2) '>]
    [(< int1 int2) '<]
    [else '=]))

(define test-set-1
  (Set cmp-int
       (Node 75 9
             (Node 62 2 (Node 55 1 'Empty 'Empty) 'Empty)
             (Node 90 6 (Node 80 3
                              (Node 77 1 'Empty 'Empty)
                              (Node 84 2 'Empty
                                    (Node 87 1 'Empty 'Empty)))
                   (Node 100 2 'Empty (Node 101 1 'Empty 'Empty))))))

(check-expect
 ((inst member? Integer) 12 (Set cmp-int 'Empty)) #f)
(check-expect
 ((inst member? Integer)
  60
  (Set cmp-int (Node 60 2 'Empty (Node 65 1 'Empty 'Empty)))) #t)
(check-expect
 ((inst member? Integer)
  50
  (Set cmp-int (Node 60 2 'Empty (Node 65 1 'Empty 'Empty)))) #f)
(check-expect
 ((inst member? Integer)
  55
  (Set cmp-int (Node 60 2 'Empty (Node 65 1 'Empty 'Empty)))) #f)
(check-expect
 ((inst member? Integer) 87 test-set-1) #t)
(check-expect
 ((inst member? Integer) 102 test-set-1) #f)
(check-expect
 ((inst member? Integer) 55 test-set-1) #t)
(check-expect
 ((inst member? Integer) 77 test-set-1) #t)                  

(: bst-size : All (T) (BST T) -> Natural)
;; Returns the size of a BST.
(define (bst-size bst)
  (match bst
    ['Empty 0]
    [(Node _ size _ _) size]))

(: make-list->set : All (T) (Cmp T) -> (Listof T) -> (Set T))
;; Given a comparison function for the type T, make
;; a list->set function that converts a list to a set.
(define (make-list->set cmp)
  (lambda ([items : (Listof T)])
    (local
      {(define sorted-items : (Listof T)
         (sort items (lambda ([x : T] [y : T]) (symbol=? (cmp x y) '<))))}
      (match (sorted-list->bst sorted-items (length items))
        [(Pair bst _) (Set cmp bst)]))))

(: sorted-list->bst : All (T) (Listof T) Natural -> (Pair (BST T) (Listof T)))
;; Helper function for make-list->set.
;; Converts the first n elements of the given sorted list
;; to a balanced BST, and returns a pair with that BST
;; and the rest of the list. Assumes input list is sorted.
(define (sorted-list->bst items n)
  (if (= n 0)
      (Pair 'Empty items)
      (local
        {(define left-size : Natural (quotient n 2))
         (define right-size : Natural (cast (- n left-size 1) Natural))}
        (match (sorted-list->bst items left-size)
          [(Pair _ '()) (error "impossible")]
          [(Pair left-child (cons root-value right-list))
           (match (sorted-list->bst right-list right-size)
             [(Pair right-child leftover)
              (Pair (Node root-value n left-child right-child) leftover)])]))))

(: gen-random-element : All (T) (Set T) -> T)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Selects a uniformly random element from the set.
(define (gen-random-element set)
  (local
    {(: element-at-index : (BST T) Natural -> T)
     (define (element-at-index bst index)
       (match bst
         ['Empty (error "gen-random-element: empty set")]
         [(Node item size left-child right-child)
          (local
            {(define left-size : Natural (bst-size left-child))}
            (cond
              [(< index left-size) (element-at-index left-child index)]
              [(= index left-size) item]
              [else (element-at-index
                     right-child (cast (- index left-size 1) Natural))]))]))}
    (match set
      [(Set _ bst) (element-at-index bst (random (bst-size bst)))])))


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
