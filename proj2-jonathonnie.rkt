#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/batch-io)
(require "bst-set-jonathonnie.rkt")

(define spacer-opacity : Byte 0)

;; An (Option T) is either 'None or (Some x),
;; where x has type T
(define-type (Option T) (U 'None (Some T)))
(define-struct (Some T) ([value : T]))

(define-struct (Pair A B)
  ([fst : A]
   [snd : B]))

(define-type Placement (U 'placed 'in 'out 'unknown))

(define-struct Tile
  ([letter : (Option Char)]
   [placement : Placement]))

(: compute-placed-and-leftover : (Listof Char) (Listof Char)
   -> (Pair (Listof (U Char Tile)) (Listof Char)))
;;takes a hidden word and a guess , and outputs a pair with two components:
;;A list representing the guess, where a letter that is placed is represented a
;;s a Tile, and a letter that is not placed is just represented as a Char.
;;A list of the letters from the hidden word that were not placed.
(define (compute-placed-and-leftover hidden guess)
  (local
    {(: compute-placed : (Listof Char) (Listof Char) -> (Listof (U Char Tile)))
     ;;computes the placed list
     (define (compute-placed hidden guess)
       (match guess
         ['() '()]
         [(cons 1st-char other-char)
          (if (char=? 1st-char (first hidden))
              (cons (Tile (Some 1st-char) 'placed)
                    (compute-placed (rest hidden) other-char))
              (cons 1st-char (compute-placed (rest hidden) other-char)))]))
     (: compute-leftover : (Listof Char) (Listof Char) -> (Listof Char))
     ;;computes the leftover list
     (define (compute-leftover hidden guess)
       (match hidden
         ['() '()]
         [(cons 1st-char other-char)
          (if (char=? 1st-char (first guess))
              (compute-leftover other-char (rest guess))
              (cons 1st-char (compute-leftover other-char (rest guess))))]))}
    (Pair (compute-placed hidden guess) (compute-leftover hidden guess)))) 

;;tests for compute-placed-and-leftover function
(check-expect (compute-placed-and-leftover '(#\h #\e #\a #\r #\t)
                                           '(#\s #\p #\a #\d #\e))
              (Pair (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e)
                    '(#\h #\e #\r #\t)))
(check-expect (compute-placed-and-leftover '(#\m #\o #\d #\e #\m)
                                           '(#\s #\m #\a #\r #\m))
              (Pair
               (list #\s #\m #\a #\r (Tile (Some #\m) 'placed))
               '(#\m #\o #\d #\e)))
(check-expect (compute-placed-and-leftover '(#\s #\m #\a #\r #\m)
                                           '(#\k #\b #\y #\z #\x))
              (Pair
               (list #\k #\b #\y #\z #\x)
               '(#\s #\m #\a #\r #\m)))
(check-expect (compute-placed-and-leftover '(#\m #\o #\d #\e #\m)
                                           '(#\m #\o #\d #\e #\m))
              (Pair
               (list (Tile (Some #\m) 'placed)
                     (Tile (Some #\o) 'placed)
                     (Tile (Some #\d) 'placed)
                     (Tile (Some #\e) 'placed)
                     (Tile (Some #\m) 'placed))
               '()))


(: fill-unplaced : (Listof (U Char Tile)) (Listof Char) -> (Listof Tile))
;;fill in tile positions for all unplaced characters in the guess
(define (fill-unplaced placed leftover)
  (local
    {(: remove : (Listof Char) Char -> (Listof Char))
     ;;remove a character from a list of characters
     (define (remove listofchar char)
       (match listofchar
         ['() '()]
         [(cons 1st-char other-chars)
          (if (char=? 1st-char char) other-chars
              (cons 1st-char (remove other-chars char)))]))}
    (match placed
      ['() '()]
      [(cons 1st others)
       (cond
         [(Tile? 1st) (cons 1st (fill-unplaced others leftover))]
         [(number? (index-of leftover 1st))
          (cons (Tile (Some 1st) 'in)
                (fill-unplaced others (remove leftover 1st)))]
         [else
          (cons (Tile (Some 1st) 'out) (fill-unplaced others leftover))])])))

;;tests for fill-unplaced function
(check-expect (fill-unplaced
               (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e)
               '(#\h #\e #\r #\t))
              (list
               (Tile (Some #\s) 'out)
               (Tile (Some #\p) 'out)
               (Tile (Some #\a) 'placed)
               (Tile (Some #\d) 'out)
               (Tile (Some #\e) 'in)))
(check-expect (fill-unplaced
               (list (Tile (Some #\m) 'placed)
                     (Tile (Some #\o) 'placed)
                     (Tile (Some #\d) 'placed)
                     (Tile (Some #\e) 'placed)
                     (Tile (Some #\m) 'placed))
               '())
              (list (Tile (Some #\m) 'placed)
                    (Tile (Some #\o) 'placed)
                    (Tile (Some #\d) 'placed)
                    (Tile (Some #\e) 'placed)
                    (Tile (Some #\m) 'placed)))
(check-expect (fill-unplaced
               (list #\k #\b #\y #\z #\x)
               '(#\s #\m #\a #\r #\m))
              (list (Tile (Some #\k) 'out)
                    (Tile (Some #\b) 'out)
                    (Tile (Some #\y) 'out)
                    (Tile (Some #\z) 'out)
                    (Tile (Some #\x) 'out)))

(: evaluate-guess : String String -> (Listof Tile))
;;takes in the hidden word and a guessed word, and computes the placements for
;;each letter of the guess, returning a list of tiles.
(define (evaluate-guess hidden guess)
  (local
    {(define pandl : (Pair (Listof (U Char Tile)) (Listof Char))
       (compute-placed-and-leftover
        (string->list hidden)
        (string->list guess)))}
    (fill-unplaced (Pair-fst pandl) (Pair-snd pandl))))

;;tests for evaluate-guess function
(check-expect (evaluate-guess "smarm" "mummy")
              (list
               (Tile (Some #\m) 'in)
               (Tile (Some #\u) 'out)
               (Tile (Some #\m) 'in)
               (Tile (Some #\m) 'out)
               (Tile (Some #\y) 'out)))
(check-expect (evaluate-guess "champ" "mummy")
              (list
               (Tile (Some #\m) 'out)
               (Tile (Some #\u) 'out)
               (Tile (Some #\m) 'out)
               (Tile (Some #\m) 'placed)
               (Tile (Some #\y) 'out)))
(check-expect (evaluate-guess "peeks" "brown")
              (list
               (Tile (Some #\b) 'out)
               (Tile (Some #\r) 'out)
               (Tile (Some #\o) 'out)
               (Tile (Some #\w) 'out)
               (Tile (Some #\n) 'out)))

;; (AssocList K V) is the type of an association list mapping keys of
;; type K to values of type V.
(define-type (AssocList K V) (Listof (Pair K V)))

;; (ColorSpec text-color bg-color outline-color) documents the colors
;; that should be used to draw a box with text.
(define-struct ColorSpec
  ([text-color : (Option Image-Color)] ;; If 'None, don't show text
   [bg-color : (Option Image-Color)] ;; If 'None, transparent
   [outline-color : (Option Image-Color)])) ;; If 'None, no outline

;; (BoxProps tile-width tile-height spacer-size placement-to-colorspec)
;; specifies how to draw a tile as a box. The box should have the specified
;; tile-width and tile-height, and the colors are determined by looking
;; up the tile's placement in the association list placement-to-colorspec.
;; When drawing a grid of tiles, the tile should be separated vertically
;; and horizontally by invisible spacer squares of side length spacer-size.
(define-struct BoxProps
  ([tile-width : Real]
   [tile-height : Real]
   [spacer-size : Real]
   [placement-to-colorspec : (AssocList Placement ColorSpec)]))

(: to-byte : Integer -> Byte)
;; Converts an integer to a byte (used for specifying a font size).
(define (to-byte n)
  (if (byte? n)
      n
      (error "to-byte: n is not a byte")))

(: bold-text : String Real Image-Color -> Image)
;; Makes an image of the string str using the provided
;; font-size and text color, using a bold font.
(define (bold-text str font-size text-color)
  (text/font str
             (to-byte (exact-round font-size))
             text-color                    
             "Gill Sans"                    
             'swiss                    
             'normal                    
             'bold                    
             #f))

(: find : All (K V) (AssocList K V) K (K K -> Boolean) -> (Option V))
;;takes in an association list, a search key, and a function k=? that checks if
;;two values of type K are equal, and looks up the search key in the
;;association list, returning the associated value as an optional value
(define (find assoc key function)
  (match assoc
    ['() 'None]
    [(cons 1st-item other-items)
     (if (function key (Pair-fst 1st-item))
         (Some (Pair-snd 1st-item))
         (find other-items key function))]))

;;tests for find function
(check-expect
 (find (list (Pair 1 'big)(Pair 3 'small)(Pair 4 'med)) 3 =)
 (Some 'small))
(check-expect
 (find (list (Pair 1 'big)(Pair 3 'small)(Pair 4 'med)) 5 =)
 'None)
(check-expect
 (find (list (Pair 'poo 15)(Pair 'pee 12)(Pair 'poopoo 7)(Pair 'peepee 9))
       'peepee symbol=?) (Some 9))

(: option-to-value : All (A) (Option A) -> A)
;;convert an option to a value if the option is 'None return an error
;;helper function for some of the other functions constructed below
(define (option-to-value opt)
  (if (Some? opt) (Some-value opt)
      (error "option-to-value : no value given")))

;;tests for option-to-value function
(check-expect (option-to-value (Some 'poop)) 'poop)
(check-error (option-to-value 'None) "option-to-value : no value given")
(check-expect (option-to-value (Some 12)) 12)

(: draw-tile : BoxProps Tile -> Image)
;;turns a tile into an image
(define (draw-tile props tile)
  (local
    {(define tile-colorspec : ColorSpec
       (match (find (BoxProps-placement-to-colorspec props)
                    (Tile-placement tile) symbol=?)
         ['None (error "draw-tile : no colorspec provided")]
         [(Some spec) spec]))}
    (overlay
     (match (ColorSpec-text-color tile-colorspec)
       ['None empty-image]
       [(Some txt-color) 
        (bold-text
         (match (Tile-letter tile)
           [(Some char) (string-upcase (string char))]
           [_ ""])
         (* 0.6 (min (BoxProps-tile-width props) (BoxProps-tile-height props)))
         txt-color)])
     (match (ColorSpec-outline-color tile-colorspec)
       ['None empty-image]
       [(Some out-color)
        (rectangle (BoxProps-tile-width props)
                   (BoxProps-tile-height props) "outline"
                   out-color)])
     (match (ColorSpec-bg-color tile-colorspec)
       ['None empty-image]
       [(Some back-color)
        (rectangle (BoxProps-tile-width props)
                   (BoxProps-tile-height props) "solid"
                   back-color)]))))

;;eyeball tests and check-expect tests for draw-tile function
(draw-tile
 (BoxProps 200 200 10
           (list (Pair 'in (ColorSpec ((inst Some Image-Color) "blue")
                                      ((inst Some Image-Color) "blue")
                                      ((inst Some Image-Color) "blue")))
                 (Pair 'out (ColorSpec ((inst Some Image-Color) "red")
                                       ((inst Some Image-Color) "red")
                                       ((inst Some Image-Color) "red")))
                 (Pair 'placed (ColorSpec ((inst Some Image-Color) "black")
                                          ((inst Some Image-Color) "green")
                                          ((inst Some Image-Color) "black")))))
 (Tile (Some #\x) 'placed))
(draw-tile
 (BoxProps 50 50 10
           (list (Pair 'in (ColorSpec ((inst Some Image-Color) "blue")
                                      ((inst Some Image-Color) "blue")
                                      ((inst Some Image-Color) "blue")))
                 (Pair 'out (ColorSpec ((inst Some Image-Color) "red")
                                       'None
                                       'None))
                 (Pair 'placed (ColorSpec ((inst Some Image-Color) "black")
                                          ((inst Some Image-Color) "green")
                                          ((inst Some Image-Color) "black")))))
 (Tile (Some #\y) 'out))
(check-expect
 (image-height
  (draw-tile
   (BoxProps 200 200 10
             (list
              (Pair 'in (ColorSpec ((inst Some Image-Color) "blue")
                                   ((inst Some Image-Color) "blue")
                                   ((inst Some Image-Color) "blue")))
              (Pair 'out (ColorSpec ((inst Some Image-Color) "red")
                                    ((inst Some Image-Color) "red")
                                    ((inst Some Image-Color) "red")))
              (Pair 'placed (ColorSpec ((inst Some Image-Color) "black")
                                       ((inst Some Image-Color) "green")
                                       ((inst Some Image-Color) "black")))))
   (Tile (Some #\x) 'placed))) 200)
(check-expect
 (image-width
  (draw-tile
   (BoxProps 40 29 10
             (list (Pair 'in (ColorSpec ((inst Some Image-Color) "blue")
                                        ((inst Some Image-Color) "blue")
                                        ((inst Some Image-Color) "blue")))
                   (Pair 'out (ColorSpec ((inst Some Image-Color) "red")
                                         ((inst Some Image-Color) "red")
                                         ((inst Some Image-Color) "red")))
                   (Pair 'placed
                         (ColorSpec ((inst Some Image-Color) "black")
                                    ((inst Some Image-Color) "green")
                                    ((inst Some Image-Color) "black")))))
   (Tile (Some #\x) 'placed))) 40)

(: spacer : Real -> Image)
;; Makes a fully transparent square of the given side length.
(define (spacer size) (square size spacer-opacity "red"))

(: draw-row : BoxProps (Listof Tile) -> Image)
;;turns a list of tiles into an image depicting a row of tiles
(define (draw-row props tiles)
  (foldl (lambda ([tile : Tile] [acc : Image])
           (beside acc (spacer (BoxProps-spacer-size props))
                   (draw-tile props tile)))
         (draw-tile props (first tiles)) (rest tiles)))

;;tests for draw-row function
(define standard-colors-partial : (AssocList Placement ColorSpec)
  (list (Pair 'placed (ColorSpec (Some 'white) (Some 'seagreen) 'None))
        (Pair 'in (ColorSpec (Some 'white) (Some 'goldenrod) 'None))
        (Pair 'out (ColorSpec (Some 'white) (Some 'dimgray) 'None))))

(define standard-gameboard-colors : (AssocList Placement ColorSpec)
  (cons (Pair 'unknown (ColorSpec (Some 'black) 'None (Some 'black)))
        standard-colors-partial))

(define standard-keyboard-colors : (AssocList Placement ColorSpec)
  (cons (Pair 'unknown (ColorSpec (Some 'black) (Some 'lightgray) 'None))
        standard-colors-partial))

(define standard-gameboard-props : BoxProps
  (BoxProps 50 50 5 standard-gameboard-colors))

(define standard-keyboard-props : BoxProps
  (BoxProps 24.3 40 3 standard-keyboard-colors))

(draw-row standard-gameboard-props (evaluate-guess "heart" "spade"))

(draw-row standard-keyboard-props
          (list
           (Tile (Some #\a) 'out)
           (Tile (Some #\s) 'out)
           (Tile (Some #\d) 'out)
           (Tile (Some #\f) 'placed)
           (Tile (Some #\g) 'unknown)
           (Tile (Some #\h) 'unknown)
           (Tile (Some #\j) 'unknown)
           (Tile (Some #\k) 'unknown)
           (Tile (Some #\l) 'out)))
(check-within (image-width
               (draw-row standard-keyboard-props
                         (list
                          (Tile (Some #\a) 'out)
                          (Tile (Some #\s) 'out)
                          (Tile (Some #\d) 'out)
                          (Tile (Some #\f) 'placed)
                          (Tile (Some #\g) 'unknown)
                          (Tile (Some #\h) 'unknown)
                          (Tile (Some #\j) 'unknown)
                          (Tile (Some #\k) 'unknown)
                          (Tile (Some #\l) 'out)))) (+ (* 24.3 9) (* 3 8)) 1)
(check-within (image-height
               (draw-row standard-keyboard-props
                         (list
                          (Tile (Some #\a) 'out)
                          (Tile (Some #\s) 'out)
                          (Tile (Some #\d) 'out)
                          (Tile (Some #\f) 'placed)
                          (Tile (Some #\g) 'unknown)
                          (Tile (Some #\h) 'unknown)
                          (Tile (Some #\j) 'unknown)
                          (Tile (Some #\k) 'unknown)
                          (Tile (Some #\l) 'out)))) 40 1)

(: draw-grid : BoxProps (Listof (Listof Tile)) -> Image)
;;that turns a list of list of tiles (that is, a list of rows) into an image
;;depicting a grid of tiles, using the sizes and colors specified by theBoxProps
;;value. The first row in the list should be at the bottom of the image
(define (draw-grid props list-of-row)
  (foldl
   (lambda ([row : (Listof Tile)] [acc : Image])
     (above (draw-row props row)
            (spacer (BoxProps-spacer-size props)) acc))
   (draw-row props (first list-of-row))
   (rest list-of-row)))

;;eyeball tests and check-expect tests for draw-grid function
(draw-grid standard-gameboard-props
           (list (evaluate-guess "funny" "paren")
                 (evaluate-guess "funny" "folds")
                 (evaluate-guess "funny" "typed")))
(draw-grid standard-gameboard-props
           (list (evaluate-guess "sick" "sick")
                 (evaluate-guess "sick" "pick")
                 (evaluate-guess "sick" "cots")))
(check-expect (image-height
               (draw-grid standard-gameboard-props
                          (list (evaluate-guess "bunny" "clown")
                                (evaluate-guess "bunny" "blake")
                                (evaluate-guess "bunny" "typed"))))
              160)
(check-expect (image-width
               (draw-grid standard-gameboard-props
                          (list (evaluate-guess "bunny" "clown")
                                (evaluate-guess "bunny" "blake")
                                (evaluate-guess "bunny" "typed"))))
              270)

(: make-new-keyboard-tiles : (Listof String) -> (Listof (Listof Tile)))
;;create a keyboard of tiles with the inputs as a list of string
;;each string represents a row in the keyboard
(define (make-new-keyboard-tiles string-list)
  (local
    {(: make-new-row : String -> (Listof Tile))
     ;;create a row of tiles for the make-new-keyboard function
     (define (make-new-row str)
       (map (lambda ([character : Char])
              (Tile (Some character) 'unknown))
            (string->list str)))}
    (map make-new-row string-list)))

;;tests for make-new-keyboard function
(check-expect
 (make-new-keyboard-tiles
  (list "zxcvbnm" "asdfghjkl" "qwertyuiop"))
 (list
  (list (Tile (Some #\z) 'unknown)
        (Tile (Some #\x) 'unknown)
        (Tile (Some #\c) 'unknown)
        (Tile (Some #\v) 'unknown)
        (Tile (Some #\b) 'unknown)
        (Tile (Some #\n) 'unknown)
        (Tile (Some #\m) 'unknown))
  (list
   (Tile (Some #\a) 'unknown)
   (Tile (Some #\s) 'unknown)
   (Tile (Some #\d) 'unknown)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'unknown)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown)
   (Tile (Some #\l) 'unknown))
  (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'unknown)
   (Tile (Some #\r) 'unknown)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'unknown)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'unknown)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'unknown))))
(check-expect
 (make-new-keyboard-tiles
  (list "asdfghjkl"))
 (list
  (list
   (Tile (Some #\a) 'unknown)
   (Tile (Some #\s) 'unknown)
   (Tile (Some #\d) 'unknown)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'unknown)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown)
   (Tile (Some #\l) 'unknown))))
(check-expect
 (make-new-keyboard-tiles
  (list "abc" "def" "ghijk"))
 (list
  (list (Tile (Some #\a) 'unknown)
        (Tile (Some #\b) 'unknown)
        (Tile (Some #\c) 'unknown))
  (list
   (Tile (Some #\d) 'unknown)
   (Tile (Some #\e) 'unknown)
   (Tile (Some #\f) 'unknown))
  (list
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'unknown)
   (Tile (Some #\i) 'unknown)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown))))
              

(: placement<? : Placement Placement -> Boolean)
;;determine if placement2 has higher priority than placement1
(define (placement<? place-1 place-2)
  (local
    {(: placement->integer : Placement -> Integer)
     ;;assigns a value to a placement
     (define (placement->integer place)
       (match place
         ['unknown 1]
         ['out 2]
         ['in 3]
         ['placed 4]))}
    (> (placement->integer place-2)
       (placement->integer place-1))))

;;tests for placement<? function
(check-expect (placement<? 'placed 'placed) #f)
(check-expect (placement<? 'in 'out) #f)
(check-expect (placement<? 'unknown 'placed) #t)

(: update-keyboard : Tile (Listof (Listof Tile)) -> (Listof (Listof Tile)))
;;takes in a tile that comes from a guess , and a list of list of tiles
;;representing a keyboard, and does a functional update on the keyboard,
;;incorporating the information from the guess tile
(define (update-keyboard tile keyboard)
  (local
    {(: update-row : (Listof Tile) -> (Listof Tile))
     ;;does the functional update on a single row
     (define (update-row tile-list)
       (map (lambda ([tile-of-list : Tile])
              (if (and
                   (char=? (option-to-value (Tile-letter tile))
                           (option-to-value (Tile-letter tile-of-list)))
                   (placement<? (Tile-placement tile-of-list)
                                (Tile-placement tile)))
                  tile tile-of-list)) tile-list))}
    (map update-row keyboard)))

;;tests for update-keyboard function
(check-expect
 (update-keyboard (Tile (Some #\m) 'in)
                  (make-new-keyboard-tiles
                   (list "lmn")))
 (list
  (list (Tile (Some #\l) 'unknown)
        (Tile (Some #\m) 'in)
        (Tile (Some #\n) 'unknown))))            
(check-expect
 (update-keyboard (Tile (Some #\m) 'unknown)
                  (make-new-keyboard-tiles
                   (list "lmn")))
 (list
  (list (Tile (Some #\l) 'unknown)
        (Tile (Some #\m) 'unknown)
        (Tile (Some #\n) 'unknown))))
(check-expect
 (update-keyboard (Tile (Some #\b) 'placed)
                  (make-new-keyboard-tiles
                   (list "zxcvbnm" "asdfghjkl" "qwertyuiop")))
 (list
  (list (Tile (Some #\z) 'unknown)
        (Tile (Some #\x) 'unknown)
        (Tile (Some #\c) 'unknown)
        (Tile (Some #\v) 'unknown)
        (Tile (Some #\b) 'placed)
        (Tile (Some #\n) 'unknown)
        (Tile (Some #\m) 'unknown))
  (list
   (Tile (Some #\a) 'unknown)
   (Tile (Some #\s) 'unknown)
   (Tile (Some #\d) 'unknown)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'unknown)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown)
   (Tile (Some #\l) 'unknown))
  (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'unknown)
   (Tile (Some #\r) 'unknown)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'unknown)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'unknown)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'unknown))))
              

(: multi-update-keyboard : (Listof Tile) (Listof (Listof Tile))
   -> (Listof (Listof Tile)))
;;takes a list of tiles coming from guesses (the "guess tiles"),
;;and a list of list of tiles representing a keyboard, and does a
;;functional update on the keyboard, incorporating each guess tile.
(define (multi-update-keyboard guesses keyboard)
  (foldl update-keyboard keyboard guesses))

;;tests for multi-update-keyboard function
(check-expect
 (multi-update-keyboard
  (list (Tile (Some #\m) 'in)
        (Tile (Some #\r) 'out)
        (Tile (Some #\p) 'placed))
  (make-new-keyboard-tiles
   (list "zxcvbnm" "asdfghjkl" "qwertyuiop")))
 (list
  (list (Tile (Some #\z) 'unknown)
        (Tile (Some #\x) 'unknown)
        (Tile (Some #\c) 'unknown)
        (Tile (Some #\v) 'unknown)
        (Tile (Some #\b) 'unknown) 
        (Tile (Some #\n) 'unknown)
        (Tile (Some #\m) 'in))
  (list
   (Tile (Some #\a) 'unknown)
   (Tile (Some #\s) 'unknown)
   (Tile (Some #\d) 'unknown)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'unknown)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown)
   (Tile (Some #\l) 'unknown))
  (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'unknown)
   (Tile (Some #\r) 'out)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'unknown)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'unknown)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'placed))))
(check-expect
 (multi-update-keyboard
  (list (Tile (Some #\m) 'unknown)
        (Tile (Some #\h) 'placed)
        (Tile (Some #\a) 'unknown)
        (Tile (Some #\n) 'out))
  (make-new-keyboard-tiles
   (list "zxcvbnm" "asdfghjkl" "qwertyuiop")))
 (list
  (list (Tile (Some #\z) 'unknown)
        (Tile (Some #\x) 'unknown)
        (Tile (Some #\c) 'unknown)
        (Tile (Some #\v) 'unknown)
        (Tile (Some #\b) 'unknown)
        (Tile (Some #\n) 'out)
        (Tile (Some #\m) 'unknown))
  (list
   (Tile (Some #\a) 'unknown)
   (Tile (Some #\s) 'unknown)
   (Tile (Some #\d) 'unknown)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'placed)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown)
   (Tile (Some #\l) 'unknown))
  (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'unknown)
   (Tile (Some #\r) 'unknown)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'unknown)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'unknown)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'unknown))))
(check-expect
 (multi-update-keyboard
  (list (Tile (Some #\f) 'unknown)
        (Tile (Some #\e) 'out)
        (Tile (Some #\a) 'in)
        (Tile (Some #\l) 'placed)
        (Tile (Some #\k) 'out)
        (Tile (Some #\d) 'out))
  (make-new-keyboard-tiles
   (list "flaked")))
 (list
  (list (Tile (Some #\f) 'unknown)
        (Tile (Some #\l) 'placed)
        (Tile (Some #\a) 'in)
        (Tile (Some #\k) 'out)
        (Tile (Some #\e) 'out)
        (Tile (Some #\d) 'out))))

(: string-cmp : (Cmp String))
;; Comparison on strings
(define (string-cmp s1 s2)
  (cond
    [(string<? s1 s2) '<]
    [(string>? s1 s2) '>]
    [else '=]))

(: string-list->set : (Listof String) -> (Set String))
;;converts a list of strings to a set
(define string-list->set
  (make-list->set string-cmp))

;;tests for string-list->set function
(define test-set-1 (string-list->set '("a" "b" "c")))
(check-expect (member? "a" test-set-1) #t)
(check-expect (member? "A" test-set-1) #f)
(check-expect (member? "c" test-set-1) #t)
(check-expect (member? "d" test-set-1) #f)
(define test-set-2 (string-list->set '("pee" "poop" "poo" "123")))
(check-expect (member? "pee" test-set-2) #t)
(check-expect (member? "452" test-set-2) #f)
(check-expect (member? "123" test-set-2) #t)
(check-expect (member? "poop" test-set-2) #t)

;; The parameters of a Racketle game.
(define-struct GameParams
  ([title : String]
   [wordlength : Natural]
   [max-guesses : Natural]
   [hidden-set : (Set String)]
   [guess-set : (Set String)]))

;; The current state of the game.
(define-struct Game
  ([params : GameParams]
   [hidden : String]
   [past-guesses : (Listof (Listof Tile))]
   [current-input : String]
   [keyboard : (Listof (Listof Tile))]
   [message : String]
   [ongoing? : Boolean]))

(: make-new-game : GameParams (Listof String) -> Game)
;;takes in a GameParams and a list of strings (the letters of the keyboard)
;;and returns a new value of type Game where the hidden word has been randomly
;;chosen from possible hidden words, there are no past guesses,the current
;;input is empty, the message is blank, and the game is currently ongoing.
(define (make-new-game params keyboard)
  (Game params
        (gen-random-element (GameParams-hidden-set params))
        '()
        ""
        (make-new-keyboard-tiles keyboard) "" #t))

;;tests for make-new-game function. these are eyeball tests since check-expect
;;tests will not work for random output (the hidden word)
(make-new-game
 (GameParams "racktle" 3 5 (string-list->set '("hey" "she" "ban" "sue"))
             (string-list->set '("hey" "she")))
 (list "zxcvbnm" "asdfghjkl" "qwertyuiop"))
(make-new-game
 (GameParams "racktle" 1 1 test-set-1
             (string-list->set '("a")))
 (list "a" "b" "c" "d"))

(: restart-game : Game -> Game)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Restarts the game, choosing a new random hidden word and clearing
;; out all guesses.
(define (restart-game game)
  (Game
   (Game-params game)
   (gen-random-element (GameParams-hidden-set (Game-params game)))
   '()
   ""
   (map (lambda ([row : (Listof Tile)])
          (map (lambda ([tile : Tile]) (Tile (Tile-letter tile) 'unknown)) row))
        (Game-keyboard game))
   ""
   #t))

(: backspace : Game -> Game)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Updates the state of the game based on the user pressing the
;; backspace key.
(define (backspace game)
  (match game
    [(Game _ _ _ _ _ _ #f) game]
    [(Game params hidden past-guesses current-input keyboard _ #t)
     (Game params
           hidden
           past-guesses
           (if (> (string-length current-input) 0)
               (substring current-input 0 (- (string-length current-input) 1))
               current-input)
           keyboard
           ""
           #t)]))

(: type-letter : Game String -> Game)
;;that does a functional update on the game, updating the state of the game
;;by what would happen if a player pressed a letter on the keyboard.
(define (type-letter game input)
  (match game
    [(Game _ _ _ _ _ _ #f) game]
    [(Game params hidden past-guesses current-input keyboard _ #t)
     (if (>= (string-length current-input) (GameParams-wordlength params))
         (Game params hidden past-guesses current-input keyboard
               "Max characters reached!" #t)
         (Game params hidden past-guesses (string-append current-input input)
               keyboard "" #t))]))

;;tests for type-letter function
(define test-params-1
  (GameParams "racktle" 3 3 (string-list->set '("hey" "she"))
              (string-list->set '("hey" "she" "ban" "sue"))))
(check-expect
 (type-letter (Game test-params-1 "she" '() "he"
                    (list
                     (list (Tile (Some #\h) 'unknown)
                           (Tile (Some #\e) 'unknown)
                           (Tile (Some #\y) 'unknown)
                           (Tile (Some #\s) 'unknown)
                           (Tile (Some #\h) 'unknown)
                           (Tile (Some #\b) 'unknown)
                           (Tile (Some #\a) 'unknown))
                     (list
                      (Tile (Some #\n) 'unknown))) "" #t) "y")
 (Game test-params-1 "she" '() "hey"
       (list
        (list (Tile (Some #\h) 'unknown)
              (Tile (Some #\e) 'unknown)
              (Tile (Some #\y) 'unknown)
              (Tile (Some #\s) 'unknown)
              (Tile (Some #\h) 'unknown)
              (Tile (Some #\b) 'unknown)
              (Tile (Some #\a) 'unknown))
        (list
         (Tile (Some #\n) 'unknown))) "" #t))
(check-expect
 (type-letter (Game test-params-1 "she" '() ""
                    (list
                     (list (Tile (Some #\h) 'unknown)
                           (Tile (Some #\e) 'unknown)
                           (Tile (Some #\y) 'unknown)
                           (Tile (Some #\s) 'unknown)
                           (Tile (Some #\h) 'unknown)
                           (Tile (Some #\b) 'unknown)
                           (Tile (Some #\a) 'unknown))
                     (list
                      (Tile (Some #\n) 'unknown)
                      (Tile (Some #\g) 'unknown)
                      (Tile (Some #\z) 'unknown)
                      (Tile (Some #\u) 'unknown)
                      (Tile (Some #\v) 'unknown))) "seer" #f) "e")
 (Game test-params-1 "she" '() ""
       (list
        (list (Tile (Some #\h) 'unknown)
              (Tile (Some #\e) 'unknown)
              (Tile (Some #\y) 'unknown)
              (Tile (Some #\s) 'unknown)
              (Tile (Some #\h) 'unknown)
              (Tile (Some #\b) 'unknown)
              (Tile (Some #\a) 'unknown))
        (list
         (Tile (Some #\n) 'unknown)
         (Tile (Some #\g) 'unknown)
         (Tile (Some #\z) 'unknown)
         (Tile (Some #\u) 'unknown)
         (Tile (Some #\v) 'unknown))) "seer" #f))
(check-expect
 (type-letter (Game test-params-1 "she"
                    (list
                     (list (Tile (Some #\h) 'unknown)
                           (Tile (Some #\e) 'unknown)
                           (Tile (Some #\y) 'unknown)))
                    "heh"
                    (list
                     (list (Tile (Some #\h) 'unknown)
                           (Tile (Some #\e) 'unknown)
                           (Tile (Some #\y) 'unknown)
                           (Tile (Some #\s) 'unknown)
                           (Tile (Some #\h) 'unknown)
                           (Tile (Some #\b) 'unknown)
                           (Tile (Some #\a) 'unknown))
                     (list
                      (Tile (Some #\n) 'unknown)))
                    "" #t) "e")
 (Game test-params-1 "she"
       (list
        (list (Tile (Some #\h) 'unknown)
              (Tile (Some #\e) 'unknown)
              (Tile (Some #\y) 'unknown)))
       "heh"
       (list
        (list (Tile (Some #\h) 'unknown)
              (Tile (Some #\e) 'unknown)
              (Tile (Some #\y) 'unknown)
              (Tile (Some #\s) 'unknown)
              (Tile (Some #\h) 'unknown)
              (Tile (Some #\b) 'unknown)
              (Tile (Some #\a) 'unknown))
        (list
         (Tile (Some #\n) 'unknown))) "Max characters reached!" #t))

(: game-validate-input : Game -> (Pair Boolean String))
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Determines whether the current-input of the game is a valid guess.
;; Returns a pair with a boolean indicating whether it is a valid
;; guess, and a string consisting of what the message should become
;; if the user attempts to submit this guess.
(define (game-validate-input game)
  (match game
    [(Game (GameParams _ wordlength max-guesses _ guess-set)
           _ past-guesses current-input _ old-message ongoing?)
     (cond
       [(> (string-length current-input) wordlength)
        (error "game-validate-input: current-input longer than wordlength")]
       [(not ongoing?) (Pair #f old-message)]
       [(>= (length past-guesses) max-guesses)
        (error "game-validate-input: out of guesses but still ongoing")]
       [(< (string-length current-input) wordlength) (Pair #f "")]
       [(not (member? current-input guess-set))
        (Pair #f "Not in word list")]
       [else (Pair #t "")])]))

(: submit-guess : Game -> Game)
;;checks if the current-input is a valid guess
(define (submit-guess game)
  (match game
    [(Game params hidden past-guesses current-input old-keyboard
           old-message ongoing?)
     (local
       {(define validity (game-validate-input game))
        (define correct? (string=? current-input hidden))
        (define lost? (and (not correct?)
                           (>= (length past-guesses)
                               (- (GameParams-max-guesses params) 1))))}
       (cond
         [(not (Pair-fst validity))
          (Game params hidden past-guesses current-input old-keyboard
                (Pair-snd validity) ongoing?)]
         [correct?
          (Game params hidden
                (cons (evaluate-guess hidden current-input) past-guesses) ""
                (multi-update-keyboard (evaluate-guess hidden current-input)
                                       old-keyboard) "You win!" #f)]
         [lost?
          (Game params hidden
                (cons (evaluate-guess hidden current-input) past-guesses) ""
                (multi-update-keyboard (evaluate-guess hidden current-input)
                                       old-keyboard)
                (string-append "Game over. The word was " hidden ".") #f)]
         [else
          (Game params hidden
                (cons (evaluate-guess hidden current-input) past-guesses) ""
                (multi-update-keyboard (evaluate-guess hidden current-input)
                                       old-keyboard)
                (Pair-snd validity) #t)]))]))

;;tests for submit-guess function
(check-expect
 (submit-guess
  (Game test-params-1 "she" '() "hey"
        (list
         (list (Tile (Some #\h) 'unknown)
               (Tile (Some #\e) 'unknown)
               (Tile (Some #\y) 'unknown)
               (Tile (Some #\s) 'unknown)
               (Tile (Some #\b) 'unknown)
               (Tile (Some #\a) 'unknown))
         (list
          (Tile (Some #\n) 'unknown))) "" #t))
 (Game test-params-1 "she"
       (list
        (list (Tile (Some #\h) 'in)
              (Tile (Some #\e) 'in)
              (Tile (Some #\y) 'out))) ""
                                       (list
                                        (list (Tile (Some #\h) 'in)
                                              (Tile (Some #\e) 'in)
                                              (Tile (Some #\y) 'out)
                                              (Tile (Some #\s) 'unknown)
                                              (Tile (Some #\b) 'unknown)
                                              (Tile (Some #\a) 'unknown))
                                        (list
                                         (Tile (Some #\n) 'unknown))) "" #t))
(check-expect
 (submit-guess
  (Game test-params-1 "she" '() "ysb"
        (list
         (list (Tile (Some #\h) 'unknown)
               (Tile (Some #\e) 'unknown)
               (Tile (Some #\y) 'unknown)
               (Tile (Some #\s) 'unknown)
               (Tile (Some #\b) 'unknown)
               (Tile (Some #\a) 'unknown))
         (list
          (Tile (Some #\n) 'unknown))) "" #t))
 (Game test-params-1 "she" '() "ysb"
       (list
        (list (Tile (Some #\h) 'unknown)
              (Tile (Some #\e) 'unknown)
              (Tile (Some #\y) 'unknown)
              (Tile (Some #\s) 'unknown)
              (Tile (Some #\b) 'unknown)
              (Tile (Some #\a) 'unknown))
        (list
         (Tile (Some #\n) 'unknown))) "Not in word list" #t))
(check-expect
 (submit-guess
  (Game test-params-1 "she" '() "she"
        (list
         (list (Tile (Some #\h) 'unknown)
               (Tile (Some #\e) 'unknown)
               (Tile (Some #\y) 'unknown)
               (Tile (Some #\s) 'unknown)
               (Tile (Some #\b) 'unknown)
               (Tile (Some #\a) 'unknown))
         (list
          (Tile (Some #\n) 'unknown))) "" #t))
 (Game test-params-1 "she"
       (list
        (list (Tile (Some #\s) 'placed)
              (Tile (Some #\h) 'placed)
              (Tile (Some #\e) 'placed)))
       ""
       (list
        (list (Tile (Some #\h) 'placed)
              (Tile (Some #\e) 'placed)
              (Tile (Some #\y) 'unknown)
              (Tile (Some #\s) 'placed)
              (Tile (Some #\b) 'unknown)
              (Tile (Some #\a) 'unknown))
        (list
         (Tile (Some #\n) 'unknown))) "You win!" #f))
(check-expect
 (submit-guess
  (Game test-params-1 "she"
        (list
         (list (Tile (Some #\b) 'out)
               (Tile (Some #\a) 'out)
               (Tile (Some #\n) 'out))
         (list (Tile (Some #\h) 'in)
               (Tile (Some #\e) 'in)
               (Tile (Some #\y) 'out)))
        "sue"
        (list
         (list (Tile (Some #\h) 'in)
               (Tile (Some #\e) 'in)
               (Tile (Some #\y) 'out)
               (Tile (Some #\s) 'unknown)
               (Tile (Some #\b) 'out)
               (Tile (Some #\a) 'out)
               (Tile (Some #\u) 'unknown))
         (list
          (Tile (Some #\n) 'out))) "" #t))
 (Game test-params-1 "she"
       (list
        (list (Tile (Some #\s) 'placed)
              (Tile (Some #\u) 'out)
              (Tile (Some #\e) 'placed))
        (list (Tile (Some #\b) 'out)
              (Tile (Some #\a) 'out)
              (Tile (Some #\n) 'out))
        (list (Tile (Some #\h) 'in)
              (Tile (Some #\e) 'in)
              (Tile (Some #\y) 'out)))
       ""
       (list
        (list (Tile (Some #\h) 'in)
              (Tile (Some #\e) 'placed)
              (Tile (Some #\y) 'out)
              (Tile (Some #\s) 'placed)
              (Tile (Some #\b) 'out)
              (Tile (Some #\a) 'out)
              (Tile (Some #\u) 'out))
        (list
         (Tile (Some #\n) 'out))) "Game over. The word was she." #f))

(: row-of-unknown : Natural String -> (Listof Tile))
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Makes a list of wordlength tiles with placement 'unknown,
;; where the letters in the string fill the first tiles in the
;; list, and the remaining tiles are empty).
(define (row-of-unknown wordlength current-input)
  (append (map (lambda ([letter : Char])
                 (Tile (Some letter) 'unknown))
               (string->list current-input))
          (make-list (max 0 (- wordlength (string-length current-input)))
                     (Tile 'None 'unknown))))

(: assemble-gameboard : Natural Natural (Listof (Listof Tile)) String
   -> (Listof (Listof Tile)))
;;takes in wordlength, max-guesses, past-guesses, and current-input,
;;and assembles the list of list of tiles representing the gameboard.
;;returns past-guesses if the number of past guesses = guesses allowed
(define (assemble-gameboard wordlength max-guesses past-guesses current-input)
  (cond [(= (length past-guesses) (- max-guesses 1))
         (cons (row-of-unknown wordlength current-input) past-guesses)]
        [(>= (length past-guesses) max-guesses) past-guesses]
        [else
         (assemble-gameboard wordlength max-guesses 
                             (cons (row-of-unknown wordlength current-input)
                                   past-guesses) "")]))

;;tests for assemble-gameboard function
(check-expect
 (assemble-gameboard 4 4 '() "hi")
 (list
  (list (Tile 'None 'unknown) (Tile 'None 'unknown)
        (Tile 'None 'unknown) (Tile 'None 'unknown))
  (list (Tile 'None 'unknown) (Tile 'None 'unknown)
        (Tile 'None 'unknown) (Tile 'None 'unknown))
  (list (Tile 'None 'unknown) (Tile 'None 'unknown)
        (Tile 'None 'unknown) (Tile 'None 'unknown))
  (list (Tile (Some #\h) 'unknown) (Tile (Some #\i) 'unknown)
        (Tile 'None 'unknown) (Tile 'None 'unknown))))
(check-expect
 (assemble-gameboard
  2 3
  (list
   (list (Tile (Some #\i) 'out) (Tile (Some #\n) 'out))
   (list (Tile (Some #\h) 'placed) (Tile (Some #\e) 'out))) "ha")
 (list (list (Tile (Some #\h) 'unknown) (Tile (Some #\a) 'unknown))
       (list (Tile (Some #\i) 'out) (Tile (Some #\n) 'out))
       (list (Tile (Some #\h) 'placed) (Tile (Some #\e) 'out))))
(check-expect
 (assemble-gameboard
  2 3
  (list (list (Tile (Some #\h) 'unknown) (Tile (Some #\a) 'unknown))
        (list (Tile (Some #\i) 'out) (Tile (Some #\n) 'out))
        (list (Tile (Some #\h) 'placed) (Tile (Some #\e) 'out))) "le")
 (list (list (Tile (Some #\h) 'unknown) (Tile (Some #\a) 'unknown))
       (list (Tile (Some #\i) 'out) (Tile (Some #\n) 'out))
       (list (Tile (Some #\h) 'placed) (Tile (Some #\e) 'out))))

;; /// World type definition provided
;; Keeps track of a Game as well as the size and color
;; properties needed to draw the game.
(define-struct WordWorld
  ([game : Game]
   [general-props : GeneralProps]
   [gameboard-props : BoxProps]
   [keyboard-props : BoxProps]))

;; General properties for drawing a world.
(define-struct GeneralProps
  ([width : Real]
   [margin : Real]
   [header-height : Real]
   [message-height : Real]
   [text-color : Image-Color]
   [bg-color : Image-Color]))

(: draw-world : WordWorld -> Image)
;;draws a world
(define (draw-world wordwrld)
  (match wordwrld
    [(WordWorld (Game (GameParams title wordlength max-guesses hidden-set
                                  guess-set)
                      hidden past-guesses current-input keyboard message _)
                (GeneralProps width margin header-height message-height
                              text-color bg-color)
                gameboard-props keyboard-props)
     (local
       {(define 4-stack-image
          (beside
           (spacer margin)
           (above
            (spacer margin)
            (overlay
             (text title
                   (to-byte (exact-round (* 0.8 header-height))) text-color)
             (rectangle width header-height "solid" bg-color))
            (draw-grid gameboard-props
                       (assemble-gameboard wordlength max-guesses past-guesses
                                           current-input))
            (overlay
             (text message
                   (to-byte (exact-round (* 0.5 header-height))) text-color)
             (rectangle width header-height "solid" bg-color))
            (draw-grid keyboard-props keyboard)
            (spacer margin))
           (spacer margin)))
        (define 4-stack-height (image-height 4-stack-image))
        (define 4-stack-width (image-width 4-stack-image))}
       (overlay
        4-stack-image
        (rectangle 4-stack-width 4-stack-height "solid" bg-color)))]))

;;tests for draw-world function (both eyeball tests and image-heigth and width
;;check-expect tests)
(define instructor-gameparams-example
  (GameParams "Racktle" 5 6
              (string-list->set '("funny" "sunny"))
              (string-list->set '("typed" "folds" "paren" "poops"
                                          "funny" "sunny"))))
(define test-world-image
  (draw-world
   (WordWorld (Game instructor-gameparams-example "funny"
                    (list (evaluate-guess "funny" "typed")
                          (evaluate-guess "funny" "folds")
                          (evaluate-guess "funny" "paren"))
                    "funct"
                    (list
                     (list (Tile (Some #\z) 'unknown)
                           (Tile (Some #\x) 'unknown)
                           (Tile (Some #\c) 'unknown)
                           (Tile (Some #\v) 'unknown)
                           (Tile (Some #\b) 'unknown)
                           (Tile (Some #\n) 'in)
                           (Tile (Some #\m) 'unknown))
                     (list
                      (Tile (Some #\a) 'out)
                      (Tile (Some #\s) 'out)
                      (Tile (Some #\d) 'out)
                      (Tile (Some #\f) 'placed)
                      (Tile (Some #\g) 'unknown)
                      (Tile (Some #\h) 'unknown)
                      (Tile (Some #\j) 'unknown)
                      (Tile (Some #\k) 'unknown)
                      (Tile (Some #\l) 'out))
                     (list
                      (Tile (Some #\q) 'unknown)
                      (Tile (Some #\w) 'unknown)
                      (Tile (Some #\e) 'out)
                      (Tile (Some #\r) 'out)
                      (Tile (Some #\t) 'out)
                      (Tile (Some #\y) 'in)
                      (Tile (Some #\u) 'unknown)
                      (Tile (Some #\i) 'unknown)
                      (Tile (Some #\o) 'out)
                      (Tile (Some #\p) 'out))) "Not in word list" #t)
              (GeneralProps 270 20 50 25 'royalblue 'lightblue)
              standard-gameboard-props standard-keyboard-props)))
test-world-image
(check-expect
 (image-width test-world-image) (+ 270 40))
(check-expect
 (image-height test-world-image) (+ 325 140 6 120))


(: world-replace-game : WordWorld Game -> WordWorld)
;;a functional update on a world, replacing the world's game with the given one
(define (world-replace-game world new-game)
  (match world
    [(WordWorld _ general-props gameboard-props keyboard-props)
     (WordWorld new-game general-props gameboard-props keyboard-props)]))

;;tests for world-replace-game function
(define new-test-game-1
  (Game instructor-gameparams-example "funny"
        (list (evaluate-guess "funny" "typed")
              (evaluate-guess "funny" "folds")
              (evaluate-guess "funny" "paren"))
        "funct"
        (list
         (list (Tile (Some #\z) 'unknown)
               (Tile (Some #\x) 'unknown)
               (Tile (Some #\c) 'unknown)
               (Tile (Some #\v) 'unknown)
               (Tile (Some #\b) 'unknown)
               (Tile (Some #\n) 'in)
               (Tile (Some #\m) 'unknown))
         (list
          (Tile (Some #\a) 'out)
          (Tile (Some #\s) 'out)
          (Tile (Some #\d) 'out)
          (Tile (Some #\f) 'placed)
          (Tile (Some #\g) 'unknown)
          (Tile (Some #\h) 'unknown)
          (Tile (Some #\j) 'unknown)
          (Tile (Some #\k) 'unknown)
          (Tile (Some #\l) 'out))
         (list
          (Tile (Some #\q) 'unknown)
          (Tile (Some #\w) 'unknown)
          (Tile (Some #\e) 'out)
          (Tile (Some #\r) 'out)
          (Tile (Some #\t) 'out)
          (Tile (Some #\y) 'in)
          (Tile (Some #\u) 'unknown)
          (Tile (Some #\i) 'unknown)
          (Tile (Some #\o) 'out)
          (Tile (Some #\p) 'out))) "Not in word list" #t))
(check-expect
 (world-replace-game
  (WordWorld
   (Game test-params-1 "she" '() "she"
         (list
          (list (Tile (Some #\h) 'unknown)
                (Tile (Some #\e) 'unknown)
                (Tile (Some #\y) 'unknown)
                (Tile (Some #\s) 'unknown)
                (Tile (Some #\b) 'unknown)
                (Tile (Some #\a) 'unknown))
          (list
           (Tile (Some #\n) 'unknown))) "" #t)
   (GeneralProps 270 20 50 25 'royalblue 'lightblue)
   standard-gameboard-props standard-keyboard-props)
  new-test-game-1)
 (WordWorld new-test-game-1
            (GeneralProps 270 20 50 25 'royalblue 'lightblue)
            standard-gameboard-props standard-keyboard-props))
(check-expect
 (world-replace-game
  (WordWorld new-test-game-1
             (GeneralProps 270 20 50 25 'royalblue 'lightblue)
             standard-gameboard-props standard-keyboard-props)
  (Game test-params-1 "she" '() "she"
        (list
         (list (Tile (Some #\h) 'unknown)
               (Tile (Some #\e) 'unknown)
               (Tile (Some #\y) 'unknown)
               (Tile (Some #\s) 'unknown)
               (Tile (Some #\b) 'unknown)
               (Tile (Some #\a) 'unknown))
         (list
          (Tile (Some #\n) 'unknown))) "" #t))
 (WordWorld
  (Game test-params-1 "she" '() "she"
        (list
         (list (Tile (Some #\h) 'unknown)
               (Tile (Some #\e) 'unknown)
               (Tile (Some #\y) 'unknown)
               (Tile (Some #\s) 'unknown)
               (Tile (Some #\b) 'unknown)
               (Tile (Some #\a) 'unknown))
         (list
          (Tile (Some #\n) 'unknown))) "" #t)
  (GeneralProps 270 20 50 25 'royalblue 'lightblue)
  standard-gameboard-props standard-keyboard-props))

(check-expect
 (world-replace-game
  (WordWorld
   (Game test-params-1 "she" '() "she"
         (list
          (list (Tile (Some #\h) 'unknown)
                (Tile (Some #\e) 'unknown)
                (Tile (Some #\y) 'unknown)
                (Tile (Some #\s) 'unknown)
                (Tile (Some #\b) 'unknown)
                (Tile (Some #\a) 'unknown))
          (list
           (Tile (Some #\n) 'unknown))) "" #t)
   (GeneralProps 270 20 50 25 'royalblue 'lightblue)
   standard-gameboard-props standard-keyboard-props)
  (Game
   (GameParams "racktle" 1 1 test-set-1 test-set-2)
   "c" '() ""
   (list (list (Tile (Some #\a) 'unknown)) (list (Tile (Some #\b) 'unknown))
         (list (Tile (Some #\c) 'unknown)) (list (Tile (Some #\d) 'unknown)))
   "" #t))
 (WordWorld
  (Game
   (GameParams "racktle" 1 1 test-set-1 test-set-2)
   "c" '() ""
   (list (list (Tile (Some #\a) 'unknown)) (list (Tile (Some #\b) 'unknown))
         (list (Tile (Some #\c) 'unknown)) (list (Tile (Some #\d) 'unknown)))
   "" #t)
  (GeneralProps 270 20 50 25 'royalblue 'lightblue)
  standard-gameboard-props standard-keyboard-props))
 

(: is-letter? : String -> Boolean)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Checks if the given string is a letter.
(define (is-letter? str)
  (and (= (string-length str) 1)
       (or (string<=? "a" str "z") (string<=? "A" str "Z"))))

(: react-to-keyboard : WordWorld String -> WordWorld)
;;does a functional update on a world. This function takes in keyboard
;;input and updates the world based on that input.
(define (react-to-keyboard world key)
  (match world
    [(WordWorld game gen-props gb-props kb-props)
     (cond
       [(key=? key "\b")
        (WordWorld (backspace game) gen-props gb-props kb-props)]
       [(key=? key "\r")
        (WordWorld (submit-guess game) gen-props gb-props kb-props)]
       [(key=? key "escape")
        (WordWorld (restart-game game) gen-props gb-props kb-props)]
       [(is-letter? key)
        (WordWorld (type-letter game (string-downcase key)) gen-props
                   gb-props kb-props)]
       [else world])]))

;;tests for react-to-keyboard function. the "escape" input case cannot
;;be tested because it generates a random word that we cannot predict
;;I test this later using the run function
(define test-world-1
  (WordWorld new-test-game-1 (GeneralProps 220 20 40 25 'pink 'orange)
             standard-gameboard-props standard-keyboard-props))
(check-expect
 (react-to-keyboard test-world-1 "\b")
 (WordWorld
  (Game
   instructor-gameparams-example
   "funny"
   (list
    (list (Tile (Some #\t) 'out) (Tile (Some #\y) 'in)
          (Tile (Some #\p) 'out) (Tile (Some #\e) 'out) (Tile (Some #\d) 'out))
    (list (Tile (Some #\f) 'placed) (Tile (Some #\o) 'out)
          (Tile (Some #\l) 'out) (Tile (Some #\d) 'out) (Tile (Some #\s) 'out))
    (list (Tile (Some #\p) 'out) (Tile (Some #\a) 'out)
          (Tile (Some #\r) 'out) (Tile (Some #\e) 'out) (Tile (Some #\n) 'in)))
   "func"
   (list
    (list (Tile (Some #\z) 'unknown) (Tile (Some #\x) 'unknown)
          (Tile (Some #\c) 'unknown) (Tile (Some #\v) 'unknown)
          (Tile (Some #\b) 'unknown) (Tile (Some #\n) 'in)
          (Tile (Some #\m) 'unknown))
    (list (Tile (Some #\a) 'out) (Tile (Some #\s) 'out)
          (Tile (Some #\d) 'out) (Tile (Some #\f) 'placed)
          (Tile (Some #\g) 'unknown) (Tile (Some #\h) 'unknown)
          (Tile (Some #\j) 'unknown) (Tile (Some #\k) 'unknown)
          (Tile (Some #\l) 'out))
    (list
     (Tile (Some #\q) 'unknown) (Tile (Some #\w) 'unknown)
     (Tile (Some #\e) 'out) (Tile (Some #\r) 'out)
     (Tile (Some #\t) 'out) (Tile (Some #\y) 'in)
     (Tile (Some #\u) 'unknown) (Tile (Some #\i) 'unknown)
     (Tile (Some #\o) 'out) (Tile (Some #\p) 'out)))
   ""
   #t)
  (GeneralProps 220 20 40 25 'pink 'orange)
  standard-gameboard-props standard-keyboard-props))

(check-expect
 (react-to-keyboard test-world-1 "\r")
 (WordWorld
  (Game
   instructor-gameparams-example
   "funny"
   (list
    (list (Tile (Some #\t) 'out) (Tile (Some #\y) 'in) (Tile (Some #\p) 'out)
          (Tile (Some #\e) 'out) (Tile (Some #\d) 'out))
    (list (Tile (Some #\f) 'placed) (Tile (Some #\o) 'out)
          (Tile (Some #\l) 'out) (Tile (Some #\d) 'out) (Tile (Some #\s) 'out))
    (list (Tile (Some #\p) 'out) (Tile (Some #\a) 'out) (Tile (Some #\r) 'out)
          (Tile (Some #\e) 'out) (Tile (Some #\n) 'in)))
   "funct"
   (list
    (list (Tile (Some #\z) 'unknown) (Tile (Some #\x) 'unknown)
          (Tile (Some #\c) 'unknown) (Tile (Some #\v) 'unknown)
          (Tile (Some #\b) 'unknown) (Tile (Some #\n) 'in)
          (Tile (Some #\m) 'unknown))
    (list (Tile (Some #\a) 'out) (Tile (Some #\s) 'out)
          (Tile (Some #\d) 'out) (Tile (Some #\f) 'placed)
          (Tile (Some #\g) 'unknown) (Tile (Some #\h) 'unknown)
          (Tile (Some #\j) 'unknown) (Tile (Some #\k) 'unknown)
          (Tile (Some #\l) 'out))
    (list
     (Tile (Some #\q) 'unknown) (Tile (Some #\w) 'unknown)
     (Tile (Some #\e) 'out) (Tile (Some #\r) 'out)
     (Tile (Some #\t) 'out) (Tile (Some #\y) 'in)
     (Tile (Some #\u) 'unknown) (Tile (Some #\i) 'unknown)
     (Tile (Some #\o) 'out) (Tile (Some #\p) 'out)))
   "Not in word list"
   #t)
  (GeneralProps 220 20 40 25 'pink 'orange)
  standard-gameboard-props standard-keyboard-props))

(check-expect
 (react-to-keyboard test-world-1 "b")
 (WordWorld
  (Game
   instructor-gameparams-example
   "funny"
   (list
    (list (Tile (Some #\t) 'out) (Tile (Some #\y) 'in) (Tile (Some #\p) 'out)
          (Tile (Some #\e) 'out) (Tile (Some #\d) 'out))
    (list (Tile (Some #\f) 'placed) (Tile (Some #\o) 'out)
          (Tile (Some #\l) 'out) (Tile (Some #\d) 'out) (Tile (Some #\s) 'out))
    (list (Tile (Some #\p) 'out) (Tile (Some #\a) 'out) (Tile (Some #\r) 'out)
          (Tile (Some #\e) 'out) (Tile (Some #\n) 'in)))
   "funct"
   (list
    (list (Tile (Some #\z) 'unknown) (Tile (Some #\x) 'unknown)
          (Tile (Some #\c) 'unknown) (Tile (Some #\v) 'unknown)
          (Tile (Some #\b) 'unknown) (Tile (Some #\n) 'in)
          (Tile (Some #\m) 'unknown))
    (list (Tile (Some #\a) 'out) (Tile (Some #\s) 'out)
          (Tile (Some #\d) 'out) (Tile (Some #\f) 'placed)
          (Tile (Some #\g) 'unknown) (Tile (Some #\h) 'unknown)
          (Tile (Some #\j) 'unknown) (Tile (Some #\k) 'unknown)
          (Tile (Some #\l) 'out))
    (list
     (Tile (Some #\q) 'unknown) (Tile (Some #\w) 'unknown)
     (Tile (Some #\e) 'out) (Tile (Some #\r) 'out)
     (Tile (Some #\t) 'out) (Tile (Some #\y) 'in)
     (Tile (Some #\u) 'unknown) (Tile (Some #\i) 'unknown)
     (Tile (Some #\o) 'out) (Tile (Some #\p) 'out)))
   "Max characters reached!"
   #t)
  (GeneralProps 220 20 40 25 'pink 'orange)
  standard-gameboard-props standard-keyboard-props))

(check-expect
 (react-to-keyboard test-world-1 "%")
 test-world-1)

(: run : GameParams (Listof String) GeneralProps BoxProps BoxProps -> Image)
;;starts a new interactive game using big-bang
(define (run params keyboard-letters gen-props gb-props kb-props)
  (draw-world
   (big-bang
       (WordWorld (make-new-game params keyboard-letters) gen-props
                  gb-props kb-props) : WordWorld
     [to-draw draw-world]
     [on-key react-to-keyboard]
     [name (GameParams-title params)])))

;;tests for run function ****tests are now removed since we do not want to run
;;the program upon initially running this file

(test)