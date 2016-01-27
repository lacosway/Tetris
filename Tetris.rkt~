;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Tetris
;; Elizabeth Cosway and Charlotte McHugh
;; Fundamentals of Computer Science 1, Fall 2015
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
(define SQUARE-SIZE 10)
(define GRIDWIDTH 10)
(define WIDTH 10)
(define HEIGHT 20)
(define BG (empty-scene (* GRIDWIDTH WIDTH) (* GRIDWIDTH HEIGHT)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;;starting point for tetra blocks
(define green1 (make-block 35 5 'green))
(define green2 (make-block 45 5 'green))
(define green3 (make-block 35 -5 'green))
(define green4 (make-block 45 -5 'green))

(define blue1 (make-block 25 5 'blue))
(define blue2 (make-block 35 5 'blue))
(define blue3 (make-block 45 5 'blue))
(define blue4 (make-block 55 5 'blue))

(define purple1 (make-block 25 5 'purple))
(define purple2 (make-block 35 5 'purple))
(define purple3 (make-block 45 5 'purple))
(define purple4 (make-block 45 -5 'purple))

(define aqua1 (make-block 25 -5 'aqua))
(define aqua2 (make-block 25 5 'aqua))
(define aqua3 (make-block 35 5 'aqua))
(define aqua4 (make-block 45 5 'aqua))

(define orange1 (make-block 25 5 'orange))
(define orange2 (make-block 35 5 'orange))
(define orange3 (make-block 35 -5 'orange))
(define orange4 (make-block 45 5 'orange))

(define pink1 (make-block 25 -5 'pink))
(define pink2 (make-block 35 -5 'pink))
(define pink3 (make-block 35 5 'pink))
(define pink4 (make-block 45 5 'pink))

(define red1 (make-block 25 5 'red))
(define red2 (make-block 35 5 'red))
(define red3 (make-block 35 -5 'red))
(define red4 (make-block 45 -5 'red))


;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;;falling tetras
(define OSET (list green1 green2 green3 green4))
(define ISET (list blue1 blue2 blue3 blue4))
(define LSET (list purple1 purple2 purple3 purple4))
(define JSET (list aqua1 aqua2 aqua3 aqua4))
(define TSET (list orange1 orange2 orange3 orange4))
(define ZSET (list pink1 pink2 pink3 pink4))
(define SSET (list red1 red2 red3 red4))

;;test pile
(define test-bset (list (make-block 5 195 'green)))
#; (define (bset-temp bset)
     (cond [(empty? bset)...]
           [(cons? bset)...(posn-temp (first bset))
                        ...(bset-temp (rest bset))...]))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))
(define O (make-tetra (make-posn 40 0) OSET))
(define I (make-tetra (make-posn 50 0) ISET))
(define L (make-tetra (make-posn 40 0) LSET))
(define J (make-tetra (make-posn 30 0) JSET))
(define T (make-tetra (make-posn 30 0) TSET))
(define Z (make-tetra (make-posn 30 0) ZSET))
(define S (make-tetra (make-posn 40 0) SSET))
#; (define (tetra-temp t)
     (...(tetra-center t)
         (tetra-blocks t)...))
 
;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))
(define w1 (make-world O test-bset))
(define start (make-world I '()))
#; (define (world-temp w)
     (... (tetra-temp (world-tetra w))
          (bset-temp (world-pile w))...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;move-world: World Key-event -> World
;;shifts or rotates tetra depending on given key event
(check-expect (move-world w1 "left")
              (make-world (make-tetra (make-posn 30 0)
                                      (list (make-block 25 5 'green)
                                            (make-block 35 5 'green)
                                            (make-block 25 -5 'green)
                                            (make-block 35 -5 'green)))
                          test-bset))
(check-expect (move-world w1 "right")
              (make-world (make-tetra (make-posn 50 0)
                                      (list (make-block 45 5 'green)
                                            (make-block 55 5 'green)
                                            (make-block 45 -5 'green)
                                            (make-block 55 -5 'green)))
                          test-bset))
(check-expect (move-world w1 "a")
              (make-world (make-tetra (make-posn 40 0)
                                      (list (make-block 35 -5 'green)
                                            (make-block 35 5 'green)
                                            (make-block 45 -5 'green)
                                            (make-block 45 5 'green)))
                          test-bset))
(check-expect (move-world w1 "s")
              (make-world (make-tetra (make-posn 40 0)
                                      (list (make-block 45 5 'green)
                                            (make-block 45 -5 'green)
                                            (make-block 35 5 'green)
                                            (make-block 35 -5 'green)))
                          test-bset))
(check-expect (move-world w1 "t")
              w1)
(define (move-world w k)
  (cond[(or (key=? k "left")(key=? k "right"))
        (make-world (shift-tetra (world-tetra w) k)
                    (world-pile w))]
       [(key=? k "a")
        (make-world
         (make-tetra (tetra-center (world-tetra w))
                     (tetra-rotate-ccw (tetra-center (world-tetra w))
                                       (tetra-blocks (world-tetra w))))
         (world-pile w))]
       [(key=? k "s")
        (make-world
         (make-tetra (tetra-center (world-tetra w))
                     (tetra-rotate-cw (tetra-center (world-tetra w))
                                      (tetra-blocks (world-tetra w))))
         (world-pile w))]
       [else w]))

;; tetra-rotate-ccw: Posn BSet -> BSet
;; rotates a list of blocks 90 degrees counterclockwise
(check-expect (tetra-rotate-ccw (make-posn 40 0) OSET)
              (list (make-block 35 -5 'green)
                    (make-block 35 5 'green)
                    (make-block 45 -5 'green)
                    (make-block 45 5 'green)))
(define (tetra-rotate-ccw p a-lob)
  (local [(define (block-rotate-ccw b)
            (make-block (+ (posn-x p) (- (posn-y p) (block-y b)))
                        (+ (posn-y p) (- (block-x b) (posn-x p)))
                        (block-color b)))]
    (map block-rotate-ccw a-lob)))

;; tetra-rotate-cw: Posn BSet -> BSet
;; rotates a list of blocks 90 degrees clockwise
(check-expect (tetra-rotate-cw (make-posn 40 0) OSET)
              (list (make-block 45 5 'green)
                    (make-block 45 -5 'green)
                    (make-block 35 5 'green)
                    (make-block 35 -5 'green)))
(define (tetra-rotate-cw p a-lob)
  (tetra-rotate-ccw p (tetra-rotate-ccw p (tetra-rotate-ccw p a-lob))))

;; shift-tetra: tetra string -> tetra
;; shifts tetra given key even
(check-expect (shift-tetra O "left")
              (make-tetra (make-posn 30 0) (list (make-block 25 5 'green)
                                                 (make-block 35 5 'green)
                                                 (make-block 25 -5 'green)
                                                 (make-block 35 -5 'green))))
(check-expect (shift-tetra O "right")
              (make-tetra (make-posn 50 0) (list (make-block 45 5 'green)
                                                 (make-block 55 5 'green)
                                                 (make-block 45 -5 'green)
                                                 (make-block 55 -5 'green))))
(define (shift-tetra t s)
  (cond [(on-screen? (tetra-blocks t) s)
         (make-tetra (make-posn
                      (cond [(string=? "left" s)
                             (- (posn-x (tetra-center t)) 10)]
                            [else (+ (posn-x (tetra-center t)) 10)])
                      (posn-y (tetra-center t)))            
                     (shift-bset (tetra-blocks t) s))]
        [else t]))

;; shift-bset: BSet String -> BSet
;; moves the bsets
(check-expect (shift-bset (list (make-block 35 5 'green)
                                (make-block 45 5 'green)) "up")
              (list (make-block 35 5 'green) (make-block 45 5 'green)))
(define (shift-bset a-bset s)
  (cond [(empty? a-bset) empty]
        [(string=? "left" s)
         (cons (make-block (- (block-x (first a-bset)) 10)
                           (block-y (first a-bset))
                           (block-color (first a-bset)))
               (shift-bset (rest a-bset) s))]
        [(string=? "right" s)
         (cons (make-block (+ (block-x (first a-bset)) 10)
                           (block-y (first a-bset))
                           (block-color (first a-bset)))
               (shift-bset (rest a-bset) s))]
        [else a-bset]))

;; on-screen?: BSet String -> Boolean
;; checks to see if tetra is on the screen
(check-expect (on-screen? '() "left") true)
(check-expect (on-screen? (list (make-block 45 45 'green)
                                (make-block 55 45 'green)) "left") true)
(check-expect (on-screen? (list (make-block 5 45 'green)
                                (make-block 15 45 'green)) "left") false)
(check-expect (on-screen? (list (make-block 85 55 'green)
                                (make-block 95 55 'green)) "right") false)
(define (on-screen? a-bset s)
  (local [(define (left b) (> (block-x b) 5))
          (define (right b) (< (block-x b) 95))]
    (if (string=? s "left")
        (andmap left a-bset)
        (andmap right a-bset))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-world: World -> Image
;; draws a tetra on the grid
(check-expect (draw-world w1)
              (bset->image OSET test-bset))
(define (draw-world w)
  (bset->image (tetra-blocks (world-tetra w)) (world-pile w)))

;; bset->image: BSet BSet -> Image
;; draws image of a given bset
(check-expect (bset->image OSET test-bset)
              (place-image
               (overlay (square SQUARE-SIZE 'outline 'black)
                        (square SQUARE-SIZE 'solid 'green)) 35 5
               (place-image
                (overlay (square SQUARE-SIZE 'outline 'black)
                         (square SQUARE-SIZE 'solid 'green)) 45 5
                (place-image
                 (overlay (square SQUARE-SIZE 'outline 'black)
                          (square SQUARE-SIZE 'solid 'green)) 45 -5
                 (place-image
                  (overlay (square SQUARE-SIZE 'outline 'black)
                           (square SQUARE-SIZE 'solid 'green)) 35 -5
                  (place-image
                   (overlay (square SQUARE-SIZE 'outline 'black)
                            (square SQUARE-SIZE 'solid 'green)) 5 195
                   BG))))))
(define (bset->image a-bset p)
  (foldr (λ(b y)
           (place-image (overlay (square SQUARE-SIZE 'outline 'black)
                                 (square SQUARE-SIZE 'solid (block-color b)))
                        (block-x b) (block-y b)
                        y)) (pile->image p) a-bset))

;; pile->image: BSet -> Image
;; turns the pile into an image
(check-expect (pile->image test-bset)
              (place-image
               (overlay (square SQUARE-SIZE 'outline 'black)
                        (square SQUARE-SIZE 'solid 'green)) 5 195
               BG))
(define (pile->image p)
 (foldr (λ(x y)
          (place-image (overlay (square SQUARE-SIZE 'outline 'black)
                                (square SQUARE-SIZE 'solid (block-color x)))
                       (block-x x) (block-y x)
                       y)) BG p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; drop: World -> World
;; moves the tetra until it hits the top of the pile
(check-expect (drop w1)
              (make-world (make-tetra (make-posn 40 10)
                                      (list (make-block 35 15 'green)
                                            (make-block 45 15 'green)
                                            (make-block 35 5 'green)
                                            (make-block 45 5 'green)))
                          test-bset))
(check-random (drop (make-world (make-tetra (make-posn 50 190) test-bset)
                                test-bset))
              (tetra-gen (make-world (make-tetra (make-posn 50 190) test-bset)
                                     test-bset) (random 7)))
(define (drop w)
  (cond [(tetra-hit? w) (tetra-gen w (random 7))]
        [else (make-world (block-fall (world-tetra w)) (world-pile w))]))
;; tetra-gen World Number -> World
;; generates world with new, randomly
#;(check-expect (tetra-gen w1 0) (make-world O (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
#;(check-expect (tetra-gen w1 1) (make-world I (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
#;(check-expect (tetra-gen w1 2) (make-world L (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
#;(check-expect (tetra-gen w1 3) (make-world J (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
#;(check-expect (tetra-gen w1 4) (make-world T (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
#;(check-expect (tetra-gen w1 5) (make-world Z (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
#;(check-expect (tetra-gen w1 6) (make-world S (list (make-block 35 5 'green)
                                                   (make-block 45 5 'green)
                                                   (make-block 35 -5 'green)
                                                   (make-block 45 -5 'green)
                                                   (make-block 5 195 'green))))
(define (tetra-gen w n)
  (make-world (cond [(= n 0) O]
                    [(= n 1) I]
                    [(= n 2) L]
                    [(= n 3) J]
                    [(= n 4) T]
                    [(= n 5) Z]
                    [(= n 6) S])
              (new-pile (add-pile (tetra-blocks (world-tetra w))
                                  (world-pile w)))))

;; tetra-hit?: World -> Boolean
;; does the tetra hit the bottom or the pile
(check-expect (tetra-hit? w1) false)
(check-expect (tetra-hit? (make-world (make-tetra (make-posn 50 190)
                                                  test-bset) test-bset)) true)
(define (tetra-hit? w)
  (cond [(= (posn-y (tetra-center (world-tetra w))) 190) true]
        [else (compare (tetra-blocks (world-tetra w)) (world-pile w))]))

;; compare: BSet BSet -> Boolean
;; compares two BSets to see if they have any matching posns
(check-expect (compare '() test-bset) false)
(check-expect (compare OSET (list (make-block 25 15 'red)
                                  (make-block 35 15 'red))) true)
(define (compare b p)
  (ormap (λ(x) (at-bottom? x p)) b))

;; at-bottom?: Block BSet -> Boolean
(check-expect (at-bottom? green1 '()) false)
(check-expect (at-bottom? green1 (list (make-block 25 15 'red)
                                       (make-block 35 15 'red))) true)
(define (at-bottom? block pile)
  (local [(define x-block (block-x block))
          (define y-block (block-y block))
          (define (comp-blocks b)
            (and (= x-block (block-x b))
                 (= (+ 10 y-block) (block-y b))))]
    (ormap comp-blocks pile)))
;; block-y=?: BSet Number -> BSet
(check-expect (block-y=? (list (make-block 15 45 'red)
                               (make-block 25 45 'red)
                               (make-block 25 55 'red)
                               (make-block 35 55 'red)) 55)
              (list (make-block 25 55 'red)
                    (make-block 35 55 'red)))
(define (block-y=? a-bset r)
  (filter (λ(b) (= (block-y b) r)) a-bset))

;; full-row?: BSet -> Boolean
;; is the row full?
(check-expect (full-row? (list (make-block 5 45 'green)
                               (make-block 15 45 'green)
                               (make-block 25 45 'green)
                               (make-block 35 45 'green)
                               (make-block 45 45 'green)
                               (make-block 55 45 'green)
                               (make-block 65 45 'green)
                               (make-block 75 45 'green)
                               (make-block 85 45 'green)
                               (make-block 95 45 'green)))
              true)
              
(define (full-row? a-bset)
  (= 500 (foldr (λ(x y) (+ (block-x x) y)) 0 a-bset)))

;; clear-row: BSet Number -> BSet
;; clears a full row
(check-expect (clear-row (list (make-block 15 45 'red)
                               (make-block 25 45 'red)
                               (make-block 25 55 'red)
                               (make-block 35 55 'red)) 55)
              (list (make-block 15 45 'red)
                    (make-block 25 45 'red)))
(define (clear-row a-bset r)
  (local [(define (not-in-row? b)
            (not (= (block-y b) r)))]
    (filter not-in-row? a-bset)))

;; above-row: BSet Number -> BSet
(check-expect (above-row (list (make-block 15 45 'red)
                               (make-block 25 45 'red)
                               (make-block 25 55 'red)
                               (make-block 35 55 'red)) 55)
              (list (make-block 15 45 'red)
                    (make-block 25 45 'red)))
(define (above-row a-bset r)
  (filter (λ(x) (< (block-y x) r)) a-bset))

;; below-row: BSet Number -> BSet
(check-expect (below-row (list (make-block 15 45 'red)
                               (make-block 25 45 'red)
                               (make-block 25 55 'red)
                               (make-block 35 55 'red)) 45)
              (list (make-block 25 55 'red)
                    (make-block 35 55 'red)))
(define (below-row a-bset r)
  (filter (λ(x) (> (block-y x) r)) a-bset))

;; shift-pile: BSet -> BSet
;; shifts the pile down
(check-expect (shift-pile (list (make-block 15 45 'red)
                                (make-block 25 45 'red)
                                (make-block 25 55 'red)
                                (make-block 35 55 'red)))
              (list (make-block 15 55 'red)
                    (make-block 25 55 'red)
                    (make-block 25 65 'red)
                    (make-block 35 65 'red)))
(define (shift-pile a-bset)
  (map (λ(b) (make-block (block-x b) (+ (block-y b) 10) (block-color b)))
       a-bset))

;; new-pile: BSet -> BSet
(check-expect (new-pile (list  (make-block 5  45 'green)
                               (make-block 15 45 'green)
                               (make-block 25 45 'green)
                               (make-block 35 45 'green)
                               (make-block 45 45 'green)
                               (make-block 55 45 'green)
                               (make-block 65 45 'green)
                               (make-block 75 45 'green)
                               (make-block 85 45 'green)
                               (make-block 95 45 'green)
                               (make-block 15 35 'blue)
                               (make-block 15 55 'red)))
              (list (make-block 15 55 'red)
                    (make-block 15 45 'blue)))
(check-expect (new-pile (list  (make-block 5  45 'green)
                               (make-block 15 45 'green)
                               (make-block 25 45 'green)
                               (make-block 35 45 'green)
                               (make-block 45 45 'green)
                               (make-block 55 45 'green)
                               (make-block 65 45 'green)
                               (make-block 75 45 'green)
                               (make-block 85 45 'green)
                               (make-block 95 45 'green)
                               (make-block 15 35 'blue)
                               (make-block 15 55 'red)
                               (make-block 5  65 'green)
                               (make-block 15 65 'green)
                               (make-block 25 65 'green)
                               (make-block 35 65 'green)
                               (make-block 45 65 'green)
                               (make-block 55 65 'green)
                               (make-block 65 65 'green)
                               (make-block 75 65 'green)
                               (make-block 85 65 'green)
                               (make-block 95 65 'green)))
              (list (make-block 15 65 'red)
                    (make-block 15 55 'blue)))
(define (new-pile p)
  (local [(define (delete current pile)
            (cond[(< current 5)
                   pile]
                 [(not (full-row? (block-y=? pile current)))
                  (delete (- current 10) pile)]
                 [(full-row? (block-y=? pile current))
                  (delete current
                          (append
                           (below-row pile current)
                           (shift-pile (above-row pile current))))]))]
    (delete 195 p)))

;; add-pile: BSet BSet -> BSet
;; adds the tetra to the pile
(check-expect (add-pile (list (make-block 35 195 'green)
                              (make-block 45 195 'green)
                              (make-block 35 185 'green)
                              (make-block 45 185 'green))
                        test-bset)
              (list (make-block 35 195 'green)
                    (make-block 45 195 'green)
                    (make-block 35 185 'green)
                    (make-block 45 185 'green)
                    (make-block 5 195 'green)))
(define (add-pile blocks a-bset)
  (foldr (λ (x y) (cons x y)) a-bset blocks))

;; block-fall: Tetra -> Tetra
;; moves a tetra down
(check-expect (block-fall O)
              (make-tetra (make-posn 40 10)
                          (list (make-block 35 15 'green)
                                (make-block 45 15 'green)
                                (make-block 35 5 'green)
                                (make-block 45 5 'green))))
(define (block-fall t)
  (make-tetra
   (make-posn (posn-x (tetra-center t))
              (+ 10 (posn-y (tetra-center t))))
   (move-blocks (tetra-blocks t))))

;; move-blocks: BSet -> BSet
(check-expect (move-blocks (list (make-block 15 35 'green)
                                 (make-block 15 45 'green)))
              (list (make-block 15 45 'green)
                    (make-block 15 55 'green)))
(define (move-blocks a-bset)
  (map (λ (block) (make-block (block-x block)
                              (+ (block-y block) 10)
                              (block-color block))) a-bset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stop-world: World -> Boolean
;; stops the game
(check-expect (stop-world (make-world O (list (make-block 40 5 'green)))) true)
(define (stop-world w)
  (at-top? (world-pile w)))

;; at-top?: BSet -> Boolean
(check-expect (at-top? '()) false)
(check-expect (at-top? (list (make-block 40 15 'green)
                             (make-block 40 5 'green))) true)
(define (at-top? pile)
  (ormap (λ (x) (= (block-y x) 5)) pile))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; main
(define (main w)
  (big-bang w
            (to-draw draw-world)
            (on-tick drop .25)
            (on-key move-world)
            (stop-when stop-world draw-score)))

;; draw-score: World -> Image
;; displays final score
(check-expect (draw-score w1)
              (place-image (text "Score: 1" 20 "black") 50 100 BG))
(define (draw-score w)
  (place-image (text
                (string-append "Score: "
                               (number->string (score (world-pile w))))
                20 "black") 50 100 BG))

;; score: BSet -> Number
;; calculates the score of the game
(check-expect (score OSET) 4)
(define (score bset)
  (foldr (λ(b y) (+ 1 y)) 0 bset))