;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname tetris_project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
(require 2htdp/image)
(require math/base)
(require racket/list)
 (require 2htdp/universe)
 (require racket/base)


;;;
;;;CONSTANTS
;;;
(define BACKGROUND (rectangle 240 500  "outline" "white"))
(define SHAPE-CONSTRUCTOR (rectangle 20 20 "solid" "green"))
(define SHAPE-CONSTRUCTOR2 (rectangle 20 20 "solid" "gray"))
(define ORIGINAL-MATRIX (make-list 25 (make-list 12 0))) ;12 x 25 grid filled with zeros

(define WIDTH-BOUNDARY 12) ; 12 squares so 20*12 pixels = 240
(define HEIGHT-BOUNDARY 25) ; 30 squares so 20*25 pixels = 500

(define WORLD-WIDTH 300)
(define WORLD-HEIGHT 550)
(define PIXEL 20)

;draw : tetris-world -> image
;to draw the scene given world
;(length (list-ref (tw-matrix tw) 0)) = width boundary
; (length (tw-matrix tw)) = height boundary
; (place-image (text (number->string (tw-score tw)) 20 "black") 200 200
(define (draw tw)
  (if (game-over? tw)
      (place-image (text  "GAME OVER" 20 "red") 150 50
                                         (place-image (text (number->string (tw-score tw)) 20 "black") 150 25
                                                      (draw-matrix (tw-matrix tw) (- (length (list-ref (tw-matrix tw) 0)) 1) (- (length (tw-matrix tw)) 1) ;11, 24
                                                                   (draw-shape (tw-current-shape tw)
                                                                               BACKGROUND))))
      ;;if game over, draw matrix and display message 
      (place-image (text (number->string (tw-score tw)) 20 "black") 150 25
                   (draw-matrix (tw-matrix tw) (- (length (list-ref (tw-matrix tw) 0)) 1) (- (length (tw-matrix tw)) 1) ;11, 24
                                (draw-shape (tw-current-shape tw)
                                            BACKGROUND)))))


               
               
;
;TESTING CONSTANTS
;
(define NON-BLANK-BOARD
  (append
   (make-list 22 (make-list 12 0))
   `((0 0 0 0 1 1 0 1 0 1 1 1)
     (1 0 0 1 1 0 1 1 1 1 1 1)
     ,(make-list 12 1))))

(define EMPTY-ROW (make-list 12 0))
(define FULL-ROW (make-list 12 1))

                                                        
                                         


;A Tetris-World is
; (make-tw shape List-of-shapes Matrix list-of-natural(heights) natural(score) timer)
;interp.: if 'a-tw' is a tetris-world, then all of:
;- (tw-current-shape a-tw) is the shape that is currently falling from the top of the screen

;- matrix is the representaion of the board, of which grid is filled and which is not
; a matrix is basically a list of numbers to show which part of the grid is filled


; - score is bascially 10 * #of lines cleared  + 5 * # of multiple lines cleared at once --> 10 if 1 line cleared, 15 per line if more than 1 cleared
; - timer counts down from 300 seconds to 0 --> IMPOSSIBLE!

;; a matrix is a list of list

(define-struct tw (current-shape matrix score timer))

;A shape is
;invariant: always made up of 4 squares (side = 50)
; (make-shape posn posn posn posn)



;Actually, shape takes 4 posns, with the first one being the primary posn
;rotations will happen around posn1
(define-struct shape (posn1 posn2 posn3 posn4 type))


;type = one of:
; 1 : square
; 2 : T
; 2.25 = T rotated right 90 degrees
; 2.50 = T rotated right 180 degrees
; 2.75 = T rotated right 270 degrees
; 3: s-shaped
; 3.5: s-shaped rotated 90 deg
; 4: flipped s
; 4.5: rotated 90 degrees
; 5: L
; 5.25: L -> 90 right
; 5.50: L-> 180 right
; 5.75: L->270 right
; 6: flipped L
; 6.25: 6->90 right
; 6.50: 6 -> 180 right
; 6.75: 6-> 270 right
; 7 : stick
; 7.5: stick sideways
; make-shape: type -> shape
;posn1 = top left, posn2= bottom right
                                   
(define (make-shape-type type)
  (cond
    [(= type 1) (make-shape (make-posn 0 0) (make-posn 1 1) (make-posn 1 0) (make-posn 0 1) 1)]
    [(= type 2)    (make-shape (make-posn 0 0) (make-posn 1 0) (make-posn 2 0) (make-posn 1 1) 2)]
    [(= type 3) (make-shape (make-posn 1 0) (make-posn 2 0) (make-posn 0 1) (make-posn 1 1) 3)]
    [(= type 4) (make-shape (make-posn 0 0) (make-posn 1 0) (make-posn 1 1) (make-posn 2 1) 4)]
    [(= type 5) (make-shape (make-posn 0 0) (make-posn 0 1) (make-posn 0 2) (make-posn 1 2) 5)]
    [(= type 6) (make-shape (make-posn 1 0) (make-posn 1 1) (make-posn 1 2) (make-posn 0 2) 6)]
    [(= type 7) (make-shape (make-posn 0 0) (make-posn 0 1) (make-posn 0 2) (make-posn 0 3) 7)]; to make rotations work... i m going to have to pull some hair
    ))

;Example Worlds
(define example-tw-0 (make-tw (make-shape (make-posn 0 0) (make-posn 0 1) (make-posn 1 0) (make-posn 1 1) 1)
                            (list (list 0 0 0 0 0 0 0 0 0 0 0 0) ;1
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0));25
                            0
                            300))

(define example-tw-1 (make-tw (make-shape (make-posn 0 1) (make-posn 0 2) (make-posn 1 1) (make-posn 1 2) 1)
                            (list (list 0 0 0 0 0 0 0 0 0 0 0 0) ;1
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0));25
                            0
                            299))

(define example-tw-2 (make-tw (make-shape (make-posn 4 22) (make-posn 5 23) (make-posn 5 22) (make-posn 4 23) 1)
                            ;           0 1 2 3 4 5 6 7 8 9 10 11
                            (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                  (list 1 1 1 1 0 0 1 1 1 1 1  1);24
                                  (list 1 1 1 1 0 0 1 1 1 1 1  1));25
                            0
                            300))

(define example-tw-3 (make-tw (make-shape (make-posn 4 23) (make-posn 5 24) (make-posn 5 23) (make-posn 4 24) 1)
                            ;           0 1 2 3 4 5 6 7 8 9 10 11
                            (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                  (list 1 1 1 1 0 0 1 1 1 1 1  1);24
                                  (list 1 1 1 1 0 0 1 1 1 1 1  1));25
                            0
                            299))
(define example-tw-4 (make-tw (make-shape-type 1)
                            ;           0 1 2 3 4 5 6 7 8 9 10 11
                            (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                  (list 1 1 1 1 1 1 1 1 1 1 1  1);24
                                  (list 1 1 1 1 1 1 1 1 1 1 1  1));25
                            0
                            298))

(define example-tw-5 (make-tw (make-shape (make-posn 0 0) (make-posn 0 1) (make-posn 1 0) (make-posn 1 1) 1)
                            ;           0 1 2 3 4 5 6 7 8 9 10 11
                            (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0 );24
                                  (list  0 0 0 0 0 0 0 0 0 0 0  0));25
                            30
                            297))

(define example-tw-6 (make-tw (make-shape (make-posn 0 0) (make-posn 1 0) (make-posn 2 0) (make-posn 1 1) 2)
                            (list (list 0 0 0 0 0 0 0 0 0 0 0 0) ;1
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0 0));25
                            0
                            299))



;; TICK
(define (tick World)
  (make-tw (update-current-shape World)
           (update-matrix World)
           (change-score World)
           (update-time World)))

(check-expect (tick example-tw-0) example-tw-1)
(check-expect (tick example-tw-2) example-tw-3)
(check-expect (tick example-tw-3) example-tw-4)
(check-expect (tick example-tw-4) example-tw-5)
                      
                            
                                  
;;;;;;;;;;;;;;;;;;;
;; CURRENT-SHAPE ;;
;;;;;;;;;;;;;;;;;;;
;update-current-shape: world -> shape
;first checks if grid is filled and new shape is needed. Then, moves shape down by 1 if it can go down.
(define (update-current-shape tw)
  (shift-shape-down-1 (get-shape tw) (tw-matrix tw)))





;get-shapes: world -> shape
;returns a shape that will show up 
(define (get-shape tw)
  (get-shape-helper (tw-current-shape tw) (tw-matrix tw) (random-integer 1 8)))



;get-shape-helper: current-shape matrix -> current shape
(define (get-shape-helper shape matrix type)
 ( if
   (fill-grid? shape matrix)
   (make-shape-type (random-integer 1 8))
   shape))
       



;;;;;;;;;;;;
;; MATRIX ;;
;;;;;;;;;;;;
  

;checks if value is in list
(define (in list value)
  (cond
    [(empty? list) #f]
    [else
     (cond
       [(= (first list) value) #t]
       [else (in (rest list) value)])]))

;update-matrix: World -> matrix
;updates the matrix per tick --> Need to delete rows (delete-row) and shift rows down (shift-row) --> AFTER filling up matrix according to shapes (fill-grid)
(define (update-matrix World)
  (if (empty? (delete-which-row? (tw-matrix World) (- (length (tw-matrix World)) 1)))
      (fill-grid (tw-current-shape World) (tw-matrix World))
      (shift-row (delete-row (fill-grid (tw-current-shape World) (tw-matrix World)) (delete-which-row? (tw-matrix World) (- (length (tw-matrix World)) 1) )) 0)))

;fill-grid?: current-shape matrix -> boolean
;checks if shape has hit height at any of given x-posns.
(define (fill-grid? shape matrix)
    ;; check if matrix[posn-x][posn-y+1]==1: true
    ;; else: false
  (if (or (= (posn-y (shape-posn1 shape)) (sub1 (length matrix)))
          (= (posn-y (shape-posn2 shape)) (sub1 (length matrix)))
          (= (posn-y (shape-posn3 shape)) (sub1 (length matrix)))
          (= (posn-y (shape-posn4 shape)) (sub1 (length matrix))))

      #true

      (or (= (list-ref (list-ref matrix (+( posn-y (shape-posn1 shape)) 1)) (posn-x (shape-posn1 shape))) 1)
          (= (list-ref (list-ref matrix (+( posn-y (shape-posn2 shape)) 1)) (posn-x (shape-posn2 shape))) 1)
          (= (list-ref (list-ref matrix (+( posn-y (shape-posn3 shape)) 1)) (posn-x (shape-posn3 shape))) 1)
          (= (list-ref (list-ref matrix (+( posn-y (shape-posn4 shape)) 1)) (posn-x (shape-posn4 shape))) 1))))

(check-expect (fill-grid? (tw-current-shape example-tw-3) (tw-matrix example-tw-3)) true)

;fill-grid-helper: [list-of posn] matrix -> matrix
;places each posn on matrix, until list is empty
(define (fill-grid-helper posns matrix)
  (cond
    [(empty? posns) matrix]
    [else
     (fill-grid-helper (rest posns)
                       (fill-grid-helper-3 (first posns) matrix))]))


;;fill-grid-helper-3: posn matrix -> matrix
;places a posn (square pixel) onto the matrix
(define (fill-grid-helper-3 posn matrix)
  (cond
    [(= (posn-y posn) 0)
     (append (list (list-set (list-ref matrix (posn-y posn)) (posn-x posn) 1))
             (list-tail matrix 1))]
    [else
     (append (take matrix (posn-y posn))
             (list (list-set (list-ref matrix (posn-y posn)) (posn-x posn) 1))
             (list-tail matrix (+ (posn-y posn) 1)))]))


;fill-grid: current-shape matrix -> matrix
;checks if first element of shape is going to hit one of the grids, fill up matrix once it hits groud level
(define (fill-grid shape matrix)
  (cond
    [(fill-grid? shape matrix) (fill-grid-helper (list (shape-posn1 shape) (shape-posn2 shape) (shape-posn3 shape) (shape-posn4 shape)) matrix)]
    [else matrix]))


(check-expect (fill-grid (tw-current-shape example-tw-3) (tw-matrix example-tw-3))                             ;           0 1 2 3 4 5 6 7 8 9 10 11
                            (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                  (list 1 1 1 1 1 1 1 1 1 1 1  1);24
                                  (list 1 1 1 1 1 1 1 1 1 1 1  1)))
;delete-which-row?: matrix counter (integer = height boundary -1)->list of integers (indexes that must be deleted)
;checks matrix if rows are elligible to be deleted (filled up), returns the index of rows that can be deleted...
;counter must start at HEIGHT-BOUNDARY-1 when initiated 
(define (delete-which-row? matrix counter)
  (cond
    [(= counter 0) '()]
    [else
     (if (not (in (list-ref  matrix counter) 0))
         (cons counter (delete-which-row? matrix (- counter 1)))
         (delete-which-row? matrix (- counter 1)))]))
(check-expect (delete-which-row? (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                  (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                  (list 1 1 1 1 1 1 1 1 1 1 1  1);24
                                  (list 1 1 1 1 1 1 1 1 1 1 1  1)) 24) (list 24 23)) ;25
     
;delete-row: matrix [list-of indexes to delete]=(call to delete-which-row?) -> matrix
;deletes the row that is filled up
(define (delete-row matrix indexes)
  (cond
    [(empty? indexes) matrix]
    [else
      (delete-row (delete-row-helper matrix (first indexes)) (rest indexes))]))

;delete-row-helper: [listof [listof booleans]] index -> [listof [listof booleans]]
;deletes the element(list) at index in matrix and replaces it with [listof 0's] of same length 
(define (delete-row-helper matrix index)
  (append (list (make-list (length (list-ref matrix 0)) 0))
          (take matrix index)
          (take-right matrix (- (- (length matrix) 1) index))))



  
(check-expect (delete-row (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                                (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                                (list 1 1 1 1 1 1 1 1 1 1 1  1);24
                                (list 1 1 1 1 1 1 1 1 1 1 1  1))
                          (list 24 23))
              (list (list 0 0 0 0 0 0 0 0 0 0 0  0);1
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);2
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);3
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);4
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);5
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);6
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);7
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);8
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);9
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);10
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);11
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);12
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);13
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);14
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);15
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);16
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);17
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);18
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);19
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);20
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);21
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);22
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);23
                    (list 0 0 0 0 0 0 0 0 0 0 0  0);24
                    (list 1 1 1 1 1 1 1 1 1 1 1  1)))
       
;shift-row: matrix counter -> matrix
;iterates through each list, shifts row when necessary
; start from counter= 0, (- (- (length list_above) 1) x-counter)
(define (shift-row matrix y-counter)
  (cond
    [(= y-counter (- (length matrix) 1)) matrix]
    [else
     (shift-row (append (take matrix y-counter)
                        (shift-row-helper2 (list-ref matrix y-counter) (list-ref matrix (+ 1 y-counter)) 0)
                        ;(list (make-list 12 1) (make-list 12 1))
                        (take-right matrix (- (- (length matrix) 2) y-counter)))
                (+ 1 y-counter))]
    ))

(check-expect (shift-row (list (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)
                               (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)) 0)
                         (list (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)))

(check-expect (shift-row (list (list 1 0 0 0  0 0 1 0  0 0 0 0)
                               (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)) 0)
                         (list (list 0 0 0 0  0 0 0 0  0 0 0 0)
                               (list 1 0 0 0  0 0 1 0  0 0 0 0)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)
                               (list 1 1 1 1  1 1 1 1  1 1 1 1)))
  


;shift-row-helper2: [listof booleans] [listof booleans] counter -> [listof [listof bool]]
;if (list[counter]=1 && list_below[counter]=0):
;shift the element in upper list to list below and recur on the two lists again until counter = length
;else just return the matrix of two lists
;start with counter = 0
(define (shift-row-helper2 list_above list_below x-counter)
  (cond
    [(>= x-counter (length list_above))  (list list_above list_below)]
    [else
     (if (and (= (list-ref list_above x-counter) 1) (= (list-ref list_below x-counter) 0))
         (shift-row-helper2
          (append (take list_above x-counter) (list 0) (take-right list_above (- (- (length list_above) 1) x-counter))) ;gets the list until x-counter (noninclusive), append 0, append the list after x-counter (noninclusive)
          (append (take list_below x-counter) (list 1) (take-right list_below (- (- (length list_above) 1) x-counter)))
          (+ 1 x-counter))
         (shift-row-helper2 list_above list_below (+ 1 x-counter)))]
    ))
(check-expect (shift-row-helper2 (list 1 1 1 1) (list 0 0 0 0) 0) (list (list 0 0 0 0) (list 1 1 1 1)))

(check-expect (shift-row-helper2 (list 1 1 1 1) (list 1 0 0 0) 0) (list (list 1 0 0 0) (list 1 1 1 1)))








;;;;;;;;;;;;
;; SCORE  ;;
;;;;;;;;;;;;
;change-score: World -> integer
;changes the current score according to the matrix
(define (change-score tw)
  (change-score-helper (tw-matrix tw) (tw-score tw)))


;change-score-helper: matrix integer -> integer
;changes score according to matrix
(define (change-score-helper matrix current-score)
  (if (= (length (delete-which-row? matrix (- (length matrix) 1))) 0)
      current-score
      (if (> (length (delete-which-row? matrix (- (length matrix) 1))) 1)
          (+ current-score (* 15 (length (delete-which-row? matrix (- (length matrix) 1)))))
          (+ current-score(* 10 (length (delete-which-row? matrix (- (length matrix) 1))))))))
      


;;;;;;;;;;;;
;; TIMER ;;;
;;;;;;;;;;;;
(define (update-time tw)
  (- (tw-timer tw) 1))
  

;;;;;;;;;;;;;;;
;; KEY-EVENT ;;
;;;;;;;;;;;;;;;

;key: Tetris-World Key-event -> Tetris world
; to rotate shape right 90 degrees, at upper arrow
; to shift shape left or right (x axis), at left and right arrow
; to shift shape down (y axis), at down arrow
; to send the shape all the way down, at space-bar
(define (key tw ke)
  (make-tw (key-helper tw ke)
           (update-matrix tw)
           (tw-score tw)
           (tw-timer tw)))

;posn-equal?: posn posn -> boolean
(define (posn-equal? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;shape-equal?: shape shape -> boolean
;come on u know
(define (shape-equal? s1 s2)
  (and (posn-equal? (shape-posn1 s1) (shape-posn1 s2))
       (posn-equal? (shape-posn2 s1) (shape-posn2 s2))
       (posn-equal? (shape-posn3 s1) (shape-posn3 s2))
       (posn-equal? (shape-posn4 s1) (shape-posn4 s2))))

;key-helper: tetris-world key-event -> shape
(define (key-helper tw ke)
  (cond
    [ (key=? ke "left") (shift-shape-left (tw-current-shape tw))]
    [ (key=? ke "right") (shift-shape-right (tw-current-shape tw))]
    [ (key=? ke "down") (shift-shape-down-1 (tw-current-shape tw) (tw-matrix tw))]
    [ (key=? ke "up") (rotate-shape (tw-current-shape tw))]
    [ (key=? ke " ") (shift-shape-down-1 (tw-current-shape tw)(tw-matrix tw))]
    [else (tw-current-shape tw)]))
  
(check-expect (shape-equal? (key-helper example-tw-0 "left") (make-shape (make-posn 0 0) (make-posn 0 1) (make-posn 1 0) (make-posn 1 1) 1)) #true)
(check-expect (shape-equal? (key-helper example-tw-0 "right") (make-shape (make-posn 1 0) (make-posn 1 1) (make-posn 2 0) (make-posn 2 1) 1)) #true)
(check-expect (shape-equal? (key-helper example-tw-0 "down") (make-shape (make-posn 0 1) (make-posn 0 2) (make-posn 1 1) (make-posn 1 2) 1)) #true)
(check-expect (shape-equal? (key-helper example-tw-6 "up") (make-shape (make-posn 1 0) (make-posn 1 1) (make-posn 1 2) (make-posn 0 1) 2.25)) #true)
              
;(make-shape (make-posn 0 0) (make-posn 0 1) (make-posn 1 0) (make-posn 1 1) 1)

;shift-shape-left: shape -> shape
;shifts shape left by 1
(define (shift-shape-left shape)
  (if (or (= (posn-x (shape-posn1 shape)) 0) (= (posn-x (shape-posn2 shape)) 0) (= (posn-x (shape-posn3 shape)) 0) (= (posn-x (shape-posn4 shape)) 0))
      shape 
      (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 1) (posn-y (shape-posn1 shape)))
                  (make-posn (- (posn-x (shape-posn2 shape)) 1) (posn-y (shape-posn2 shape)))
                  (make-posn (- (posn-x (shape-posn3 shape)) 1) (posn-y (shape-posn3 shape)))
                  (make-posn (- (posn-x (shape-posn4 shape)) 1) (posn-y (shape-posn4 shape)))
                  (shape-type shape))))

;shift-shape-right: shape -> shape
;shifts shape right by 1
(define (shift-shape-right shape)
  (if (or (= (posn-x (shape-posn1 shape)) (- WIDTH-BOUNDARY 1))
          (= (posn-x (shape-posn2 shape)) (- WIDTH-BOUNDARY 1))
          (= (posn-x (shape-posn3 shape)) (- WIDTH-BOUNDARY 1))
          (= (posn-x (shape-posn4 shape))(- WIDTH-BOUNDARY 1)))
      shape 
      (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 1) (posn-y (shape-posn1 shape)))
                  (make-posn (+ (posn-x (shape-posn2 shape)) 1) (posn-y (shape-posn2 shape)))
                  (make-posn (+ (posn-x (shape-posn3 shape)) 1) (posn-y (shape-posn3 shape)))
                  (make-posn (+ (posn-x (shape-posn4 shape)) 1) (posn-y (shape-posn4 shape)))
                  (shape-type shape) )))



;rotate-shape: shape -> shape
;rotate shape right by 90 degrees
(define (rotate-shape shape)
  (cond
    [(and (not (=(shape-type shape) 7))
          (or (= (posn-x (shape-posn1 shape)) 0)
              (= (posn-x (shape-posn2 shape)) 0)
              (= (posn-x (shape-posn3 shape)) 0)
              (= (posn-x (shape-posn4 shape)) 0)))
     (rotate-shape (shift-shape-right shape))]
    [(and (not (=(shape-type shape) 7))
          (or (= (posn-x (shape-posn1 shape)) (- WIDTH-BOUNDARY 1))
              (= (posn-x (shape-posn2 shape)) (- WIDTH-BOUNDARY 1))
              (= (posn-x (shape-posn3 shape)) (- WIDTH-BOUNDARY 1))
              (= (posn-x (shape-posn4 shape)) (- WIDTH-BOUNDARY 1))))
     (rotate-shape (shift-shape-left shape))]
    [(and (=(shape-type shape) 7)
          (or (= (posn-x (shape-posn1 shape)) (- WIDTH-BOUNDARY 1))
              (= (posn-x (shape-posn2 shape)) (- WIDTH-BOUNDARY 1))
              (= (posn-x (shape-posn3 shape)) (- WIDTH-BOUNDARY 1))
              (= (posn-x (shape-posn4 shape)) (- WIDTH-BOUNDARY 1))))
     (rotate-shape (shift-shape-left (shift-shape-left (shift-shape-left shape))))]
    [(and (=(shape-type shape) 7)
          (or (= (posn-x (shape-posn1 shape)) 0)
              (= (posn-x (shape-posn2 shape)) 0)
              (= (posn-x (shape-posn3 shape)) 0)
              (= (posn-x (shape-posn4 shape)) 0)))
     (rotate-shape (shift-shape-right (shift-shape-right (shift-shape-right shape))))]
    [(= (shape-type shape) 1) shape]
    [(= (shape-type shape) 2)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 1)
                            (posn-y (shape-posn1 shape)))
                 (make-posn (posn-x (shape-posn2 shape))
                            (+ (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 1)
                            (+ (posn-y (shape-posn3 shape)) 2))
                 (make-posn (- (posn-x (shape-posn4 shape)) 1)
                            (posn-y (shape-posn4 shape)))
                 2.25)]
    [(= (shape-type shape) 2.25)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 1)
                            (+ (posn-y (shape-posn1 shape)) 1))
                 (make-posn (posn-x (shape-posn2 shape))
                            (posn-y (shape-posn2 shape))) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 1)
                            (- (posn-y (shape-posn3 shape)) 1))
                 (make-posn (+ (posn-x (shape-posn4 shape)) 1)
                            (- (posn-y (shape-posn4 shape)) 1))
                 2.5)]
    [(= (shape-type shape) 2.5)
     (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 2)
                            (+ (posn-y (shape-posn1 shape)) 1))
                 (make-posn (- (posn-x (shape-posn2 shape)) 1)
                            (posn-y (shape-posn2 shape))) 
                 (make-posn (posn-x (shape-posn3 shape))
                            (- (posn-y (shape-posn3 shape)) 1))
                 (make-posn (posn-x (shape-posn4 shape)) 
                            (+ (posn-y (shape-posn4 shape)) 1))
                 2.75)]
    [(=(shape-type shape) 2.75)
     (make-shape (make-posn (posn-x (shape-posn1 shape))
                            (- (posn-y (shape-posn1 shape)) 2))
                 (make-posn (+ (posn-x (shape-posn2 shape)) 1)
                            (- (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (+ (posn-x (shape-posn3 shape)) 2)
                            (posn-y (shape-posn3 shape)))
                 (make-posn (posn-x (shape-posn4 shape)) 
                            (posn-y (shape-posn4 shape)))
                 2)]
    [(=(shape-type shape) 3)
     (make-shape (make-posn (posn-x (shape-posn1 shape))
                            (+ (posn-y (shape-posn1 shape)) 1))
                 (make-posn (- (posn-x (shape-posn2 shape)) 1)
                            (+ (posn-y (shape-posn2 shape)) 2)) 
                 (make-posn (posn-x (shape-posn3 shape)) 
                            (- (posn-y (shape-posn3 shape)) 1))
                 (make-posn (- (posn-x (shape-posn4 shape)) 1) 
                            (posn-y (shape-posn4 shape)))
                 3.5)]
    [(=(shape-type shape) 3.5)
     (make-shape (make-posn (posn-x (shape-posn1 shape))
                            (- (posn-y (shape-posn1 shape)) 1))
                 (make-posn (+ (posn-x (shape-posn2 shape)) 1)
                            (- (posn-y (shape-posn2 shape)) 2)) 
                 (make-posn (posn-x (shape-posn3 shape)) 
                            (+ (posn-y (shape-posn3 shape)) 1))
                 (make-posn (+ (posn-x (shape-posn4 shape)) 1) 
                            (posn-y (shape-posn4 shape)))
                 3)]
   
    [(=(shape-type shape) 4)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 1)
                            (posn-y (shape-posn1 shape)) )
                 (make-posn (posn-x (shape-posn2 shape))
                            (+ (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 1)
                            (posn-y (shape-posn3 shape)))
                 (make-posn (- (posn-x (shape-posn4 shape)) 2) 
                            (+ (posn-y (shape-posn4 shape)) 1))
                 4.5)]
    
    [(=(shape-type shape) 4.5)
     (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 1)
                            (posn-y (shape-posn1 shape)) )
                 (make-posn (posn-x (shape-posn2 shape))
                            (- (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (+ (posn-x (shape-posn3 shape)) 1)
                            (posn-y (shape-posn3 shape)))
                 (make-posn (+ (posn-x (shape-posn4 shape)) 2) 
                            (- (posn-y (shape-posn4 shape)) 1))
                 4)]
    [(=(shape-type shape) 5)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 2)
                            (posn-y (shape-posn1 shape)) )
                 (make-posn (+ (posn-x (shape-posn2 shape)) 1)
                            (- (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (posn-x (shape-posn3 shape))
                            (- (posn-y (shape-posn3 shape)) 2))
                 (make-posn (- (posn-x (shape-posn4 shape)) 1) 
                            (- (posn-y (shape-posn4 shape)) 1))
                 5.25)]
    [(=(shape-type shape) 5.25)
     (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 1)
                            (+ (posn-y (shape-posn1 shape)) 2))

                 (make-posn (posn-x (shape-posn2 shape))
                            (+ (posn-y (shape-posn2 shape)) 1)) 

                 (make-posn (+ (posn-x (shape-posn3 shape)) 1)
                            (posn-y (shape-posn3 shape)))

                 (make-posn (posn-x (shape-posn4 shape)) 
                            (- (posn-y (shape-posn4 shape)) 1))
                 5.5)]
    [(=(shape-type shape) 5.5)
     (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 1)
                            (- (posn-y (shape-posn1 shape)) 1))
                 (make-posn (posn-x (shape-posn2 shape))
                            (posn-y (shape-posn2 shape))) 
                 (make-posn (+ (posn-x (shape-posn3 shape)) 1)
                            (+ (posn-y (shape-posn3 shape)) 1))
                 (make-posn (+ (posn-x (shape-posn4 shape)) 2) 
                            (posn-y (shape-posn4 shape)))
                 5.75)]
    [(=(shape-type shape) 5.75)
     (make-shape (make-posn (posn-x (shape-posn1 shape)) 
                            (- (posn-y (shape-posn1 shape)) 1))
                 (make-posn (- (posn-x (shape-posn2 shape)) 1)
                            (posn-y (shape-posn2 shape))) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 2)
                            (+ (posn-y (shape-posn3 shape)) 1))
                 (make-posn (- (posn-x (shape-posn4 shape)) 1) 
                            (+(posn-y (shape-posn4 shape)) 2))
                 5)]
    [(=(shape-type shape) 6)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 1) 
                            (+ (posn-y (shape-posn1 shape)) 1))
                 (make-posn (posn-x (shape-posn2 shape))
                            (posn-y (shape-posn2 shape))) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 1)
                            (- (posn-y (shape-posn3 shape)) 1))
                 (make-posn (posn-x (shape-posn4 shape)) 
                            (- (posn-y (shape-posn4 shape)) 2))
                 6.25)]
    [(=(shape-type shape) 6.25)
     (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 2) 
                            (+ (posn-y (shape-posn1 shape)) 1))
                 (make-posn (- (posn-x (shape-posn2 shape)) 1)
                            (posn-y (shape-posn2 shape))) 
                 (make-posn (posn-x (shape-posn3 shape))
                            (- (posn-y (shape-posn3 shape)) 1))
                 (make-posn (+ (posn-x (shape-posn4 shape)) 1) 
                            (posn-y (shape-posn4 shape)))
                 6.5)]
    [(=(shape-type shape) 6.5)
     (make-shape (make-posn (posn-x (shape-posn1 shape)) 
                            (- (posn-y (shape-posn1 shape)) 2))
                 (make-posn (+ (posn-x (shape-posn2 shape)) 1)
                            (- (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (+ (posn-x (shape-posn3 shape)) 2)
                            (posn-y (shape-posn3 shape)))
                 (make-posn (+ (posn-x (shape-posn4 shape)) 1) 
                            (+ (posn-y (shape-posn4 shape)) 1))
                 6.75)]
    [(=(shape-type shape) 6.75)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 1)
                            (posn-y (shape-posn1 shape)))
                 (make-posn (posn-x (shape-posn2 shape))
                            (+ (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 1)
                            (+ (posn-y (shape-posn3 shape)) 2))
                 (make-posn (- (posn-x (shape-posn4 shape)) 2) 
                            (+(posn-y (shape-posn4 shape)) 1))
                 6)]
    [(=(shape-type shape) 7)
     (make-shape (make-posn (+ (posn-x (shape-posn1 shape)) 3)
                            (posn-y (shape-posn1 shape)))
                 (make-posn (+ (posn-x (shape-posn2 shape)) 2)
                            (- (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (+ (posn-x (shape-posn3 shape)) 1)
                            (- (posn-y (shape-posn3 shape)) 2))
                 (make-posn (posn-x (shape-posn4 shape)) 
                            (- (posn-y (shape-posn4 shape)) 3))
                 7.5)]

    [(=(shape-type shape) 7.5)
     (make-shape (make-posn (- (posn-x (shape-posn1 shape)) 3)
                            (posn-y (shape-posn1 shape)))
                 (make-posn (- (posn-x (shape-posn2 shape)) 2)
                            (+ (posn-y (shape-posn2 shape)) 1)) 
                 (make-posn (- (posn-x (shape-posn3 shape)) 1)
                            (+ (posn-y (shape-posn3 shape)) 2))
                 (make-posn (posn-x (shape-posn4 shape)) 
                            (+ (posn-y (shape-posn4 shape)) 3))
                 7)]))


(check-expect (shape-equal? (rotate-shape (make-shape-type 3)) (make-shape (make-posn 1 1) (make-posn 1 2) (make-posn 0 0) (make-posn 0 1) 3.5)) #true)
;shift-shape-down-1: shape->shape
;shifts shape down 1 at y axis
(define (shift-shape-down-1 shape matrix)
  (if (fill-grid? shape matrix)
      shape 
      (make-shape (make-posn (posn-x (shape-posn1 shape)) (+ (posn-y (shape-posn1 shape)) 1))
                  (make-posn (posn-x (shape-posn2 shape)) (+ (posn-y (shape-posn2 shape)) 1))
                  (make-posn (posn-x (shape-posn3 shape)) (+ (posn-y (shape-posn3 shape)) 1))
                  (make-posn (posn-x (shape-posn4 shape)) (+ (posn-y (shape-posn4 shape)) 1))
                  (shape-type shape)) ))

        
       

;game-over?: tetris-world -> boolean
;checks if any element in the top line of matrix is filled (1)

(define (game-over? tw)
  (or (in (list-ref (tw-matrix tw) 0) 1)
      (= (tw-timer tw) 0)))
  

;(place-image
 ;     (text (number->string (bw-score tw)) 20 "black")
  ;    25 25

;draw : tetris-world -> image
;to draw the scene given world
;(define (draw tw)
;  ;(if (game-over tw)
;      ;;if game over, draw matrix and display message 
;  (draw-matrix (tw-matrix tw) (- WIDTH-BOUNDARY 1) (- HEIGHT-BOUNDARY 1)
;               (draw-shape (tw-current-shape tw)
;                           (place-image (text (number->string (tw-score tw)) 20 "black") 25 25 BACKGROUND))))
;               
               
;width-boundary = (length (list-ref matrix 0))
              
;draw-matrix: matrix counter image -> image
;visually represents the filled in grid on the background
;basically, iterate thru matrix, check if there is 1. Find 1, use x*20, y*20 to place image.
;x=0,11 y=0,24
;start with x=width-boundary-1, y=height-boundary-1
(define (draw-matrix matrix counter-x counter-y background)
  (cond
    [(< counter-x 0) (draw-matrix matrix (- (length (list-ref matrix 0)) 1) (- counter-y 1) background)]
    [(< counter-y 0) background]
    [(= (list-ref (list-ref matrix counter-y) counter-x) 1)
     (draw-matrix matrix
                  (- counter-x 1)
                  counter-y
                  (place-image SHAPE-CONSTRUCTOR2 (* 20 counter-x) (* 20 counter-y) background))]
    [else (draw-matrix matrix (- counter-x 1) counter-y background)]))
      
      


;draw-shape: shape image -> image
;draws the shape onto the background
(define (draw-shape shape background)
  (draw-posns (list (shape-posn1 shape) (shape-posn2 shape) (shape-posn3 shape) (shape-posn4 shape)) background))

;draw-posns: [listof posn] image -> image
(define (draw-posns posns background)
  (cond [(empty? posns) background]
        [else
         (draw-posns (rest posns) (draw-posn (first posns) background))]))

;draw-posn:  posn image -> image
(define (draw-posn posn background)
  (place-image SHAPE-CONSTRUCTOR (* PIXEL (posn-x posn)) (* PIXEL (posn-y posn)) background))




(define (start _dummy)
  (big-bang (make-tw
             (make-shape (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1) 1)
             ORIGINAL-MATRIX
             0
             1500)
    [on-tick tick 1/5]
    [on-key key]
    [to-draw draw]))


(start 0)
