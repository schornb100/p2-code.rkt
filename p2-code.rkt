(require 2htdp/image)
; List-of-strings -> Number
; adds the number of characters each word on the list
; returns the total number of characters
; "X", "Y", "Z" -> 3
(check-expect
  (score-by-length (cons "X" (cons "Y"  (cons "Z" '()))))
  3)
; "A", "Flatt" -> 7
(check-expect
  (score-by-length (cons "A" (cons "Flatt" (cons "C" '()))))
  7)
(define (score-by-length List-of-strings)
  (cond
    ((empty? List-of-strings) 0)
    (else (+ (string-length (first List-of-strings)) (score-by-length (rest List-of-strings))))))

; List-of-images -> Image
; overlays a group of images on top of each other
; red 2pxl star -> red 2pxl star
(check-expect
 (overlay-all (cons (star 2 "solid" "red") '()))
 (overlay (star 2 "solid" "red") (rectangle 10 10 "solid" "white")))
; red 2*2 rectangle, blue 5pxl circle, purple 8pxl star -> overlays rectangle, circle, and star on top of each other
(check-expect
 (overlay-all (cons (rectangle 2 2 "solid" "red") (cons (circle 5 "solid" "blue") (cons (star 8 "solid" "purple") '()))))
   (overlay (rectangle 2 2 "solid" "red") (circle 5 "solid" "blue") (star 8 "solid" "purple") (rectangle 10 10 "solid" "white")))
; blue 50pxl star, purple tpxl triangle, red 10pxl star -> overlays star, triangle, and star on top of each other
(check-expect
 (overlay-all (cons (star 50 "solid" "blue") (cons (triangle 5 "solid" "purple") (cons (star 10 "outline" "red") '()))))
 (overlay (star 50 "solid" "blue") (triangle 5 "solid" "purple") (star 10 "outline" "red") (rectangle 10 10 "solid" "white")))
(define (overlay-all List-of-images)
  (cond
    [(empty? List-of-images) (rectangle 10 10 "solid" "white")]
    [else (overlay (first List-of-images) (overlay-all (rest List-of-images)))]))
    
; List-of-numbers -> Images
; creates side by side black bar graphs with varying heights given from a list of numbers
; "14" -> black rectangle size 10*14
(check-expect
 (bar-graph (cons 14 '()))
 (beside/align "bottom" (rectangle 10 14 "solid" "black") (rectangle 1 1 "solid" "white")))
; "20, 10, 30" -> black rectangle size 10*20 next to black rectangle size 10*10 next to black rectangle 10*30
(check-expect
 (bar-graph (cons 20 (cons 10 (cons 30 '()))))
 (beside/align "bottom" (rectangle 10 20 "solid" "black") (rectangle 10 10 "solid" "black") (rectangle 10 30 "solid" "black") (rectangle 1 1 "solid" "white")))
; "11.5", "22" -> black rectangle size 10*11 next to black rectangle size 10*22
(check-expect
 (bar-graph (cons 11.5 (cons 22 '())))
 (beside/align "bottom" (rectangle 10 11.5 "solid" "black") (rectangle 10 22 "solid" "black") (rectangle 1 1 "solid" "white")))
(define (bar-graph List-of-numbers)
  (cond
    [(empty? List-of-numbers) (rectangle 1 1 "solid" "white")]
    [else (beside/align "bottom" (rectangle 10 (first List-of-numbers) "solid" "black") (bar-graph (rest List-of-numbers)))]))

; Any List-of-any -> Boolean
; runs through a list of values
; determines if Any is anywhere in the list
; 1 -- 1, 2, 3, 1 -> #true
(check-expect
 (is-in? 1 (cons 2 (cons 3 (cons 1 '()))))
 #true)
; "hi" -- "hola, hello, hello-world" -> #false
(check-expect
 (is-in? "hi" (cons "hola" (cons "hello" (cons "hello-world" '()))))
 #false)
; 10.1 -- #empty -> #false
(check-expect
 (is-in? 10.1 (cons null '()))
 #false)
(define (is-in? Any List-of-any)
  (cond
    [(empty? List-of-any) #false]
    [(equal? (first List-of-any) Any) #true]
    [else (is-in? Any (rest List-of-any))]))
    
; List-of-strings -> String
; consumes a list of words
; returns one string of words with a space between each pair of words
; "hello" "world" -> "hello world"
(check-expect
 (words-to-sentence (cons "hello" (cons "world" '())))
 "hello world")
; "hi" "how" "was" "your" "day" -> "hi how was your day"
(check-expect
 (words-to-sentence (cons "hi" (cons "how" (cons "was" (cons "your" (cons "day" '()))))))
 "hi how was your day")
; "www.www.com" -> "www.www.com"
(check-expect
 (words-to-sentence (cons "www.www.com" '()))
 "www.www.com")
(define (words-to-sentence List-of-strings)
  (cond
    [(equal? (length List-of-strings) 1) (first List-of-strings)]
    [else (string-append (first List-of-strings) " " (words-to-sentence (rest List-of-strings)))])) 
