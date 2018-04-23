;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor-project-sample-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; editor-project-sample-4.rkt
;;
;; In this project you will design a simple one line text editor.  
;;
;; The screen looks like:
;; 
;;     abc|def
;;
;; where | is the cursor.
;;
;; Typing a character inserts that character before the cursor.
;; The backspace key deletes the character before the cursor.
;; The left and right arrow keys move the cursor left and right.



;; =================================================================================
;; Constants:

(define WIDTH  200)
(define HEIGHT  20)

(define TEXT-SIZE  18)
(define TEXT-COLOR "BLACK")

(define CURSOR (rectangle 1 20 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))



;; =================================================================================
;; Data Definitions:

(define-struct editor (txt cp))
;; Editor is (make-editor String Natural)
;; interp. the current text (txt) and cursor position (cp) using a 0-based index

(define ED1 (make-editor ""       0)) ; empty
(define ED2 (make-editor "abcdef" 0)) ; cursor at beginning as in |abcdef
(define ED3 (make-editor "abcdef" 3)) ; cursor in middle of text as in abc|def
(define ED4 (make-editor "abcdef" 6)) ; cursor at end as in abcdef|



;; =================================================================================
;; Functions:

;; Editor -> Editor
;; start the world with an initial state e, for example (main (make-editor "" 0))
(define (main e)
  (big-bang e
            (to-draw    render)                  ; Editor -> Image
            (on-key     handle-key)))            ; Editor KeyEvent -> Editor



;; Editor -> Image
;; place text with cursor at left, middle edge of MTS
(check-expect (render (make-editor "abcdef" 3))
              (overlay/align "left"
                             "middle"
                             (beside (text "abc" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "def" TEXT-SIZE TEXT-COLOR))
                             MTS))

;(define (render e) MTS) ;stub

;; Took template from Editor

(define (render e)
  (overlay/align "left"
                 "middle"
                 (beside (text (txt-before-cp e) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (txt-after-cp e) TEXT-SIZE TEXT-COLOR))
                 MTS))



;; Editor KeyEvent -> Editor
;; call appropriate function for each keyboard command
(check-expect (handle-key (make-editor "abc" 2) "left")  (cursor-left (make-editor "abc" 2)))
(check-expect (handle-key (make-editor "abc" 2) "right") (cursor-right (make-editor "abc" 2)))
(check-expect (handle-key (make-editor "abc" 2) "\b")    (backspace (make-editor "abc" 2)))
(check-expect (handle-key (make-editor "abc" 2) "x")     (insert (make-editor "abc" 2) "x"))
(check-expect (handle-key (make-editor "abc" 2) "shift") (make-editor "abc" 2))

;(define (handle-key e key) e) ;stub

;; Took template from Editor

(define (handle-key e key)
  (cond [(key=? key "left")        (cursor-left e)]
        [(key=? key "right")       (cursor-right e)]
        [(key=? key "\b")          (backspace e)]        
        [(= (string-length key) 1) (insert e key)]   
        [else e]))



;; Editor -> Editor
;; moves the cursor left by 1
(check-expect (cursor-left  (make-editor "abc" 0)) (make-editor "abc" 0))
(check-expect (cursor-left  (make-editor "abc" 1)) (make-editor "abc" 0))

;(define (cursor-left e) e)

;; Took template from Editor

(define (cursor-left e)
  (make-editor (editor-txt e) 
               (max 0 (sub1 (editor-cp e)))))



;; Editor -> Editor
;; moves the cursor right by 1
(check-expect (cursor-right (make-editor "abc" 1)) (make-editor "abc" 2))
(check-expect (cursor-right (make-editor "abc" 3)) (make-editor "abc" 3))

;(define (cursor-right e) e)

;; Took template from Editor

(define (cursor-right e)
  (make-editor (editor-txt e)
               (min (string-length (editor-txt e)) (add1 (editor-cp e)))))



;; Editor 1String -> Editor
;; Insert the 1 character string before the cursor position
(check-expect (insert  (make-editor ""   0) "x") (make-editor "x"   1))
(check-expect (insert  (make-editor "abc" 0) "x") (make-editor "xabc" 1))
(check-expect (insert  (make-editor "abc" 1) "x") (make-editor "axbc" 2))
(check-expect (insert  (make-editor "abc" 2) "x") (make-editor "abxc" 3))

;(define (insert e s) e) ;stub

;; Took template from Editor

(define (insert e s)
  (make-editor (string-append (txt-before-cp e) s (txt-after-cp e))
               (add1 (editor-cp e))))



;; Editor  -> Editor
;; Delete 1 character before the cursor position
(check-expect (backspace  (make-editor "abcdef" 0)) (make-editor "abcdef" 0))
(check-expect (backspace  (make-editor "abcdef" 1)) (make-editor "bcdef" 0))
(check-expect (backspace  (make-editor "abcdef" 2)) (make-editor "acdef" 1))

;(define (backspace e) e) ;stub

;; Took template from Editor

(define (backspace e)
  (if (> (editor-cp e) 0)
      (make-editor (string-append (substring (editor-txt e) 0 (sub1 (editor-cp e)))
                                  (txt-after-cp e))
                   (sub1 (editor-cp e)))
      e))



;; Editor -> String
;; produce the text before the cursor position
(check-expect (txt-before-cp (make-editor ""    0)) "")
(check-expect (txt-before-cp (make-editor "abc" 0)) "")
(check-expect (txt-before-cp (make-editor "abc" 1)) "a")
(check-expect (txt-before-cp (make-editor "abc" 3)) "abc")

;(define (txt-before-cp e) "") ;stub

;; Took template from Editor

(define (txt-before-cp e)
  (substring (editor-txt e) 0 (editor-cp e)))



;; Editor -> String
;; produce the text after the cursor position
(check-expect (txt-after-cp (make-editor ""    0)) "")
(check-expect (txt-after-cp (make-editor "abc" 3)) "")
(check-expect (txt-after-cp (make-editor "abc" 2)) "c")
(check-expect (txt-after-cp (make-editor "abc" 0)) "abc")

;(define (txt-after-cp e) "") ;stub

;; Took template from Editor

(define (txt-after-cp e)
  (substring (editor-txt e) (editor-cp e)))