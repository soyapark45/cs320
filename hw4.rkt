#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

; Homwork #4
; Date: 9/25/2015
; soya@kaist.ac.kr

; I only made comment with description and test case for the function I created or modified.
;;TODO: error detection related to id

;; PWAE abstract syntax trees
(define-type PWAE
  [num (num number?)]
  [nums (nums (listof number?))]
  [add  (left PWAE?) (right PWAE?)]
  [sub  (left PWAE?) (right PWAE?)]
  [with (name symbol?) (init PWAE?) (body PWAE?)]
  [id   (name symbol?)])


; parse-sexpr : sexpr -> PWAE
;; It receives s-expressions then converting into PWAEs

;(parse-sexpr 3) should return (num 3)
;(parse-sexpr 'a) should return (id 'a)

(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? (listof number?)) (nums sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(list 'pooh a ...)
     (case (first (reverse a))
       [(+) (cond [(> (length a) 3) (add (parse-sexpr (first a)) (parse-sexpr (remove (first a) sexp)))]
                  [else (add (parse-sexpr (first a)) (parse-sexpr (second a)))])]
       [(-) (cond [(> (length a) 3) (sub (parse-sexpr (first a)) (parse-sexpr (append (drop-right (remove (first a) sexp) 1) '(+))))]
                  [else (sub (parse-sexpr (first a)) (parse-sexpr (second a)))])]
       )]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;test cases for parse-sexpr
(test (parse-sexpr 3) (num 3))
(test (parse-sexpr '(1 2 3)) (nums '(1 2 3)))
(test (parse-sexpr '(+ 1 2)) (add (num 1) (num 2)))
(test (parse-sexpr '(+ 1 (+ 2 3))) (add (num 1) (add (num 2) (num 3))))
(test (parse-sexpr '(- 1 2)) (sub (num 1) (num 2)))
(test (parse-sexpr '(- 1 (- 2 3))) (sub (num 1) (sub (num 2) (num 3))))
(test (parse-sexpr '(with (a 1) (+ a 2))) (with 'a (num 1) (add (id 'a) (num 2))))
(test (parse-sexpr '(with (a (+ a 2)) a)) (with 'a (add (id 'a) (num 2)) (id 'a)))
(test (parse-sexpr '(pooh 1 2 +)) (add (num 1) (num 2)))
(test (parse-sexpr '(pooh 1 2 3 +)) (add (num 1) (add (num 2) (num 3))))
(test (parse-sexpr '(pooh 1 2 -)) (sub (num 1) (num 2)))
(test (parse-sexpr '(pooh 1 2 3 -)) (sub (num 1) (add (num 2) (num 3))))
(test (parse-sexpr 'a) (id 'a))
(test/exn (parse-sexpr '(soya did homework)) "bad syntax: (soya did homework)")

;; parses a string containing a PWAE expression to a PWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

; subst: WAE symbol list-of-number -> WAE

; Get data type of WAE and replace(substitutes) from to to. 
; I add handler for nums which is the list of number(not num, this is native number). 

; (subst (with 'a (num 1) (id 'a)) 'a '(3)) should return (with 'a (num 1) (id 'a))

(define (subst expr from to)
  (type-case PWAE expr
    [num (n)   expr]
    [nums(n)   expr]
    [add (l r) (add (subst l from to) (subst r from to))]
    [sub (l r) (sub (subst l from to) (subst r from to))]
    [id (name) (if (symbol=? name from) (nums to) expr)]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr from to)
                (if (symbol=? bound-id from)
                    bound-body
                    (subst bound-body from to)))]))

; test case for subst
(test (subst (num 1) 'a 1) (num 1))
(test (subst (nums '(1)) 'a '2) (nums '(1)))
(test (subst (add (num 1) (id 'a)) 'a '(2)) (add (num 1) (nums '(2))))
(test (subst (sub (num 1) (id 'a)) 'a '(2)) (sub (num 1) (nums '(2))))
(test (subst (with 'a (add (num 5) (num 5)) (id 'a)) 'a '(5)) (with 'a (add (num 5) (num 5)) (id 'a)))
(test (subst (with 'a (add (num 5) (num 5)) (id 'b)) 'b '(5)) (with 'a (add (num 5) (num 5)) (nums '(5))))

;bin-op : (number number -> number) (listof number or number) (listof number or number) -> (listof number))
; do binary operation for list of number. Iterate each list of input and apply the operation. 

;(bin-op + '(1 2) '(2 3)) should return '(3 4 4 5) 

(define (bin-op op ls rs)
  (define (helper l rs)
    ;; f : number -> number
    (define (f n)
      (op l n)
      )
    (map f rs))
  (if (null? ls)
    null
    (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

; test case for bin-op
(test (bin-op + '() '()) '())
(test (bin-op + '() '(1 2)) '())
(test (bin-op + '(1 2) '()) '())
(test (bin-op + '(1 2 3) '(5)) '(6 7 8))
(test (bin-op - '(1 2 3) '(5)) '(-4 -3 -2))
(test (bin-op + '(1 2) '(10 15)) '(11 16 12 17))

; eval: (number number -> number) (listof number or number) (listof number or number) -> (listof number))
;; evaluates PWAE expressions by reducing them to numbers

;(eval (num 1)) should return '(1)

(define (eval expr)
  (type-case PWAE expr
    [num (n) (list n)]
    [nums(n) n]
    [add (l r) (bin-op + (eval l) (eval r))]
    [sub (l r) (bin-op - (eval l) (eval r))]
    [with (bound-id named-expr bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval named-expr)))]
    [id (name) (error 'eval "free identifier: ~s" name)]))

(test (eval (num 1)) '(1))
(test (eval (nums '(1))) '(1))
(test (eval (add (num 1) (num 1))) '(2))
(test (eval (add (num 1) (nums '(1 2)))) '(2 3))
(test (eval (sub (num 1) (nums '(1 2)))) '(0 -1))
(test (eval (sub (num 1) (nums '(1 2)))) '(0 -1))
(test (eval (with 'a (num 1) (nums '(1 2)))) '(1 2))
(test/exn (eval (id 'a)) "free identifier: a")

; run: string -> listof number
;; evaluate value of a parsed PWAE in form of list of number

; (run "2") should return '(2)

(define (run str)
  (eval (parse str)))

;(define t1 (nums (list (num 1) (num 2))))
(test (parse "2") (num 2))
(test (string->sexpr "{2 1}") '(2 1))
(test (parse "{2 1}") (nums '(2 1)))
(test (parse  "{+ {2 1} {3 4}}") (add (nums '(2 1)) (nums '(3 4))))
(test (parse "{+ 2 {3 4}}") (add (num 2) (nums '(3 4))))

(test (run "2") '(2))
(test (run "{2 1}") '(2 1))
(test (run "5") '(5))
(test (run "{+ 5 5}") '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))
(test/exn (run "a") "eval: free identifier: a")

; Followings are test cases from prof.
(test (run "{+ {2 1} {3 4}}") '(5 6 4 5))
(test (run "{+ {- {+ 1 3} 2} {10 -10}}") '(12 -8))

(test (run "{+ 3 7}") '(10))
(test (run "{- 10 {3 5}}") '(7 5))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))

(test (run "{pooh 1 2 3 4 5 +}") '(15))

(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 -}") '(-1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
(test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
(test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
(test (run "{pooh 1 2 3 4 5 +}") '(15))
(test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 3 4 +}") '(10))
(test (run "{pooh {3 4} {-4 0} 5 +}") '(4 8 5 9))
(test (run "{pooh 1 2 3 4 -}") '(-8))
(test (run "{pooh {4 1} 1 {5 6 7} -}") '(-2 -3 -4 -5 -6 -7))
(test (run "{+ {pooh 1 {4 9} -3 {2 0 1} +} {- {pooh {3 4} {2} -} 4}}") '(1 2 -1 0 0 1 6 7 4 5 5 6))
(test (run "{pooh 1 {pooh 1 2 -} 3 +}") '(3))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh {2 1} {3 4} +}") '(5 6 4 5))
(test (run "{with {x {1 2}} {pooh x {+ {1 2} 1} -}}") '(-1 -2 0 -1))
(test (run "{with {x {1 2}} {pooh x {pooh {1 2} 1 +} -}}") '(-1 -2 0 -1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {y {1 -2}} {pooh 1 y 2 -}}} {+ x x}}") '(-4 -1 -1 2))
