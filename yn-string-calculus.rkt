; An implementation of a computational calculus using the "yn-string" representation of combinators
; described in Diller (2011).
;
; Reference
; =========
;
; Diller, Antoni (2001). Efficient bracket abstraction using iconic representations for combinators.
; Available online at https://www.cantab.net/users/antoni.diller/papers/efficient.pdf.

#lang racket
(require data/bit-vector)
(require rackunit)

(define (flip f)
  (λ (x y) (f y x)))

; Syntax
; ======

; We will represent Diller's "yn-strings" as bit vectors, with 1 corresponding to 'y' and 0
; corresponding to 'n'. Variables, and primitive combinators that are not represented by yn-strings,
; will be represented as symbols, while applications will be represented as lists, where the first
; element is the head and the second element is the argument.

(define comb? (flat-rec-contract c symbol? bit-vector? (list/c c c)))

(define/contract (app t . us) (->* (comb?) () #:rest (listof comb?) comb?)
  (foldl (flip list) t us))

; For better readability, we also represent combinators as "combinator expressions", which are just
; like combinators except they may contain lists of arbitrary nonzero length; such lists represent
; combinators formed by repeated left-associative application.

(define comb-e? (flat-rec-contract c string? symbol? (non-empty-listof c)))

(define/contract (comb t) (-> comb-e? comb?)
  (match t
    [(? string? φ)   (string->bit-vector φ)]
    [(? symbol? x)   x]
    [(list t us ...) (apply app (comb t) (map comb us))]))

(module+ test
  (check-equal? (comb '("10110" x y z u v w))
                `((((((,(bit-vector #t #f #t #t #f) x) y) z) u) v) w)))

; For every combinator t, there is a unique nonempty list of combinators a, u_1, ..., u_n such that a
; is atomic (i.e. a variable or a primitive combinator); these combinators are called the primal
; components of t. By decomposing t into its primal components we can translate it into a more
; readable combinator expression.

(define atom? (or/c symbol? bit-vector?))

(define/contract (primal-components t) (-> comb? (cons/c atom? (listof comb?)))
  (let loop ([t t] [us '()])
    (match t
      [`(,t ,u) (loop t (cons u us))]
      [a        (cons a us)])))

(module+ test
  (check-equal? (primal-components `((,S (,K ,S)) ,K))
                (list S `(,K ,S) K)))

(define/contract (comb-e t) (-> comb? comb-e?)
  (match-let ([(list a ts ...) (primal-components t)])
    (let ([a-e (match a
                 [(? symbol? x)     x]
                 [(? bit-vector? φ) (bit-vector->string φ)])])
      (if (null? ts)
          a-e
          (cons a-e (map comb-e ts))))))

; Reduction
; =========

; Each bit vector φ = (φ_1, ..., φ_n) has the associated reduction rule
;
;   φ t_1 ... t_(n + 1) → u_1 ... u_n,
;
; where t_1, ..., t_(n + 1) stand for arbitrary combinators, and u_1, ..., u_n are defined by
;
;         { t_k t_(n + 1)   if φ_k = 1,
;   u_k = {                                    (k ∈ {1, ..., n})
;         { t_k             if φ_k = 0.
;
; So, in less precise language, a bit vector of size n takes n + 1 arguments, removes the last one
; from its initial position, and re-inserts it as the argument of each of the former n arguments whose
; position corresponds to a 1 in the bit vector.
;
; In the cases ϕ = "11" and ϕ = "0", we recover the usual reduction rules for S and K:
;
;  "11" t u v → t v (u v),
;  "0" t u   → t,
;
; where t and u stand for arbitrary combinators. So we needn't have any additional reduction rules for
; other primitive combinators; all primitive combinators can be represented via bit vectors.
;
; The following procedure implements normal-order reduction, based on the reduction rules for
; bit vectors and the usual auxiliary rules of
;
;   tu → t′u   if t → t′,
;   tu → tu′   if u → u′.
;
; The procedure is written for clarity rather than efficiency.

(define/contract (reduce t) (-> comb? (or/c comb? #f))
  (let ([u
         (match-let ([(cons φ ts) (primal-components t)])
           (if (symbol? φ)
               #f
               (let ([n (bit-vector-length φ)])
                 (if (<= (length ts) n)
                     #f
                     (let ([ts (take ts n)]
                           [u (list-ref ts n)]
                           [us (drop ts (add1 n))])
                       (apply app
                              (apply app (map (λ (b t)
                                                    (if b
                                                        `(,t ,u)
                                                        t))
                                                  (bit-vector->list φ) ts))
                              us))))))])
    (or u
        (match t
          [`(,t ,u) (let ([v (reduce t)])
                      (if v
                          `(,v ,u)
                          (let ([v (reduce u)])
                            (and v `(,t ,v)))))]
          [_        #f]))))

(define/contract (reduce* t) (-> comb? comb?)
  (let ([u (reduce t)])
    (if u
        (reduce* u)
        t)))

(module+ test
  (check-equal? (reduce* (comb '("10110" x y z u v w)))
                (comb '(x w y (z w) (u w) v))))

; Here are bit vector representations for some well-known combinators:

(define K (comb "0")) ; K t u → t
(define B (comb "01")) ; B t u v → t (u v)
(define C (comb "10")) ; C t u v → t v u
(define S (comb "11")) ; S t u v → t v (u v)
(define B′ (comb "001")) ; B′ t u v w → t u (v w)
(define C′ (comb "010")) ; C′ t u v w → t (u w) v
(define S′ (comb "011")) ; S′ t u v w → t (u w) (v w)

(module+ test
  (check-equal? (comb-e (reduce* (app K 't 'u))) 't)
  (check-equal? (comb-e (reduce* (app B 't 'u 'v))) '(t (u v)))
  (check-equal? (comb-e (reduce* (app C 't 'u 'v))) '(t v u))
  (check-equal? (comb-e (reduce* (app S 't 'u 'v))) '(t v (u v)))
  (check-equal? (comb-e (reduce* (app B′ 't 'u 'v 'w))) '(t u (v w)))
  (check-equal? (comb-e (reduce* (app C′ 't 'u 'v 'w))) '(t (u w) v))
  (check-equal? (comb-e (reduce* (app S′ 't 'u 'v 'w))) '(t (u w) (v w))))

; The identity combinator I (with the reduction rule I t = t) has no direct representation as a
; bit vector, but it can of course still be represented as S K K, where S and K are represented as
; bit vectors.

(define I (app S K K))

(module+ test
  (check-equal? (reduce* (app I 't)) 't))

; Translation to the SK basis
; ===========================

; To prove that yn-strings are really combinators, Diller describes a procedure for translating an
; arbitrary yn-string into a combinator over the SK basis (i.e. one in which the only atomic
; combinators are S and K).
;
; The procedure makes use of various auxiliary combinators defined in terms of S and K, and for
; debugging it is useful to see which auxiliary combinators are being used, rather than expanding
; everything all at once. These auxiliary combinators can be thought of as variables which have been
; bound to a combinator by an environment. So, we will think of the targets of our translation as
; "variable combinators"---combinators in which all atomic combinators are variables (though they may
; be bound to a combinator in the environment, and thus effectively act as primitive combinators).
(define var-comb? (flat-rec-contract c symbol? (list/c c c)))

; The auxiliary combinators we need, besides S and K, are I, B, C, and a sequence of combinators
; B_0, B_1, B_2, ... defined by the recursion
;
;   B_0       = B,
;   B_(n + 1) = B B_n B.
;
; We will represent these by the symbols 'B0, 'B1, 'B2, ... Note that we start this sequence at the
; index 0, rather than 1 as in Diller's paper.

(define/contract (B_ n) (-> natural? symbol?)
  (string->symbol (string-append "B" (number->string n))))

; The following procedure encodes the environment: it takes a variable and returns the combinator it
; is bound to, or #f if it is a free variable.

(define/contract (env-ref x) (-> symbol? (or/c var-comb? #f))
  (match x
    ['I (comb '(S K K))]
    ['B (comb '(S (K S) K))]
    ['C (comb '(S (B B S) (K K)))]
    [_  (match (string->list (symbol->string x))
          [`(#\B . ,n)
           (let ([n (string->number (apply string n))])
             (if n
                 (if (zero? n)
                     'B
                     (app 'B (B_ (sub1 n)) 'B))
                 #f))]
          [_
           #f])]))

; The actual translation procedure is pretty simple:

(define/contract (bit-vector->var-comb φ) (-> bit-vector? var-comb?)
  (let loop ([bs (reverse (bit-vector->list φ))] [n (bit-vector-length φ)])
    (match bs
      [(list #t)        '(B I)]
      [(list #f)        'K]
      [(list #t bs ...) (app (B_ (- n 2)) 'S (loop bs (sub1 n)))]
      [(list #f bs ...) (app (B_ (- n 2)) 'C (loop bs (sub1 n)))])))

(module+ test
  (check-equal? (bit-vector->var-comb (comb "10110"))
                (comb '(B3 C (B2 S (B1 S (B0 C (B I))))))))

; To fully expand a "variable combinator", obtaining a combinator in which the only atomic combinators
; are S and K, we can use the following procedure. Note that this usually produces very large
; combinators when combined with `bit-vector->var-comb`.

(define/contract (expand t) (-> var-comb? var-comb?)
  (match t
    [(? symbol? x) (let ([t (env-ref x)])
                     (if t
                         (expand t)
                         x))]
    [`(,t ,u)      `(,(expand t) ,(expand u))]))

; Bracket abstraction
; ===================

(define/contract (occurs-in? x t) (-> symbol? comb? boolean?)
  (match t
    [(? symbol? y) (eq? x y)]
    [`(,t ,u)      (or (occurs-in? x t) (occurs-in? x u))]
    [_             #f]))

(define/contract (brack-abs x t) (-> symbol? comb? comb?)
  #;(displayln (format "(brack-abs ~a ~a)" x (comb-e t)))
  (let ([ts (primal-components t)])
    (match-let ([(list (list bs us) ...) (map (λ (t)
                                                (cond [(eq? x t)        (list #t I)]
                                                      [(occurs-in? x t) (list #t (brack-abs x t))]
                                                      [else             (list #f t)]))
                                              ts)])
      (apply app (apply bit-vector bs) us))))

(module+ test
  (check-equal? (comb-e (brack-abs 'x (comb '(x (y z) (z y x) (z (x y))))))
                `("1011" ,(comb-e I)
                         (y z)
                         ("001" z y ,(comb-e I))
                         ("01" z ("10" ,(comb-e I) y))))
  (let ([t (brack-abs 'x (brack-abs 'y (brack-abs 'z (comb '(x y z (z x z))))))]
        [u (comb `("001001" "00100" "0011" ,(comb-e I) ,(comb-e I) ,(comb-e I)
                           ("00010" "0000" "101" ,(comb-e I) ,(comb-e I) ,(comb-e I))))])
    ; Diller's paper says the result should be this...
    #;(check-equal? (comb-e t) (comb-e u))
    ; ...but I get this instead.
    (check-equal? (comb-e t)
                  `("001001" "00100" "0011" ,(comb-e I) ,(comb-e I) ,(comb-e I)
                             ("0010" "101" ,(comb-e I) ,(comb-e I) ,(comb-e I))))
    ; And indeed,
    (check-equal? (comb-e (reduce* (app t 'x 'y 'z))) '(x y z (z x z)))
    ; while
    (check-equal? (comb-e (reduce* (app u 'x 'y 'z))) `(x y z ("101" ,(comb-e I) x ,(comb-e I))))
    ; So this may be an error in the paper.
    ))

; Types
; =====

; This isn't something addressed in the paper, but I was wondering: what is the type of a bit vector?

; Let's implement type inference...

(define type? (flat-rec-contract c symbol? (list/c '→ c c)))

(define/contract (fun-type A . Bs) (->* (type?) () #:rest (listof type?) type?)
  (match Bs
    ['() A]
    [_   `(→ ,A ,(apply fun-type Bs))]))

(module+ test
  (check-equal? (fun-type 'a 'b 'c) '(→ a (→ b c))))

(define type-env? (hash/c symbol? type?))

(define type-e? (flat-rec-contract c symbol? (cons/c '→ (listof c))))

(define/contract (type-e A) (-> type? type-e?)
  (match A
    [(? symbol? a) a]
    [_             (let loop ([As '()] [B A])
                     (match B
                       [`(→ ,A ,B) (loop (cons (type-e A) As) B)]
                       [_          `(→ ,@(reverse As) ,(type-e B))]))]))

(module+ test
  (check-equal? (type-e (fun-type 'a 'b 'c)) '(→ a b c))
  (check-equal? (type-e (fun-type (fun-type 'a 'b 'c) 'd 'e)) '(→ (→ a b c) d e)))

(define/contract (type-expand s A) (-> type-env? type? type?)
  (match A
    [(? symbol? a) (if (hash-has-key? s a)
                       (type-expand s (hash-ref s a))
                       a)]
    [`(→ ,A ,B)    `(→ ,(type-expand s A) ,(type-expand s B))]))

(define/contract (occurs-in-type? a A [s (hasheq)]) (->* (symbol? type?) (type-env?) boolean?)
  (when (hash-has-key? s a)
    (raise-arguments-error 'occurs-in-type? "variable undergoing occurs check is bound"
                           "variable" a "environment" s))
  (match A
    [(? symbol? b) (cond [(eq? a b)           #t]
                         [(hash-has-key? s b) (occurs-in-type? a (hash-ref s b) s)]
                         [else                #f])]
    [`(→ ,A ,B)    (or (occurs-in-type? a A s) (occurs-in-type? a B s))]))

(define/contract (mgu A B [s (hasheq)]) (->* (type? type?) (type-env?) (or/c type-env? #f))
  #;(displayln (format "(mgu ~a ~a ~a)" A B s))
  (match* (A B)
    [((? symbol? a) A)       (cond [(eq? a A)                             s]
                                   [(hash-has-key? s a)                   (mgu (hash-ref s a) A s)]
                                   [(and (symbol? A) (hash-has-key? s A)) (mgu a (hash-ref s A) s)]
                                   [(occurs-in-type? a A s)               #f]
                                   [else                                  (hash-set s a A)])]
    [(A (? symbol? a))       (mgu a A s)]
    [(`(→ ,A ,B) `(→ ,C ,D)) (let ([s (mgu A C s)])
                               (and s (mgu B D s)))]))

(module+ test
  (check-equal? (mgu 'a 'b) (hasheq 'a 'b))
  (check-equal? (mgu '(→ a b) '(→ b a)) (hasheq 'a 'b))
  (check-equal? (mgu '(→ (→ a b) (→ b a)) '(→ c c)) (hasheq 'c '(→ a b) 'a 'b))
  (check-equal? (mgu '(→ a (→ b (→ d d))) '(→ b (→ a a))) (hasheq 'a 'b 'b '(→ d d))))

(define/contract (infer-type t [s (hasheq)]) (->* (comb?) (type-env?) (cons/c type? type-env?))
  (match t
    ['S       (let ([a (gensym)] [b (gensym)] [c (gensym)])
               (cons (fun-type (fun-type a b c) (fun-type a b) (fun-type a c)) s))]
    ['K       (let ([a (gensym)] [b (gensym)])
               (cons (fun-type a b a) s))]
    [`(,t ,u) (match (infer-type t s)
                [#f #f]
                [(cons A s) (match (infer-type u s)
                              [#f #f]
                              [(cons B s)
                               (let ([a (gensym)])
                                 (cons a (mgu A (fun-type B a) s)))])])]))

(define/contract (type-vars-in A) (-> type? (listof symbol?))
  (match A
    [(? symbol? a) (list a)]
    [`(→ ,A ,B)    (remove-duplicates (append (type-vars-in A) (type-vars-in B)))]))

(define type-var-letters "abcdefghijklmnopqrstuvwxyz")

(define/contract (index->type-var i) (-> natural? symbol?)
  (string->symbol
   (let-values ([(q r) (quotient/remainder i (string-length type-var-letters))])
     (string-append
      (string (string-ref type-var-letters r))
      (if (zero? q) "" (number->string q))))))

(define/contract (norm-type A) (-> type? type?)
  (let* ([as (remove-duplicates (type-vars-in A))]
         [s (make-hash (append-map (λ (a i)
                                     (let ([b (index->type-var i)])
                                       (if (eq? a b)
                                           '()
                                           (list (cons a b)))))
                                   as (range (length as))))])
    (type-expand s A)))

(define/contract (type-of t) (-> comb? type-e?)
  (match (infer-type t)
    [#f         #f]
    [(cons A s) (type-e (norm-type (type-expand s A)))]))

(define/contract (string-type φ) (-> string? type-e?)
  (type-of (expand (bit-vector->var-comb (comb φ)))))

; Types of some bit vectors:
;
; "0"  : b       → a → b
; "1"  : (a → b) → a → b
; "00" : (b → c)     → b       → a → c
; "01" : (b → c)     → (a → b) → a → c
; "10" : (a → b → c) → b       → a → c
; "11" : (a → b → c) → (a → b) → a → c
; "000": (b → c → d)     → b       → c       → a → d
; "001": (b → c → d)     → b       → (a → c) → a → d
; "010": (b → c → d)     → (a → b) → c       → a → d
; "011": (b → c → d)     → (a → b) → (a → c) → a → d
; "100": (a → b → c → d) → b       → c       → a → d
; "101": (a → b → c → d) → b       → (a → c) → a → d
; "110": (a → b → c → d) → (a → b) → c       → a → d
; "111": (a → b → c → d) → (a → b) → (a → c) → a → d

; OK, so the pattern is:
; (b_1 ... b_n) : A → B_1 → ... → B_(n - 1) → a_1 → a_(n + 1)
;   where
;     - a_1, ..., a_(n + 1) are distinct type variables,
;     - A = (a_1 → A' if b_1 = 1, else A')
;         where A' = (a_2 → ... → a_(n + 1) if b_1 = 0, otherwise)
;     - B_k = (a_1 → a_(k + 1) if b_(k + 1) = 1, else a_(k + 1))
;
; It's not the neatest pattern, but there it is. In terms of proofs, each type corresponds to an
; inference rule where you have a major premiss with arbitrarily many antecedents, and you prove some
; of the antecedents directly (always including the first one), while you prove some other antecedents
; conditionally, using the first antecedent as a hypothesis.