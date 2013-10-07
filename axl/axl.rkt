#lang racket
(require redex "utils.rkt")

(define-extended-language L MT
  ; source program syntax
  [(p q) (m ... e)]
  [m (def x v)]
  [e v x (e e) (o e ...) (if e e e)]
  [v (λ (x) e) b ⊤]
  [b #t #f n empty INT PROC]
  [o o1 o2]
  [o1 o? ac add1]
  [o2 cons * + - =]
  [o? int? false? cons? empty? proc? tt]
  [ac car cdr]
  [n integer]
  [(x y z X Y Z) variable-not-otherwise-mentioned]
  [(x* y* z* f* l*) (x ...)]
  [n• n INT]
  [n+⊤ n• ⊤]
  [PROC• PROC (⇓ (λ (x) e) ρ)]
  
  ; run-time configuration
  [P (m ... E)]
  [E A (: e ρ) (E E) (o E ...) (if E E E) (rt V V E) (blur V E) (wait V V)]
  [A (in-hole H Err) V]
  [A* (A ...)]
  [V (⇓ (λ (x) e) ρ) (Cons V V) b ⊤ ⊥ (! X) (μ (X) V*)]
  [V* (V ...)]
  [ρ ([x ↦ V] ...)]
  [ac* (ac ...)]
  
  ; evaluation context
  [H hole (H E) (V H) (o V ... H E ...) (if H E E) (rt V V H) (blur V H)]
  [Q (m ... H)]
  
  [P+Q P Q]
  [B #t #f])

(define (ev t)
  (list->set (map last (apply-reduction-relation* ↦ (term (inj ,t)) #:cache-all? #t))))
(define (ev∘ t)
  (list->set (map last (apply-reduction-relation* ↦∘ (term (inj ,t)) #:cache-all? #t))))

(define (viz t)
  (traces ↦ (term (inj ,t))))
(define (viz∘ t)
  (traces ↦∘ (term (inj ,t))))

(define-metafunction L
  AMB : e e ... -> e
  [(AMB e) e]
  [(AMB e_1 e ...) (if ⊤ e_1 (AMB e ...))])

(define-metafunction L
  COND : [e e] ... [ELSE e] -> e
  [(COND [ELSE e]) e]
  [(COND [e_1 e_2] any ...) (if e_1 e_2 (COND any ...))])

(define-metafunction L
  AND : e ... -> e
  [(AND) #t]
  [(AND e) e]
  [(AND e_1 e ...) (if e_1 (AND e ...) #f)])

(define-metafunction L
  OR : e ... -> e
  [(OR) #f]
  [(OR e) e]
  [(OR e_1 e ...) (if e_1 #t (OR e ...))])

(define-metafunction L
  LET* : ([x e] ...) e -> e
  [(LET* () e) e]
  [(LET* ([x e_x] any ...) e) ((λ (x) (LET* (any ...) e)) e_x)])

(define-metafunction L
  inj : p -> P
  [(inj (m ... e))
   (m ...
    (def havoc (λ (x) (AMB (havoc (x ⊤)) (havoc (car x)) (havoc (cdr x)))))
    (: e {}))])

(define ↦
  (reduction-relation
   L
   #:domain P
   [==> (: v ρ) (close v ρ)  v]
   [==> (: x ρ) V
        x
        (where (any ... [x ↦ V] any_i ...) ρ)]
   [--> (m ... (in-hole H (: x ρ)))
        (m ... (in-hole H (close v {})))
        ref
        (where (any_1 ... (def x v) any_i ...) (m ...))]
   [==> (: (e_1 e_2) ρ) ((: e_1 ρ) (: e_2 ρ))  @-e]
   [==> (: (o e ...) ρ) (o (: e ρ) ...)  o-e]
   [==> (: (if e ...) ρ) (if (: e ρ) ...)  if-e]
   
   [==> (o V ...) A
        δ
        (where (any_1 ... A any_i ...) (δ o V ...))]
   [==> (if V E_1 E_2) E_1
        if-t
        (where (any_1 ... #f any_i ...) (δ false? V))]
   [==> (if V E_1 E_2) E_2
        if-f
        (where (any_1 ... #t any_i ...) (δ false? V))]
   [==> (rt V_f V_x V) V
        rt]
   [==> ((⇓ (λ (x) e) ρ) V) (: e (ρ+ ρ x V))  β]
   [==> (V V_x) Err
        app-err
        (where (any_1 ... #f any_i ...) (δ proc? V))]
   [==> (V V_x) ⊤
        app-opq
        (where (any_1 ... #t any_i ...) (δ proc? V))
        (side-condition (not (redex-match? L (⇓ any_v any_ρ) (term V))))]
   [==> (V V_x) ((: havoc {}) V_x)
        havoc
        (where (any_1 ... #t any_i ...) (δ proc? V))
        (side-condition (not (redex-match? L (⇓ any_v any_ρ) (term V))))]
   
   
   with
   [(--> (in-hole Q E_1) (in-hole Q E_2)) (==> E_1 E_2)]))

(define ↦∘
  (extend-reduction-relation
   ↦ L
   #:domain P+Q
   [--> (m ... (in-hole H ((⇓ (λ (x) e) ρ) V)))
        (m ... (in-hole H (rt (⇓ (λ (x) e) ρ) V (: e (ρ+ ρ x V)))))
        β
        (where [#f] (app-seen? H (⇓ (λ (x) e) ρ)))]
   [--> (m ... (in-hole H ((⇓ (λ (x) e) ρ) V)))
        (m ... (in-hole H (rt (⇓ (λ (x) e) ρ) V_n (: e (ρ+ ρ x V_n)))))
        β-axl
        (where V_o (app-seen? H (⇓ (λ (x) e) ρ)))
        (where V_n (+: V_o V))
        (side-condition (term (≠ V_o V_n)))]
   [--> (m ... (in-hole H ((⇓ (λ (x) e) ρ) V)))
        (m ... (in-hole H (wait (⇓ (λ (x) e) ρ) V_o)))
        β-wait
        (where V_o (app-seen? H (⇓ (λ (x) e) ρ)))
        (where V_o (+: V_o V))]
   
   [--> (in-hole Q (blur V_0 V_1))
        (in-hole Q (+: V_0 V_1))
        #;(where any ,(printf "~a  +:  ~a  =  ~a~n~n"
                              (term V_0) (term V_1) (term (+: V_0 V_1))))
        blur]))

(define (ev* t)
  ; extract modules (m ...)
  (match-define `(,m ... ,_) t)
  
  ; helper functions for queues
  (define (push! h k) (hash-set! h k #f))
  (define (map-add! h k v)
    (hash-update! h k (λ (s) (set-add s v)) {set}))
  (define (mem? h k) (hash-has-key? h k))
  (define (mt? h) (zero? (hash-count h)))
  (define (pop! h)
    (let ([k (for/first ([k (in-hash-keys h)]) k)])
      (hash-remove! h k)
      k))
  
  ; returns next states from 1-step reduction
  (define (step E)
    (let ([Es (apply-reduction-relation ↦∘ E)])
      #;(printf "Es:~n~a~n~n" Es)
      Es))
  
  (define (final? E)
    (redex-match? L (m ... A) E))
  
  ; checks for state of form (return ...)
  (define (match-rt E)
    (match (redex-match L (m ... (in-hole H (rt V_f V_x V))) E)
      [(list m)
       (let ([bs (match-bindings m)])
         (define-values (Vf Vx V) (values #f #f #f))
         (for ([b (in-list bs)])
           (match (bind-name b)
             ['V_f (set! Vf (bind-exp b))]
             ['V_x (set! Vx (bind-exp b))]
             ['V (set! V (bind-exp b))]
             [_ (void)]))
         (list Vf Vx V))]
      [#f #f]))
  
  ; checks for state of form (wait ...)
  (define (match-wt E)
    (match (redex-match L (m ... (in-hole H (rt V_f V_x (in-hole H_k (wait V_f V_x))))) E)
      [(list m)
       (let ([bs (match-bindings m)])
         (define-values (H Vf Vx Hk) (values #f #f #f #f))
         (for ([b (in-list bs)])
           (match (bind-name b)
             ['H (set! H (bind-exp b))]
             ['V_f (set! Vf (bind-exp b))]
             ['V_x (set! Vx (bind-exp b))]
             ['H_k (set! Hk (bind-exp b))]
             [_ (void)]))
         (list H Vf Vx Hk))]
      [#f #f]))
  
  ; plug function's result into the hole expecting it
  (define (recombine H H_k V_f V_x V_fx)
    (term (,@m (in-hole ,H (rt ,V_f ,V_x (blur ,V_fx (in-hole ,H_k ,V_fx)))))))
  
  ; main loop
  (let ([front (make-hash (list (cons (term (inj ,t)) #f)))]
        [seen (make-hash)]
        [finals (make-hash)]
        [returns (make-hash)]
        [waits (make-hash)])
    (let loop ()
      (unless (mt? front)
        (let ([E (pop! front)])
          (for ([Ei (step E)])
            (cond
              [(final? Ei) (push! finals Ei)]
              [(not (mem? seen Ei))
               (push! front Ei)
               (match (match-rt Ei)
                 [(list Vf Vx V)
                  (map-add! returns (list Vf Vx) V)
                  (for ([ctx (hash-ref waits (list Vf Vx) (λ () {set}))])
                    (match-let* ([(list H Hk) ctx]
                                 [Ej (recombine H Hk Vf Vx V)])
                      (unless (mem? seen Ej) (push! front Ej))))]
                 [#f (void)])
               (match (match-wt Ei)
                 [(list H Vf Vx Hk)
                  (map-add! waits (list Vf Vx) (list H Hk))
                  (for ([V0 (hash-ref returns (list Vf Vx) (λ () {set}))])
                    (let ([Ej (recombine H Hk Vf Vx V0)])
                      (unless (mem? seen Ej) (push! front Ej))))]
                 [#f (void)])]
              [else (void)]))
          (push! seen E))
        (loop)))
    (for/set ([P (in-hash-keys finals)])
      (match-let ([`(,m ... ,E) P]) E))))

;; ρ operations
(define-metafunction L
  ρ+ : ρ x V -> ρ
  [(ρ+ ρ x V) (env+ ρ x V)])
(define-metafunction L
  close : v ρ -> V
  [(close (λ (x) e) ρ) (⇓ (λ (x) e) ρ)]
  [(close v ρ) v])

;; checks whether current context results from given application
(define-metafunction L
  app-seen? : H V -> [#f] or V
  [(app-seen? hole V) (#f)]
  [(app-seen? (in-hole H (rt V V_x hole)) V) V_x]
  [(app-seen? (in-hole H (rt V_f V_x hole)) V) (app-seen? H V)]
  [(app-seen? (in-hole H (hole E)) V) (app-seen? H V)]
  [(app-seen? (in-hole H (V_f hole)) V) (app-seen? H V)]
  [(app-seen? (in-hole H (o E ... hole E_1 ...)) V) (app-seen? H V)]
  [(app-seen? (in-hole H (if hole E_1 E_2)) V) (app-seen? H V)]
  [(app-seen? (in-hole H (blur V_0 hole)) V) (app-seen? H V)])

;; biased approximation
(define-metafunction L
  +: : V V -> V
  [(+: ⊥ V) V]
  [(+: V ⊥) V]
  [(+: V V) V]
  [(+: n•_1 n•_2) INT]
  [(+: (Cons V_1 V_2) (Cons V_3 V_4)) (Cons (+: V_1 V_3) (+: V_2 V_4))]
  [(+: (μ (X) V*) (μ (Y) {V ...})) (μ! (X) (+:: V*_1 {(V// V (! Y) (! X)) ...}))]
  [(+: (μ (X) V*) V) (μ! (X) (+:: V* {(V// V (μ (X) V*) (! X))}))]
  [(+: V_0 V_1)
   (μ! (X) {V_0 V_i})
   (where X ,(variable-not-in (term (V_0 V_1)) (term X)))
   (where V_i (V// V_1 V_0 (! X)))
   (side-condition (term (≠ V_i V_1)))]
  [(+: (⇓ v ρ_1) (⇓ v ρ_2)) (⇓ v (ρ+: ρ_1 ρ_2))]
  [(+: PROC•_1 PROC•_2) PROC]
  [(+: V_0 V_1) ⊤])

(define-metafunction L
  ρ+: : ρ ρ -> ρ
  [(ρ+: ρ {}) ρ]
  [(ρ+: (any_1 ... [x ↦ V_0] any_2 ...) ([x ↦ V_1] any ...))
   (ρ+: (any_1 ... [x ↦ (+: V_0 V_1)] any_2 ...) (any ...))])

(define-metafunction L
  μ! : (X) V* -> V
  [(μ! (X) {any ... ⊤ any_1 ...}) ⊤]
  [(μ! (X) V*) (μ (X) V*)])

(define-metafunction L
  +:: : V* V* -> V*
  [(+:: (V_1 ...) (V_2 ...))
   ,(match (term (+:* V_1 ... V_2 ...))
      [(list _ ... '⊤ _ ...) (term {⊤})]
      [V* (set->list (list->set V*))])])
(define-metafunction L
  +:* : V ... -> V*
  [(+:* (V_a ... ⊤ V_b ...)) {⊤}]
  [(+:* V) {V}]
  [(+:*) {}]
  [(+:* V V_a ... V V_b ...)
   (V V_r ...)
   (where (V_r ...) (+:* V_a ... V_b ...))]
  [(+:* n•_1 V_a ... n•_2 V_b ...)
   ((+: n•_1 n•_2) V_r ...)
   (where (V_r ...) (+:* V_a ... V_b ...))]
  [(+:* B_1 V_a ... B_2 V_b ...)
   (B_1 B_2 V_r ...)
   (where (V_r ...) (+:* V_a ... V_b ...))]
  [(+:* PROC•_1 V_a ... PROC•_2 V_b ...)
   ((+: PROC•_1 PROC•_2) V_r ...)
   (where (V_r ...) (+:* V_a ... V_b ...))]
  [(+:* V_1 V_a ... V_b ...)
   (V_1 V_r ...)
   (where (V_r ...) (+:* V_a ... V_b ...))]
  [(+:* (Cons V_1 V_2) V_a ... (Cons V_3 V_4) V_b ...)
   ((+: (Cons V_1 V_2) (Cons V_3 V_4)) V_r ...)
   (where (V_r ...) (+:* V_a ... V_b ...))])

;; closure substitution
(define-metafunction L
  V// : V V V -> V
  [(V// V_X V_X V_Y) V_Y]
  [(V// (μ (X) any) X V) (μ (X) any)]
  [(V// (Cons V_1 V_2) V_X V_Y) (Cons (V// V_1 V_X V_Y) (V// V_2 V_X V_Y))]
  [(V// V_Z V_X V_Y) V_Z])

(define-metafunction L
  δ : o V ... -> A*
  ; predicate
  [(δ tt V) {#t}]
  [(δ o? ⊤) {#t #f}]
  [(δ int? n•) {#t}]
  [(δ false? #f) {#t}]
  [(δ cons? (Cons V_1 V_2)) {#t}]
  [(δ empty? empty) {#t}]
  [(δ proc? PROC) {#t}]
  [(δ proc? (⇓ any any_ρ)) {#t}]
  [(δ o? V) {#f}]
  
  [(δ add1 n) (δ + 1 n)]
  
  [(δ car (Cons V_1 V_2)) {V_1}]
  [(δ cdr (Cons V_1 V_2)) {V_2}]
  [(δ ac ⊤) {⊤ Err}]
  [(δ ac V) {Err}]
  [(δ cons V_1 V_2) {(Cons V_1 V_2)}]
  
  [(δ + n_1 n_2) {,(+ (term n_1) (term n_2))}]
  [(δ + n• ...) {INT}]
  [(δ + n+⊤ ...) {INT Err}]
  
  [(δ - n_1 n_2) {,(- (term n_1) (term n_2))}]
  [(δ - n• ...) {INT}]
  [(δ - n+⊤ ...) {INT Err}]
  
  [(δ * n_1 n_2) {,(* (term n_1) (term n_2))}]
  [(δ * n•_1 ... 0 n•_2 ...) {0}]
  [(δ * n• ...) {INT}]
  [(δ * n+⊤ ...) {INT Err}]
  
  [(δ = n_1 n_2) {,(= (term n_1) (term n_2))}]
  [(δ = n•_1 n•_2) {#t #f}]
  [(δ = n+⊤_1 n+⊤_2) {#t #f Err}]
  
  [(δ o V ...) {Err}])


;;;;; EXAMPLES
(define p-fac
  (term
   ((def factorial
      (λ (n) (if (= n 0) 1 (* n (factorial (- n 1))))))
    (factorial INT))))
(define p-fg
  (term
   ((def f
      (λ (n) (if ⊤ 1 (+ (g n) (f n)))))
    (def g
      (λ (n) (if ⊤ 2 (+ (f n) (g n)))))
    (f ⊤))))
(define p-fgf
  (term
   ((def f
      (λ (x) (if ⊤ 1 (+ (g x) (+ (f x) (g x))))))
    (def g
      (λ (z) (if ⊤ 2 (+ (f z) (+ (g z) (f z))))))
    (f ⊤))))
(define p-ack
  (term
   ((def ackermann
      (λ (m)
        (λ (n)
          (if (= m 0) (+ n 1)
              (if (= n 0) ((ackermann (- m 1)) 1)
                  ((ackermann (- m 1)) ((ackermann m) (- n 1))))))))
    ((ackermann INT) INT))))
(define p-fib
  (term
   ((def fib
      (λ (n)
        (if ⊤ 1 (+ (fib (- n 1)) (fib (- n 2))))))
    (fib INT))))
(define p-app
  (term
   ((def append
      (λ (xs)
        (λ (ys)
          (if (empty? xs) ys
              (cons (car xs) ((append (cdr xs)) ys))))))
    ((append ⊤) empty))))
(define p-mutual
  (term
   ((def f
      (λ (x) (if ⊤ 1 (if ⊤ (+ 2 (f x)) (+ 3 (g x))))))
    (def g
      (λ (x) (if ⊤ 4 (if ⊤ (+ 5 (+ (f x) (g x))) (+ 6 (g x))))))
    (f ⊤))))
(define p-mutual-cons
  (term
   ((def f
      (λ (x) (if (= x 0) #f (cons (- x 1) (g (- x 1))))))
    (def g
      (λ (x) (if (= x 0) #t (cons (- x 1) (f (- x 1))))))
    (f INT))))
(define p-map
  (term
   ((def map
      (λ (f)
        (λ (xs)
          (if (empty? xs) empty
              (cons (f (car xs)) ((map f) (cdr xs)))))))
    ((map ⊤) ⊤))))
(define p-diverge
  (term
   ((def f (λ (x) (f (f x))))
    (f ⊤))))
(define p-with-many
  (term
   ((def with-many
      (λ (wf)
        (λ (l)
          (λ (f)
            (COND
             [(empty? l) (f l)]
             [ELSE (LET* ([x (car l)]
                          [xs (cdr l)])
                         ((wf x) (λ (x1) (((with-many wf) xs) (λ (xs1) (f (cons x1 xs1)))))))])))))
    (((with-many ⊤) ⊤) ⊤))))
(define p-takl
  (term
   ((def listn
      (λ (n) (if (= n 0) empty (cons n (listn (- n 1))))))
    (def shorter?
      (λ (x)
        (λ (y)
          (AND (false? (empty? y))
               (OR (empty? x) ((shorter? (cdr x)) (cdr y)))))))
    (def mas
      (λ (x)
        (λ (y)
          (λ (z)
            (if (false? ((shorter? y) x)) z
                (((mas (((mas (cdr x)) y) z))
                  (((mas (cdr y)) z) x))
                 (((mas (cdr z)) x) y)))))))
    (((mas ⊤) ⊤) ⊤))))
(define p-tak-cps
  (term
   ((def tak
      (λ (x)
        (λ (y)
          (λ (z)
            (λ (k)
              (AMB (k z)
                   ((((tak (- x 1))
                        y)
                        z)
                        (λ (v1)
                          ((((tak (- y 1))
                               z)
                               x)
                               (λ (v2)
                                 ((((tak (- z 1))
                                      x)
                                      y)
                                      (λ (v3) ((((tak v1) v2) v3) k)))))))))))))
    ((((tak INT) INT) INT) (λ (x) x)))))