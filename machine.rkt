#lang typed/racket/base
(require
 racket/match racket/set racket/list racket/bool racket/function
 "utils.rkt" "lang.rkt" "closure.rkt" "delta.rkt" "provability.rkt" "show.rkt")
(require/typed ; TODO for debugging only
 "read.rkt"
 [read-p (Any → .p)])
(provide (all-defined-out)) ; TODO

(define-data (.κ)
  (.if/κ [t : .E] [e : .E])
  (.@/κ [e* : (Listof .E)] [v* : (Listof .V)] [ctx : Sym])
  (.▹/κ [ce : (U (Pairof #f .E) (Pairof .V #f))] [l^3 : Sym^3])
  (.indy/κ [c : (Listof .V)] [x : (Listof .V)] [x↓ : (Listof .V)]
           [d : (U #f .↓)] [v? : (U #f Int)] [l^3 : Sym^3])
  ; contract stuff
  (.μc/κ [x : Sym])
  (.λc/κ [c : (Listof .e)] [c↓ : (Listof .V)] [d : .e] [ρ : .ρ] [v? : Bool])
  (.structc/κ [t : Sym] [c : (Listof .e)] [ρ : .ρ] [c↓ : (Listof .V)]))
(define-type .κ* (Listof .κ))

; ctx in e's position for pending states
(struct: .ς ([e : .E] [s : .σ] [k : .κ*]) #:transparent)
(define-type .ς+ (Setof .ς))
(define-type .ς* (U .ς .ς+))

(: final? : .ς → Bool)
(define final?
  (match-λ? (.ς (? .blm?) _ _) (.ς (.V) _ (list))))

(: inj : .e → .ς)
(define (inj e)
  (.ς (.↓ e ρ∅) σ∅ empty))

(: ev : .p → (Option .Ans))
(define (ev p)
  (match-define (.p (and m* (.m* _ ms)) accs e) p)
  (define step (step-p m* accs))
  
  (: m-opaque? : Sym → Bool)
  (define (m-opaque? x) ; TODO: expensive?
    (match-define (.m _ defs) (hash-ref ms x))
    (for/or ([d (in-hash-values defs)]
             #:when (.•ₗ? (car d)))
      #t))
  
  (: on-new-state : (Setof .ς) (Setof .ς) .ς
     → (Values (Setof .ς) (Setof .ς) (Option .Ans)))
  (define (on-new-state seen q ς)
    #;(begin ; debug
      (printf "~a~n" (show-ς ς))
      (read)
      (printf "~n"))
    (match ς
      [(.ς (and blm (.blm l⁺ _ _ _)) σ _)
       #;(printf "blame ~a~n" l⁺)
       (values
        seen
        q
        (cond [(or (equal? l⁺ '†) (equal? l⁺ '☠) (m-opaque? l⁺)) #f]
              [else (cons σ blm)]))]
      [(.ς (? .V? V) σ k)
       (begin
         (printf ".~n")
         (read)
         (define s : (Pairof Any Any) (show-Ans σ V))
         (printf "~a~n~a~n~n" (car s) (cdr s)))
       (match k
         [(list) (values seen q #f)]
         [(cons κ kᵣ)
          (match κ
            [(.@/κ '() _ _)
             (define ς↓ (canon ς))
             (cond
              [(set-member? seen ς↓) (values seen q #f)]
              [else (values (set-add seen ς↓) (set-add q ς) #f)])]
            [_ (values seen (set-add q ς) #f)])])]
      [_ (values seen (set-add q ς) #f)]))
  
  (define (print-ς [ς : .ς])
    (define it (show-ς ς))
    (printf "E:~n ~a~n" (second (first it)))
    (printf "σ:~n")
    (for ([x (rest (second it))])
      (printf " ~a~n" x))
    (printf "k:~n")
    (for ([x (rest (third it))])
      (printf " ~a~n" x)))
  
  #;(begin
    (let debug ([ς : .ς (inj e)])
      (cond
       [(final? ς)
        (printf "Final:~n")
        (print-ς ς)]
       [else
        (define next (step ς))
        (cond
         [(set? next)
          (define n (set-count next))
          (define nextl : (Listof .ς) (set->list next))
          (printf "~a next states:~n" n)
          (for ([ςᵢ (in-list nextl)] [i n])
            (printf "~a:~n" i)
            (print-ς ςᵢ)
            (printf "~n"))
          (define next-choice : Integer
            (let prompt ()
              (printf "Explore [0 - ~a]: " (- n 1))
              (match (read)
                [(and (? exact-integer? k) (? (λ ([k : Integer]) (<= 0 k (- n 1))))) k]
                [_ (prompt)])))
          (debug (list-ref nextl next-choice))]
         [else
          (printf "Only next:~n")
          (print-ς ς)
          (printf "Continue: ")
          (read)
          (debug next)])]))
    #f)
  
  (let search ([front : (Setof .ς) (set (inj e))] [seen : (Setof .ς) ∅])
    (begin ; debug
      (printf "~a~n" (set-count front))
      #;(when #t (= (set-count front) 106)
        (for ([ς front])
          (printf "~a~n" (show-ς ς)))
        (read)
        (printf "~n")
        (printf "~a~n" (set-count front))
        (error "STOP"))) 
    (cond
     [(set-empty? front) #f]
     [else
      (define-values (_seen front′ blm)
        (for/fold ([seen : (Setof .ς) seen]
                   [front′ : (Setof .ς) ∅]
                   [bns : (Option .Ans) #f])
                  ([ς front] #:unless bns)
          (match (step ς)
            [(? .ς? ς′) (on-new-state seen front′ ς′)]
            [(? set? ςs)
             (for/fold ([seen seen] [front′ front′] [bns bns]) ([ς′ ςs] #:unless bns)
               (on-new-state seen front′ ς′))])))
      (cond
       [(false? blm) (search front′ seen)]
       [else blm])])))

(define-syntax-rule (match/nd v [p e ...] ...) (match/nd: (.Ans → .ς) v [p e ...] ...))

(: step-p : .m* (Setof .st-ac) → (.ς → .ς*))
(define (step-p m* accs)  
  (match-define (.m* _ ms) m*)
  
  (: ref-e : Sym Sym → .e)
  (define (ref-e m x)
    (match-define (.m _ defs) (hash-ref ms m))
    (car (hash-ref defs x))) 
 
  (: ref-c : Sym Sym → .e)
  (define (ref-c m x)
    (match-define (.m _ decs) (hash-ref ms m))
    (match (cdr (hash-ref decs x))
      [(? .e? c) c]
      [_ (error (format "module ~a does not export ~a" m x))]))
  
  (: step-β : .λ↓ (Listof .V) Sym .σ .κ* → .ς)
  (define (step-β f Vx l σ k)
    #;(printf "Stepping ~a~n~n" (show-U σ f))
    (match-define (.λ↓ (.λ n e v?) ρ) f)
    (match v?
      [#f (if (= (length Vx) n)
              (.ς (.↓ e (ρ++ ρ Vx)) σ k)
              (.ς (.blm l 'Λ (Prim (length Vx)) (arity=/C n)) σ k))]
      [#t (if (>= (length Vx) (- n 1))
              (.ς (.↓ e (ρ++ ρ Vx (- n 1))) σ k)
              (.ς (.blm l 'Λ (Prim (length Vx)) (arity≥/C (- n 1))) σ k))]))
      
  (: step-@ : .V (Listof .V) Sym .σ .κ* → .ς*)
  (define (step-@ Vf V* l σ k)
    #;(printf "step-@:~n~a~n~a~n~n" (show-Ans σ Vf) (map (curry show-E σ) V*)) ;TODO reenable
    #;(printf "step-@:~nσ:~n~a~nf:~n~a~nx:~n~a~n~n" σ Vf V*)
    (match Vf
      [(.// U C*)
       (match U
         [(? .o? o) (match/nd (dbg/off '@ (δ σ o V* l)) [(cons σa A) (.ς A σa k)])]
         [(? .λ↓? f) (step-β f V* l σ k)]
         [(.Ar (.// (.Λ/C C* D v?) _) Vg (and l^3 (list _ _ lo)))
          (define V# (length V*))
          (define C# (length C*))
          (define n (if v? (- C# 1) #f))
          (if (if v? (>= V# (- C# 1)) (= V# C#))
              (.ς Vg σ (cons (.indy/κ C* V* '() D n l^3) k))
              (.ς (.blm l lo (Prim (length V*))(if v? (arity≥/C (- C# 1)) (arity=/C C#))) σ k))]
         [_
          (match/nd (δ σ (.proc?) (list Vf) 'Λ)
            [(cons σt (.// (.b #t) _)) (error "impossible" (show-V σ Vf))]
            [(cons σf (.// (.b #f) _)) (.ς (.blm l 'Λ Vf PROC/C) σf k)])])]
      [(and L (.L i))
       (match/nd (δ σ (.proc?) (list L) 'Λ)
         [(cons σt (.// (.b #t) _))
          (match/nd (δ σt (.arity-includes?) (list L (Prim (length V*))) 'Λ)
            [(cons σt (.// (.b #t) _))
             (match (σ@ σt i)
               [(and V (or (.// (? .λ↓?) _) (.// (? .Ar?) _))) (step-@ V V* l σt k)]
               [_ (step-• L V* l σt k)])]
            [(cons σf (.// (.b #f) _)) (.ς (.blm l 'Λ Vf (arity-includes/C (length V*))) σf k)])]
         [(cons σf (.// (.b #f) _)) (.ς (.blm l 'Λ Vf PROC/C) σf k)])]))
  
  (: ς∪ : .ς* * → .ς+)
  (define (ς∪ . ςs)
    (match ςs
      [(list) ∅]
      [(list (? .ς? ς)) {set ς}]
      [(list (? set? s)) s]
      [_ (for/fold ([acc : .ς+ ∅]) ([ςᵢ ςs])
           (cond [(set? ςᵢ) (set-union acc ςᵢ)]
                 [else (set-add acc ςᵢ)]))]))
  
  (: step-• : .L (Listof .V) Sym .σ .κ* → .ς*)
  (define (step-• Lf V* l σ k)
    (match-define (cons σ′ Lₐ) (σ+ σ))
    (ς∪ (step-havoc Lf V* σ k)
        (.ς Lₐ σ′ k)))
  
  (: step-havoc : .L (Listof .V) .σ .κ* → .ς*)
  (define (step-havoc Lf V* σ k)
    (match-define (.σ m l) σ)
    (match-define (.L α) Lf)
    (match V*
      [(list) ∅]
      [(list V)
       ;; Non-deterministically apply propriate operation then put back to unknown context
       (define x₀ (.x 0))
       (define ● (•!))
       (match V
         [(.// (.λ↓ (.λ n _ _) ρ) _)
          (define Vf (.λ↓ (.λ 1 (.@ ● (list (.@ x₀ (for/list ([_ n]) (•!)) '☠)) '☠) #f) ρ∅))
          (define σ′ (.σ (hash-set m α (→V Vf)) l))
          (step-β Vf V* '☠ σ′ k)]
         [(.// (.Ar (.// (.Λ/C cs _ _) _) _ _) _)
          (define n (length cs))
          (define Vf (.λ↓ (.λ 1 (.@ ● (list (.@ x₀ (for/list ([_ n]) (•!)) '☠)) '☠) #f) ρ∅))
          (define σ′ (.σ (hash-set m α (→V Vf)) l))
          (step-β Vf V* '☠ σ′ k)]
         [(.// (.St t Vs) _)
          (define n (length Vs))
          (for/fold ([ςs : (Setof .ς) ∅]) ([Vᵢ Vs] [i n])
            (define acc (.st-ac t n i))
            (define Vf (.λ↓ (.λ 1 (.@ ● (list (.@ acc (list x₀) '☠)) '☠) #f) ρ∅))
            (define σ′ (.σ (hash-set m α (→V Vf)) l))
            (set-add ςs (step-β Vf V* '☠ σ′ k)))]
         [(? .prim?) ∅]
         [_ ∅ #|TODO|#])]
      [_
       ;; Non-determistically havoc 1 arg
       (define ● (•!))
       (define n (length V*))
       (for/fold ([acc : (Setof .ς) ∅]) ([i n])
         (define Vf (.λ↓ (.λ n (.@ ● (list (.x i)) '☠) #f) ρ∅))
         (define σ′ (.σ (hash-set m α (→V Vf)) l))
         (set-add acc (step-β Vf V* '☠ σ′ k)))]))
  
  (: step-fc : .V .V Sym .σ .κ* → .ς*)
  (define (step-fc C V l σ k)
    (match (⊢ σ V C)
      ['Proved (.ς TT σ k)]
      ['Refuted (.ς FF σ k)]
      ['Neither
       (match C
         [(.// U D*)
          (match U
            [(and U (.μ/C x C′))
             (step-fc (unroll/C U) V l σ k)
             #|(match-define (cons σt _) (refine σ V C))
             (match-define (cons σf _) (refine σ V (.¬/C C)))
             {set (.ς TT σt k) (.ς FF σf k)}|#]
            [(.St 'and/c (list C1 C2)) (and/ς (list (.FC C1 V l) (.FC C2 V l)) σ k)]
            [(.St 'or/c (list C1 C2)) (or/ς (list (.FC C1 V l) (.FC C2 V l)) σ k)]
            [(.St '¬/c (list C′)) (.ς (.FC C′ V l) σ (cons (.@/κ '() (list (Prim 'not)) l) k))]
            [(.St/C t C*)
             (match/nd (δ σ (.st-p t (length C*)) (list V) l)
               [(cons σt (.// (.b #t) _))
                (match-define (.// (.St t V*) _) (σ@ σt V))
                (and/ς (for/list ([Vi V*] [Ci C*]) (.FC Ci Vi l)) σ k)]
               [(cons σf (.// (.b #f) _)) (.ς FF σf k)])]
            [_ (step-@ C (list V) l σ k)])]
         [(.L _) (step-@ C (list V) l σ k)])]))
  
  (: step-▹ : .V .V Sym^3 .σ .κ* → .ς*)
  (define (step-▹ C V l^3 σ k)
    #;(printf "Mon:~nC:~a~nV:~a~nσ:~a~nk:~a~n~n" C V σ k)
    (match-define (list l+ l- lo) l^3)
    (match (⊢ σ V C) ; want a check here to reduce redundant cases for recursive contracts
      ['Proved (.ς V σ k)]
      ['Refuted (.ς (.blm l+ lo V C) σ k)]
      ['Neither
       (match C
         [(.L i) ; FIXME this is wrong, need to take care of higher-order contract
          (match-define (cons σt Vt) (refine σ V C))
          (match-define (cons σf Vf) (refine σ V (.¬/C C)))
          {set (.ς Vt σt k) (.ς Vf σf k)}]
         [(.// Uc C*)
          (match Uc
            [(and (.μ/C x C′) Uc) (step-▹ (unroll/C Uc) V l^3 σ k)]
            [(.St 'and/c (list Cₗ Cᵣ)) (.ς V σ (▹/κ1 Cₗ l^3 (▹/κ1 Cᵣ l^3 k)))]
            [(.St 'or/c (list Cₗ Cᵣ))
             (.ς (.FC Cₗ V lo) σ (cons (.if/κ (.Assume V Cₗ) (.Mon Cᵣ V l^3)) k))]
            [(.St '¬/c (list D))
             (.ς (.FC D V lo) σ (cons (.if/κ (.blm l+ lo V C) (.Assume V C)) k))]
            [(.St/C t C*)
             (define n (length C*))
             (match/nd (δ σ (.st-p t n) (list V) lo)
               [(cons σt (.// (.b #t) _))
                (match-define (.// (.St t V*) _) (dbg/off '▹ (σ@ σt V)))
                (.ς (→V (.st-mk t n)) σt
                    (cons (.@/κ (for/list ([C C*] [V V*]) (.Mon C V l^3)) '() lo) k))]
               [(cons σf (.// (.b #f) _)) (.ς (.blm l+ lo V (→V (.st-p t n))) σf k)])]
            [(and Uc (.Λ/C Cx* D v?))
             (match/nd (δ σ (.proc?) (list V) lo)
               [(cons σt (.// (.b #t) _))
                (match v?
                  [#f (match/nd (δ σt (.arity-includes?) (list V (Prim (length Cx*))) lo)
                        [(cons σt (.// (.b #t) _))
                         (.ς (→V (.Ar C V l^3)) σt k)]
                        [(cons σf (.// (.b #f) _))
                         (.ς (.blm l+ lo V (arity-includes/C (length Cx*))) σf k)])]
                  [#t (match/nd (δ σt (.arity≥?) (list V (Prim (- (length Cx*) 1))) lo)
                        [(cons σt (.// (.b #t) _))
                         (.ς (→V (.Ar C V l^3)) σt k)]
                        [(cons σf (.// (.b #f) _))
                         (.ς (.blm l+ lo V (arity≥/C (- (length Cx*) 1))) σf k)])])]
               [(cons σf (.// (.b #f) _)) (.ς (.blm l+ lo V PROC/C) σf k)])]
            [_ (.ς (.FC C V lo) σ (cons (.if/κ (.Assume V C) (.blm l+ lo V C)) k))])])]))
  
  (: step-E : .E .σ .κ* → .ς*)
  (define (step-E E σ k)
    #;(printf "E: ~a~n~n" E)
    (match E
      [(.↓ e ρ)
       (match e
         [(.•ₗ n)
          (match-define (.σ m l) σ)
          (.ς (.L n) (.σ (hash-update m n identity (λ () ♦)) l) k)]
         [(? .v? v) (.ς (close v ρ) σ k)]
         [(.x sd) (.ς (ρ@ ρ sd) σ k)]
         [(.x/c x) (.ς (ρ@ ρ x) σ k)]
         [(.ref name ctx ctx) (.ς (.↓ (ref-e ctx name) ρ∅) σ k)]
         [(.ref name in ctx)
          (.ς (.↓ (ref-c in name) ρ∅) σ
              (cons (.▹/κ  (cons #f (.↓ (ref-e in name) ρ∅)) (list in ctx in)) k))]
         [(.@ f xs l) (.ς (.↓ f ρ) σ (cons (.@/κ (for/list ([x xs]) (.↓ x ρ)) '() l) k))]
         [(.if i t e) (.ς (.↓ i ρ) σ (cons (.if/κ (.↓ t ρ) (.↓ e ρ)) k))]
         [(.amb e*) (for/set: .ς ([e e*]) (.ς (.↓ e ρ) σ k))]
         [(.μ/c x e) (.ς (.↓ e (ρ+ ρ x (→V (.X/C x)))) σ (cons (.μc/κ x) k))]
         [(.λ/c '() d v?) (.ς (→V (.Λ/C '() (.↓ d ρ) v?)) σ k)]
         [(.λ/c (cons c c*) d v?) (.ς (.↓ c ρ) σ (cons (.λc/κ c* '() d ρ v?) k))]
         [(.struct/c t '()) (.ς (→V (.st-p t 0)) σ k)]
         [(.struct/c t (cons c c*)) (.ς (.↓ c ρ) σ (cons (.structc/κ t c* ρ '()) k))])]
      [(.Mon C E l^3) (.ς C σ (cons (.▹/κ (cons #f E) l^3) k))]
      [(.FC C V l) (step-fc C V l σ k)]
      [(.Assume V C)
       (match-define (cons σ′ V′) (refine σ V C))
       (.ς V′ σ′ k)]))
  
  (: step-V : .V .σ .κ .κ* → .ς*)
  (define (step-V V σ κ k)
    (match κ
      [(.if/κ E1 E2)
       (match/nd (δ σ .false/c (list V) 'Λ)
         [(cons σt (.// (.b #f) _)) (.ς E1 σt k)]
         [(cons σf (.// (.b #t) _)) (.ς E2 σf k)])]
      
      [(.@/κ (cons E1 Er) V* l) (.ς E1 σ (cons (.@/κ Er (cons V V*) l) k))]
      [(.@/κ '() V* l)
       (match-define (cons Vf Vx*) (reverse (cons V V*)))
       (step-@ Vf Vx* l σ k)]
      
      #;[(.apply/ar/κ E l) (.ς E σ (cons (.apply/fn/κ V l) k))]
      #;[(.apply/fn/κ Vf l) (step-apply Vf V l σ k)]
      
      [(.▹/κ (cons #f (? .E? E)) l^3)
       (.ς E σ (▹/κ1 V l^3 k))]
      [(.▹/κ (cons (? .V? C) #f) l^3) (step-▹ C V l^3 σ k)]
      
      
      ;; indy
      [(.indy/κ (list Ci) (cons Vi Vr) Vs↓ D n l^3) ; repeat last contract, handling var-args
       (step-▹ Ci Vi (¬l l^3) σ (cons (.indy/κ (list Ci) Vr (cons V Vs↓) D n l^3) k))]
      [(.indy/κ (cons Ci Cr) (cons Vi Vr) Vs↓ D n l^3)
       (step-▹ Ci Vi (¬l l^3) σ (cons (.indy/κ Cr Vr (cons V Vs↓) D n l^3) k))]
      [(.indy/κ _ '() Vs↓ (.↓ d ρ) n l^3) ; evaluate range contract
       (match-define (and V* (cons Vf Vx*)) (reverse (cons V Vs↓)))
       (.ς (.↓ d (ρ++ ρ Vx* n)) σ (cons (.indy/κ '() '() V* #f n l^3) k))]
      [(.indy/κ _ '() (cons Vf Vx) #f _ (and l^3 (list l+ _ _))) ; apply inner function
       #;(printf "range: ~a~n~n" (show-E σ V))
       (step-@ Vf Vx l+ σ (▹/κ1 V l^3 k))]
      
      ; contracts
      [(.μc/κ x) (.ς (→V (.μ/C x V)) σ k)]
      [(.λc/κ '() c↓ d ρ v?) (.ς (→V (.Λ/C (reverse (cons V c↓)) (.↓ d ρ) v?)) σ k)]
      [(.λc/κ (cons c c*) c↓ d ρ v?) (.ς (.↓ c ρ) σ (cons (.λc/κ c* (cons V c↓) d ρ v?) k))]
      [(.structc/κ t '() _ c↓) (.ς (→V (.St/C t (reverse (cons V c↓)))) σ k)]
      [(.structc/κ t (cons c c*) ρ c↓) (.ς (.↓ c ρ) σ (cons (.structc/κ t c* ρ (cons V c↓)) k))]))
  
  (match-lambda
    [(.ς (? .V? V) σ (cons κ k))
     (when (match? V (.// (.•) _))
       (printf "~a~n~n" (show-ς (.ς V σ (cons κ k))))
       (error "impossible"))
     (step-V V σ κ k)]
    [(.ς (? .E? E) σ k) (step-E E σ k)]))

(: and/ς : (Listof .E) .σ .κ* → .ς)
(define (and/ς E* σ k)
  (match E*
    ['() (.ς TT σ k)]
    [(list E) (.ς E σ k)]
    [(cons E Er)
     (.ς E σ (foldr (λ ([Ei : .E] [k : .κ*])
                      (cons (.if/κ Ei FF) k))
                    k Er))]))

(: or/ς : (Listof .E) .σ .κ* → .ς)
(define (or/ς E* σ k)
  (match E*
    ['() (.ς FF σ k)]
    [(list E) (.ς E σ k)]
    [(cons E Er)
     (.ς E σ (foldr (λ ([Ei : .E] [k : .κ*])
                      (cons (.if/κ TT Ei) k))
                    k Er))]))

(: ▹/κ1 : .V Sym^3 .κ* → .κ*)
(define (▹/κ1 C l^3 k)
  (match C
    [(.// (.λ↓ (.λ 1 (.b #t) _) _) _) k]
    [(.// (? .Λ/C?) _) (cons (.▹/κ (cons C #f) l^3) k)]
    [_ (cons (.▹/κ (cons C #f) l^3)
             (let trim : .κ* ([k : .κ* k])
               (match k
                 [(cons (and κ (.▹/κ (cons (? .V? D) #f) _)) kr)
                  (match (C⇒C C D)
                    ['Proved (trim kr)]
                    [_ (cons κ (trim kr))])]
                 [_ k])))]))

;;;;; small programs for testing
(define f
  (read-p
   `((module f
       (provide [f (int? . -> . int?)])
       (define (f n)
         (if (= n 0) 1 (* n (f (- n 1))))))
     (require f)
     (f 100))))

;; for debugging
(define (e x) (ev (read-p x)))

(: show-k : .σ .κ* → (Listof Any))
(define (show-k σ k) (for/list ([κ k]) (show-κ σ κ)))

(: show-κ : .σ .κ → Any)
(define (show-κ σ κ)
  (define E (curry show-E σ))
  (match κ
    [(.if/κ t e) `(if ● ,(E t) ,(E e))]
    [(.@/κ e* v* _) `(@ ,@(reverse (map E v*)) ● ,@(map E e*))]
    #;[(.apply/fn/κ Vf _) `(apply ,(E Vf) ∘)]
    #;[(.apply/ar/κ Ex _) `(apply ∘ ,(E Ex))]
    [(.▹/κ (cons #f (? .E? e)) l³) `(● ▹ ,(E e) ,l³)]
    [(.▹/κ (cons (? .E? C) #f) l³) `(,(E C) ,l³ ▹ ●)]
    [(.indy/κ Cs xs xs↓ d _ _) `(indy ,(map E Cs) ,(map E xs) ,(map E xs↓)
                                      ,(match d [#f '_] [(? .E? d) (E d)]))]
    [(.μc/κ x) `(μ/c ,x ●)]
    [(.λc/κ cs Cs d ρ _)
     `(λ/c (,@(reverse (map E Cs)) ,@(map (curry show-e σ) cs)) ,(show-e σ d))]
    [(.structc/κ t c _ c↓)
     `(struct/c ,t (,@(reverse (map E c↓)) ,(map (curry show-e σ) c)))]))

(: show-ς : .ς → (List (Listof Any) (Listof Any) (Listof Any)))
(define show-ς
  (match-lambda
    [(.ς E σ k) `((E: ,(show-E σ E))
                  (σ: ,@(show-σ σ))
                  (k: ,@(show-k σ k)))]))

; rename all labels to some canonnical form based on the expression's shape
; relax, this only happens a few times, not that expensive
(: canon : .ς → .ς)
(define (canon ς)
  (match-define (.ς (? .E? E) σ k) ς)
  (define F F∅)
  (: alloc! : Int → Int)
  (define (alloc! i)
    (match (hash-ref F i #f)
      [(? int? j) j]
      [#f (let ([j (hash-count F)])
            (set! F (hash-set F i j))
            j)]))
  
  (: go! : (case→ [.V → .V] [.↓ → .↓] [.E → .E]
                  [.μ/C → .μ/C] [.λ↓ → .λ↓] [.U → .U] [.ρ → .ρ] [.κ → .κ] [.κ* → .κ*]
                  [(Listof .V) → (Listof .V)] [(Listof .E) → (Listof .E)]
                  [.σ → .σ]))
  (define (go! x)
    (match x
      ; E
      [(.↓ e ρ) (.↓ e (go! ρ))]
      [(.FC C V ctx) (.FC (go! C) (go! V) ctx)]
      [(.Mon C E l) (.Mon (go! C) (go! E) l)]
      [(.Assume V C) (.Assume (go! V) (go! C))]
      [(.blm f g V C) (.blm f g (go! V) (go! C))]
      ; V
      [(.L i) (.L (alloc! i))]
      [(.// U C*) (.// (go! U) C*)]
      ; U
      [(.Ar C V l) (.Ar (go! C) (go! V) l)]
      [(.St t V*) (.St t (go! V*))]
      [(.λ↓ f ρ) (.λ↓ f (go! ρ))]
      [(.Λ/C C* D v?) (.Λ/C (go! C*) (go! D) v?)]
      [(.St/C t V*) (.St/C t (go! V*))]
      [(.μ/C x V) (.μ/C x (go! V))]
      [(? .X/C? x) x]
      [(? .prim? p) p]
      ; ρ
      [(.ρ m l) (.ρ (for/fold: ([m′ : (Map (U Int Sym) .V) m∅]) ([i (in-hash-keys m)])
                      (hash-set m′ i (go! (hash-ref m i))))
                    l)]
      ; κ
      [(.if/κ t e) (.if/κ (go! t) (go! e))]
      [(.@/κ e* v* l) (.@/κ (go! e*) (go! v*) l)]
      [(.▹/κ (cons C E) l)
       (.▹/κ (cond [(and (false? C) (.E? E)) (cons #f (go! E))]
                   [(and (.V? C) (false? E)) (cons (go! C) #f)]
                   [else (error "impossible!")])
             l)]
      [(.indy/κ c x x↓ d v? l)
       (.indy/κ (go! c) (go! x) (go! x↓) (if (.↓? d) (go! d) #f) v? l)]
      [(? .μc/κ? x) x]
      [(.λc/κ c c↓ d ρ v?) (.λc/κ c (go! c↓) d (go! ρ) v?)]
      [(.structc/κ t c ρ c↓) (.structc/κ t c (go! ρ) (go! c↓))]
      ;; list
      [(? list? l) (map go! l)]))

  (: fixup : (case→ [.V → .V] [.↓ → .↓] [.E → .E]
                  [.μ/C → .μ/C] [.λ↓ → .λ↓] [.U → .U] [.ρ → .ρ] [.κ → .κ] [.κ* → .κ*]
                  [(Listof .V) → (Listof .V)] [(Listof .E) → (Listof .E)]
                  [.σ → .σ]))
  (define (fixup x)
    (match x
      ;; E
      [(.↓ e ρ) (.↓ e (fixup ρ))]
      [(.FC c v l) (.FC (fixup c) (fixup v) l)]
      [(.Mon c e l) (.Mon (fixup c) (fixup e) l)]
      [(.Assume v c) (.Assume (fixup v) (fixup c))]
      [(.blm f g v c)(.blm f g (fixup v) (fixup c))]
      ;; V
      [(? .L? x) x]
      [(.// U C*) (.// (fixup U) (subst/L C* F))]
      ;; U
      [(.Ar c v l) (.Ar (fixup c) (fixup v) l)]
      [(.St t V*) (.St t (fixup V*))]
      [(.λ↓ f ρ) (.λ↓ f (fixup ρ))]
      [(.Λ/C c d v?) (.Λ/C (fixup c) (fixup d) v?)]
      [(.St/C t V*) (.St/C t (fixup V*))]
      [(.μ/C x c) (.μ/C x (fixup c))]
      [(? .X/C? x) x]
      [(? .prim? p) p]
      ;; ρ
      [(.ρ m l) (.ρ (for/fold ([m′ : (Map (U Int Sym) .V) m∅]) ([i (in-hash-keys m)])
                      (hash-set m′ i (fixup (hash-ref m i))))
                    l)]
      ;; κ
      [(.if/κ t e) (.if/κ (fixup t) (fixup e))]
      [(.@/κ e* v* l) (.@/κ (fixup e*) (fixup v*) l)]
      [(.▹/κ (cons C E) l)
       (.▹/κ (cond [(and (false? C) (.E? E)) (cons #f (fixup E))]
                   [(and (.V? C) (false? E)) (cons (fixup C) #f)]
                   [else (error "impossible!")])
             l)]
      [(.indy/κ c x x↓ d v? l)
       (.indy/κ (fixup c) (fixup x) (fixup x↓) (if (.↓? d) (fixup d) #f) v? l)]
      [(? .μc/κ? x) x]
      [(.λc/κ c c↓ d ρ v?) (.λc/κ c (fixup c↓) d (fixup ρ) v?)]
      [(.structc/κ t c ρ c↓) (.structc/κ t c (fixup ρ) (fixup c↓))]
      ;; σ
      [(.σ m _)
       #;(printf "F: ~a~nm: ~a~n~n" F m)
       (match-let ([(cons σ′ _) (σ++ σ∅ (hash-count F))])
                  (for/fold ([σ′ : .σ σ′]) ([i (in-hash-keys F)])
                    (match (hash-ref m i #f)
                      [(? .V? Vi) (σ-set σ′ (hash-ref F i) (subst/L Vi F))]
                      [#f σ′])))]
      [(? list? l) (map fixup l)]))
  
  (let* ([E′ (go! E)]
         [k′ (go! k)])
    (.ς (fixup E′) (fixup σ) (fixup k′))))

(: subst/L : (case→ [.L .F → .L]
                    [.// .F → .//]
                    [.V .F → .V]
                    [.U .F → .U]
                    [(Listof .V) .F → (Listof .V)]
                    [(Setof .V) .F → (Setof .V)]))
(define (subst/L V F)
  (: go : (case→ [.L → .L] [.// → .//]
                 [.V → .V] [.U → .U] [.ρ → .ρ]
                 [(Listof .V) → (Listof .V)] [(Setof .V) → (Setof .V)]))
  (define go
    (match-lambda
      ; V
      [(and V (.L i)) (match (hash-ref F i #f)
                        [(? int? j) (.L j)]
                        [#f V])]
      [(.// U C*) (.// (go U) (for/set: .V ([Ci C*]) (go Ci)))]
      ; U
      [(.Ar C V l) (.Ar (go C) (go V) l)]
      [(.St t V*) (.St t (go V*))]
      [(.λ↓ f ρ) (.λ↓ f (go ρ))]
      [(.Λ/C Cx (.↓ e ρ) v?) (.Λ/C (go Cx) (.↓ e (go ρ)) v?)]
      [(.St/C t V*) (.St/C t (go V*))]
      [(.μ/C x C) (.μ/C x (go C))]
      [(and U (or (? .X/C?) (? .prim?) (? .•?))) U]
      ; ρ
      [(.ρ m l)
       (.ρ
        (for/fold ([acc : (Map (U Sym Int) .V) m]) ([k (in-hash-keys m)])
          (match (hash-ref m k #f)
            [#f acc]
            [(? .V? V) (hash-set acc k (go V))]))
        l)]
      ; List
      [(? list? V*) (map go V*)]
      [(? set? s) (for/set: .V ([V s]) (go V))]))
  (go V))
