(*|
---
title: Exploring Subtyping in Implicit Polarized F with Rocq
date: 2026-09-09
summary: |
  I had some spare time this past weekend, and I thought I'd spend
  some of that time diving into one of my favorite papers: [Implicit
  Polarized F: local type inference for impredicativity][mercer22]
  by Mercer et al. This is a partial mechanization in Rocq.
---

I had some spare time this past weekend, and I thought I'd spend
some of that time diving into one of my favorite papers: [Implicit
Polarized F: local type inference for impredicativity][mercer22]
by Mercer et al.

[mercer22]: https://arxiv.org/abs/2203.01835

I love this paper because it tackles a hard problem in a very clever
way. Type inference for System F has been known to be undecidable
for a long time, and there is a lot of research out there exploring
ways to make it tractable, often by using black magic.

This particular paper solves the problem by leveraging the bipolar nature
of [call-by-push-value][cbpv-wiki].  With some clever tricks, the paper
is able to provide an inference algorithm that is not only decidable,
but it _doesn't require unification_!  There are limitations of course,
but this particular flavor of black magic is a good one.

[cbpv-wiki]: https://en.wikipedia.org/wiki/Call-by-push-value

This is a literate Rocq file that explores the subtyping algorithm
presented in the paper. The source can be downloaded [here](./subtyping.v).
|*)

From Stdlib Require Import String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.
Open Scope list_scope.
Create HintDb subtyping.

(*|
# Core Syntax

The syntax we're dealing with is this one:

<figure>
$$
\begin{array}{rrcl}
\text{Positive types} & P & ::= & \alpha \mid \hat{\alpha} \mid {\downarrow}N \\
\text{Negative types} & N & ::= & P \to N \mid \forall \alpha.\, N \mid {\uparrow}P \\
\end{array}
$$
<figcaption>
Types in Implicit Polarized F, taken from the paper.
</figcaption>
</figure>

Like System F, types consist of variables, functions ($P \to N$),
and abstractions ($\forall \alpha.\, N$).  Because we're working with
call-by-push-value, types are partitioned into _values_ and _computations_.
We assign polarities to each: values are positive, and computations
are negative.  The only positive types are variables (though in theory
this would include things like `Int` and `Bool`).  Everything else is
negative. There are two shift operators ${\uparrow}P$ and ${\downarrow}N$
that allow us to change polarity at will.  The paper doesn't include
existential variables ($\hat{\alpha}$) until later, but I've preemptively
added them here. Existentials are only relevant to algorithmic subtyping.

Types are defined as follows:
|*)

Variant var :=
  | Vbound : nat -> var
  | Vfree : string -> var.

Variant polarity := pos | neg.

Inductive ty : polarity -> Type :=
  | Tuvar : var -> ty pos               (* Universals *)
  | Tevar : string -> ty pos            (* Existentials *)
  | Tdown : ty neg -> ty pos            (* ↓ N *)
  | Tfun : ty pos -> ty neg -> ty neg   (* P → N *)
  | Tforall : ty neg -> ty neg          (* ∀ *)
  | Tup : ty pos -> ty neg.             (* ↑ P *)

Coercion Vbound : nat >-> var. 
Coercion Vfree : string >-> var.
Coercion Tuvar : var >-> ty.

(*|
This implementation uses [locally nameless][chargueraud2011]
variables[^nameless]. Any variable that lives under a binder is
represented as a DeBruijn index (`Vbound`), and any free variable is a
string (`Vfree`). Types are indexed by their polarity.

[chargueraud2011]: https://www.chargueraud.org/viewabs.php?sCode=2009%2Fln

[^nameless]: This post is actually my second attempt at mechanization.
  The first time I made the bold decision to use string variables
  everywhere. I thought I could get away without DeBruijn indices,
  but as time went on, the amount of ceremony required to avoid variable
  captures became tedious. Some of the declarative subtyping rules
  needed many extra premises to ensure variable freshness (the
  `Dforallr` rule in particular), and I eventually gave up and
  decided to rethink it.

The only thing worth noting is that existential variables (`Tevar`)
are _always_ strings.  Universal variables can be introduced by a
binder (`∀`), but there is no equivalent constructor for existentials.
Existential variables live in a global namespace. They also live in
a distinct namespace from universal variables; `a` and `â` do not
collide.

Next we define some notation that gets us close to the notation
in the paper. The best we can do for hats is `^a`:
|*)

Declare Scope ty_scope.
Delimit Scope ty_scope with ty.
Bind Scope ty_scope with ty.

Notation "$ k" := (Vbound k)
  (at level 1, only parsing) : ty_scope.
Notation "^ x" := (Tevar x)
  (at level 1, format "^ x") : ty_scope.
Notation "↑ P" := (Tup P)
  (at level 2, format "↑ P") : ty_scope.
Notation "↓ N" := (Tdown N)
  (at level 2, format "↓ N") : ty_scope.
Notation "P → N" := (Tfun P N)
  (at level 99, right associativity) : ty_scope.
Notation "∀ P" := (Tforall P)
  (at level 10, P at level 99) : ty_scope.

Implicit Types (i j k : nat).
Implicit Types (a b c : string).
Implicit Types (P Q : ty pos).
Implicit Types (N M : ty neg).

(*|
## Syntax Examples

The paper has a nice table of example type signatures in figure 6, a few
of which are presented here.  The examples use extensions such as lists,
pairs, and integers, which we define as axioms.
|*)

Axiom Tint : ty pos.
Axiom Tbool : ty pos.
Axiom Tstring : ty pos.

Axiom Tlist : ty pos -> ty pos.
Notation "[ P ]" := (Tlist P) : ty_scope.

Axiom Tpair : ty pos -> ty pos -> ty pos.
Notation "P * Q" := (Tpair P Q) : ty_scope.

Module Examples.
Example id : ty pos := ↓(∀ 0 → ↑0).
Example head : ty pos := ↓(∀ [0] → ↑0).
Example tail : ty pos := ↓(∀ [0] → ↑[0]).
Example map : ty pos := ↓(∀ ∀ ↓(1 → ↑0) → [1] → ↑[0]).
Example flip : ty pos := ↓(∀ ∀ ∀ ↓(2 → 1 → ↑0) → (1 → 2 → ↑0)).
End Examples.

(*|
(There's actually a small typo in the paper! The signature for `map`
is missing a `↑`.)

# Operations on Types

The term `subst x B A` replaces all occurrences of the free variable `x`
in `A` with `B`.  Because type variables are always positive, the term
`B` that we're substituting in must also be positive.
|*)

Fixpoint subst {p} (x : string) (B : ty pos) (A : ty p) : ty p :=
  match A with
  | Tuvar (Vbound i) => i
  | Tuvar (Vfree y) => if x =? y then B else Tuvar y
  | (^y)%ty => ^y
  | (↓N)%ty => ↓(subst x B N)
  | (↑P)%ty => ↑(subst x B P)
  | (P → N)%ty => (subst x B P) → (subst x B N)
  | (∀ N)%ty => ∀ (subst x B N)
  end.

(*|
The `esubst` function is similar to `subst`, except that it replaces
existential variables instead of universal variables.
|*)

Fixpoint esubst {p} (x : string) (S : ty pos) (A : ty p) : ty p :=
  match A with
  | Tuvar y => Tuvar y
  | Tevar y => if x =? y then S else Tevar y
  | (↓N)%ty => ↓(esubst x S N)
  | (↑P)%ty => ↑(esubst x S P)
  | (P → N)%ty => (esubst x S P) → (esubst x S N)
  | (∀ N)%ty => ∀ (esubst x S N)
  end.

(*|
The `instantiate` function is a specialization of `subst` that
replaces _bound_ variables with a given term. In the locally-nameless
literature this is typically called "open". This is very useful for
instantiating a quantifier (e.g. instantiating `P` in `(∀ A)` becomes
`[$0 := P]A`).
|*)

Fixpoint instantiate k (B : ty pos) {p} (A : ty p) : ty p :=
  match A with
  | Tuvar (Vbound i) => if (Nat.eqb i k) then B else i
  | Tuvar (Vfree a) => a
  | (^y)%ty => ^y
  | (↓N)%ty => ↓(instantiate k B N)
  | (↑P)%ty => ↑(instantiate k B P)
  | (P → N)%ty => (instantiate k B P) → (instantiate k B N)
  | (∀ N)%ty => ∀ (instantiate (S k) B N)
  end.

(*|
Next we define notations for substitutions.  The paper uses the
notation `[P/x]A`, but we use `[x:=P]A` instead because the forward
slash conflicts with the builtin notation for `Nat.div`. [Oh
well][steele].

[steele]: https://www.youtube.com/watch?v=7HKbjYqqPPQ&t=1868s
|*)

Notation "[ x := P ] A" := (subst x P A)
  (A at level 1, format "[ x := P ] A") : ty_scope.
Notation "[ ^ x := P ] A" := (esubst x P A)
  (A at level 1, format "[ ^ x := P ] A") : ty_scope.
Notation "[ $ k := P ] A" := (instantiate k P A)
  (A at level 1, format "[ $ k := P ] A") : ty_scope.

(*|
A `ground` type doesn't contain any existential variables:
|*)

Fixpoint ground {p} (A : ty p) : Prop :=
  match A with
  | Tuvar y => True
  | Tevar y => False
  | (↓N)%ty => ground N
  | (↑P)%ty => ground P
  | (P → N)%ty => ground P /\ ground N
  | (∀ N)%ty => ground N
  end.

Hint Unfold ground : subtyping.

(*|
# Contexts

For declarative subtyping, contexts are just lists of strings. However,
later in the paper, contexts are extended to support existential variables
for algorithmic subtyping.  We jump the gun here and define contexts
that can handle both.

Contexts are defined in section 4, figure 8 as follows:

$$
\Theta ::= \cdot
  \mid \Theta, \alpha
  \mid \Theta, \hat{\alpha}
  \mid \Theta, \hat{\alpha} = P
$$

A context element is one of three possible values: (1) a universal
variable $\alpha$, (2) an _unsolved_ existential variable $\hat{\alpha}$
or (3) a _solved_ existential variable $\hat{\alpha}$ coupled with
its solution $P$.  Context elements are called _hypotheses_ (our
terminology), and are defined as follows. Case (2) and (3) are
combined into a single constructor and are distinguished by an
optional solution.
|*)

Inductive hyp :=
  | Huvar : string -> hyp
  | Hevar : string -> option (ty pos) -> hyp.

Declare Scope ctx_scope.
Delimit Scope ctx_scope with ctx.
Bind Scope ctx_scope with hyp.

Notation "^ x [ = P ]" := (Hevar x P)
  (at level 1, format "^ x [ =  P ]") : ctx_scope.
Notation "^ x" := (Hevar x None)
  (at level 1, format "^ x") : ctx_scope.
Notation "^ x = P" := (Hevar x (Some P))
  (at level 1, format "^ x  =  P") : ctx_scope.

Coercion Huvar : string >-> hyp.

(*|
The syntax `^x[= P]` means that there is an existential variable
`x` which may or may not be solved with some solution `P`. The
solution `P` has type `option (ty pos)`.  This notation is a little
weird, but it's taken from the paper. Its only appearance is in the
`Aforalll` rule defined in figure 9.  If we happen to know whether
or not `x` is solved, then we prefer the notation `^x` for unsolved
existential variables and `^x = P` for solved existential variables.

Now we define contexts. Contexts are effectively `list hyp`, but we
define them using a custom, concrete type in order to abuse notation.
|*)

Inductive context :=
  | Cempty : context
  | Ccons : context -> hyp -> context.

Bind Scope ctx_scope with context.

Notation "[]" := Cempty : ctx_scope.
Notation "G , H" := (Ccons G H)
  (at level 61, left associativity) : ctx_scope. 
Notation "[ H ]" := (Ccons Cempty H)
  (at level 0) : ctx_scope. 
Notation "[ H3 ; .. ; H2 ; H1 ]" := 
  (Ccons (Ccons .. (Ccons Cempty H3) .. H2) H1)
  (at level 0): ctx_scope.

Implicit Types (G : context).

(*|
I'll admit I got a little crazy with the notation here.  Contexts
are _snoc lists_: the head of the context is on the right hand side!
The context `[a; b; c]%ctx` is isomorphic to the list `[c; b; a]`.
This might seem like an unusual choice, but it's important to note
that contexts for algorithmic subtyping are _ordered_ contexts (we're not
allowed to move hypotheses around). We'll see later that the `Arefl`
and `Ainst` rules split the context, and its sub derivations use
the "left" side of the context, which is the tail.

## Context Operations

We can apply an entire context to a given term. Doing so substitutes
all solved existential variables with their respective solution.
Defined in figure 17.
|*)

Fixpoint apply G {p} (A : ty p) : ty p :=
  match G with
  | []%ctx => A
  | (G', ^x)%ctx => apply G' A
  | (G', ^x = P)%ctx  => apply G' [^x := P]A
  | (G', _)%ctx => apply G' A
  end.

(*|
The functions `uvars` (and `evars`) return all the universal
(or existential) variables in a context.
|*)

Fixpoint uvars G : list string :=
  match G with
  | []%ctx => nil
  | (G', Huvar x)%ctx => x :: uvars G'
  | (G', _)%ctx => uvars G'
  end.

Fixpoint evars G : list string :=
  match G with
  | []%ctx => nil
  | (G', Hevar x _)%ctx => x :: evars G'
  | (G', _)%ctx => evars G'
  end.

(*|
# Well-formed Types

A well-formed type is one whose variables are all bound to a given
context. The proof `wf G k p A` states that all free variables in
`A` exist in `G`, and all bound variables in `A` are less than `k`
(`p` is just the polarity of `A`).  The term `wf [] 0 _ A` is proof
that `A` is closed and ground.

This definition was adapted from figure 2. The `Twfuvar` rule is
split into `Twfbvar` and `Twffvar` in order to handle locally-nameless
variables. The `Twfguess` rule is added as well to handle existential
variables. It's defined later in the paper in figure 13.
|*)

Inductive wf G : nat -> forall p, ty p -> Prop :=
  | Twfbvar : forall k i,
      i < k -> wf G k pos i
  | Twffvar : forall k a,
      In a (uvars G) -> wf G k pos a
  | Twfguess : forall k a,
      In a (evars G) -> wf G k pos ^a
  | Twfshiftdown : forall k N,
      wf G k neg N -> wf G k pos ↓N
  | Twfforall : forall k N,
      wf G (S k) neg N -> wf G k neg (∀ N)
  | Twfarrow : forall k P N,
      wf G k pos P -> wf G k neg N -> wf G k neg (P → N)
  | Twfshiftup : forall k P,
      wf G k pos P -> wf G k neg ↑P.

(*|
We adopt the notation for well-formed types from the paper.  Note that
this notation implicitly states that the well-formed term is _locally
closed_ (all bound variables have a binder).
|*)

Notation "G ⊢ P type⁺" := (wf G 0 pos P)
  (at level 90, format "G  ⊢  P  type⁺").
Notation "G ⊢ N type⁻" := (wf G 0 neg N)
  (at level 90, format "G  ⊢  N  type⁻").

Hint Constructors wf : subtyping.

(*|
The following lemmas are particularly useful for solving the premises
to `Twffvar` and `Twfguess` using `eauto with subtyping`.
|*)

Lemma in_uvars_eq : forall (x : string) G,
  In x (uvars (G, x)).
Proof.
  intros. simpl uvars. apply in_eq.
Qed.

Lemma in_uvars_cons : forall x G,
  In x (uvars G) -> forall y, In x (uvars (G, y)).
Proof.
  simpl. destruct y; auto.
  apply in_cons. auto.
Qed.

Lemma in_evars_eq : forall (x : string) G,
  In x (evars (G, ^x)).
Proof.
  intros. simpl evars. apply in_eq.
Qed.

Lemma in_evars_cons : forall x G,
  In x (evars G) -> forall y, In x (evars (G, y)).
Proof.
  simpl. destruct y; auto.
  apply in_cons. auto.
Qed.

Hint Resolve in_uvars_eq : subtyping.
Hint Resolve in_uvars_cons : subtyping.
Hint Resolve in_evars_eq : subtyping.
Hint Resolve in_evars_cons : subtyping.

(*|
# Declarative Subtyping

Now we can finally get to the definition of declarative subtyping:
|*)

Reserved Notation "G ⊢ Q ≤⁺ P"
  (at level 90).
Reserved Notation "G ⊢ M ≤⁻ N"
  (at level 90).

Inductive dsubtype : forall p, context -> ty p -> ty p -> Prop :=
  | Drefl : forall G (a : string),
      G ⊢ a type⁺ ->
      G ⊢ a ≤⁺ a
  | Dshiftdown : forall G N M,
      G ⊢ M ≤⁻ N -> G ⊢ N ≤⁻ M ->
      G ⊢ ↓N ≤⁺ ↓M
  | Dforallr : forall G (a : string) N M,
      (G, a) ⊢ N ≤⁻ [$0:=a]M -> ~ In a (uvars G) ->
      G ⊢ N ≤⁻ ∀ M
  | Dforalll : forall G P N M,
      G ⊢ P type⁺ -> G ⊢ [$0:=P]N ≤⁻ M ->
      G ⊢ ∀ N ≤⁻ M
  | Darrow: forall G Q P N M,
      G ⊢ Q ≤⁺ P -> G ⊢ N ≤⁻ M ->
      G ⊢ (P → N) ≤⁻ (Q → M)
  | Dshiftup : forall G P Q,
      G ⊢ Q ≤⁺ P -> G ⊢ P ≤⁺ Q ->
      G ⊢ ↑P ≤⁻ ↑Q
  where "G ⊢ Q ≤⁺ P" := (dsubtype pos G Q%ty P)
    and "G ⊢ M ≤⁻ N" := (dsubtype neg G M%ty N).

Hint Constructors dsubtype : subtyping.

(*|
The rules are exactly the same as the paper, except for `Dforallr`.
The `Dforallr` rule behaves just like the ${\forall}R$ rule of the
sequent calculus: the quantifier is instantiated with an eigenvariable.
This eigenvariable must be _fresh_, and cannot exist in the context.
This is implicit in the paper, but we have to add an additional premise
that the eigenvariable `a` is `~ In a (uvars G)`.

To see how declarative subtyping works, it's best to walk through
a trivial example:
|*)

Example example_derivation :
  [] ⊢ ∀ ↑0 ≤⁻ ∀ ↑0.
Proof with eauto with subtyping.
(*|
The first thing we can do is eliminate the quantifier on the right-hand
side. We do so by introducing the eigenvariable "a" and adding it to
the context.  `eauto` is able to infer that "a" is fresh.
|*)
  apply Dforallr with "a"... simpl.
(*|
Now we can instantiate the quantifier on the left-hand side by
instantiating it with "a":
|*)
  apply Dforalll with "a"... simpl.
(*|
The remainder of the proof is straightforward. We apply `Dshiftup` to
eliminate the `↑`s. We then need to prove that "a" is equivalent
to "a", which is trivial.
|*)
  apply Dshiftup.
  apply Drefl...
  apply Drefl...
Qed.

(*|
We can see a couple limitations of the declarative definition at
this point.  First, it's non-deterministic. When presented with `[]
⊢ ∀ N ≤⁻ ∀ M`, we have the option to apply either `Dforalll`
or `Dforallr`. Second, the `Dforalll` rule requires us to choose an
arbitrary term `P` with which to instantiate the left-hand side.

The algorithmic approach addresses the first issue by adding a premise
that disallows quantifiers on the right-hand side. In other words,
in algorithmic subtyping, we have to continuously apply `Aforallr`
and eliminate quantifiers on the right hand side before we can apply
`Aforalll`. The second problem is solved by the use of existential
variables, as we'll see later.

Let's go through another example. We can push quantifiers across
function boundaries, so long as they don't cross a shift.
|*)

Example example_push_quantifiers :
  ["a"] ⊢ "a" → ∀ ↑0 ≤⁻ ∀ ("a" → ↑0).
Proof with eauto with subtyping.
  apply Dforallr with "b"... simpl.
  apply Darrow...
  apply Dforalll with "b"... simpl.
  apply Dshiftup.
  apply Drefl...
  apply Drefl...
  { simpl. destruct 1. discriminate. trivial. }
Qed.

(*|
# Algorithmic Subtyping

Now we get to algorithmic subtyping.

Curiously, contexts used in algorithmic subtyping are _ordered_.  We're not
allowed to reorder hypotheses in the context. This subtlety is important
for the `Arefl` and `Ainst` rules defined below. Both of these require that
we first locate a variable in the context, split the context, and then
use the hypotheses that _precede_ the given variable in sub-derivations.
This is a requirement for preserving well-formedness, and guarantees that
solutions to existentials only depend on preceding type variables in the
context. This is similar to a telescoping context in dependent type theory.

To deal with "splitting the context", we use a zipper. We define a function
`zip L R` that effectively works like `rev_append`: the context `R` is added
to the front of `L` in reverse order:  
|*)

Fixpoint zip (L R : context) : context :=
  match R with 
  | []%ctx => L
  | (R', x)%ctx => zip (L, x) R'
  end.

(*|
We now make `zip` opaque so that it no longer reduces with `simpl`,
`cbn`, etc. We then define two lemmas that let us open, close, and
single-step a zipper.
|*)

Opaque zip.

Lemma zip_emptyr : forall L,
  zip L [] = L.
Proof. reflexivity. Qed.

Lemma zip_step : forall (L R : context) (a : hyp),
  zip (L, a) R = zip L (R, a).
Proof. reflexivity. Qed.

(*|
The following example shows how we can use a zipper to split contexts. We
consider the head of the left-hand side to be the "focus".
|*)

Example example_zip_tactics :
  ["a"; "b"; "c"]%ctx = ["a"; "b"; "c"]%ctx.
Proof.
  rewrite <- zip_emptyr at 1.  (* Open the zipper. *)
  rewrite zip_step.            (* Focus is now "b". *)
  rewrite zip_step.            (* Focus is now "a". *)
  rewrite zip_step.            (* There is no focus. *)
  do 3 rewrite <- zip_step.    (* Undo our traversal. *)
  rewrite zip_emptyr.          (* Close the zipper. *)
  reflexivity.
Qed.

(*|
Now we can finally define algorithmic subtyping:
|*)

Reserved Notation "ctx ⊢ Q ≤⁺ P ⊣ ctx'"
  (at level 90).
Reserved Notation "ctx ⊢ M ≤⁻ N ⊣ ctx'"
  (at level 90).

Inductive asubtype : forall p, context -> ty p -> ty p -> context -> Type :=
  | Arefl : forall (a : string) L R,
      zip (L, a) R ⊢ a ≤⁺ a ⊣ zip (L, a) R
  | Ainst : forall a P (L R : context),
      L ⊢ P type⁺ -> ground P ->
      zip (L, ^a) R ⊢ P ≤⁺ ^a ⊣ zip (L, ^a = P) R
  | Ashiftdown : forall M N G0 G1 G2,
      G0 ⊢ M ≤⁻ N ⊣ G1 ->
      G1 ⊢ N ≤⁻ (apply G1 M) ⊣ G2 ->
      G0 ⊢ ↓N ≤⁺ ↓M ⊣ G2
  | Aforalll : forall N M G0 G1 a (P : option (ty pos)),
      ~ (exists M', M = (∀ M')%ty) -> ~ In a (evars G0) -> 
      (G0, ^a) ⊢ [$0:=^a]N ≤⁻ M ⊣ (G1, ^a[= P]) ->
      G0 ⊢ (∀ N) ≤⁻ M ⊣ G1
  | Aforallr : forall N M G0 G1 (a : string),
      (G0, a) ⊢ N ≤⁻ [$0:=a]M ⊣ (G1, a) -> ~ In a (uvars G0) ->
      G0 ⊢ N ≤⁻ (∀ M) ⊣ G1
  | Aarrow : forall Q P N M G0 G1 G2,
      G0 ⊢ Q ≤⁺ P ⊣ G1 ->
      G1 ⊢ (apply G1 N) ≤⁻ M ⊣ G2 ->
      G0 ⊢ (P → N) ≤⁻ (Q → M) ⊣ G2
  | Ashiftup : forall Q P G0 G1 G2,
      G0 ⊢ Q ≤⁺ P ⊣ G1 ->
      G1 ⊢ (apply G1 P) ≤⁺ Q ⊣ G2 ->
      G0 ⊢ ↑P ≤⁻ ↑Q ⊣ G2
  where "ctx ⊢ Q ≤⁺ P ⊣ ctx'" := (asubtype pos ctx Q P ctx')
    and "ctx ⊢ M ≤⁻ N ⊣ ctx'" := (asubtype neg ctx M N ctx').

(*|
Its definition is almost exactly like the paper's, except that, like
declarative subtyping, an extra premise is needed in `Aforallr` to
guarantee that the eigenvariable is fresh. Same goes for `Aforalll`.

This context on the right-hand side is an _output_, which makes things a
little awkward. The output context in premises often contain variables
that don't exist in the conclusion.  This means that when constructing
derivations from the bottom-up, we're left with existentials (Rocq
existentials, not existentials in the language) that won't be solved
until we hit the `Ainst` rule.

It's best seen with an example:
|*)

Example asubtype_forall_example :
  [] ⊢ ∀ ↑0 ≤⁻ ∀ ↑0 ⊣ [].
Proof with eauto with subtyping; try solve [intuition].
(*|
The `Aforallr` rule behaves just like its declarative counterpart.
|*)
  eapply Aforallr with "a"... simpl.
(*|
Unlike `Dforalll`, `Aforalll` works by instantiating the left-hand side
with a fresh existential variable. This allows us to continue the proof
deterministically. Instead of making a choice of what to instantiate
with the left-hand side now, we instead solve for the existential
variable later.
|*)
  eapply Aforalll with (a := "e")...
(*|
We need a proof that the right hand side isn't a quantifier:
|*)
  { intros H. destruct H. discriminate. } simpl.
(*|
Now we apply `Ashiftup`. The clever part of the algorithm is that we
require shifted terms to be isomorphic. This puts the existential
on the right-hand side of the subtyping relation, setting it up
to be unified with the variable "a".
|*)
  eapply Ashiftup.
  rewrite <- zip_emptyr at 1.
  eapply Ainst... 
  rewrite zip_emptyr.
  simpl apply.
(*|
Now we just need to focus both contexts on "a" and apply `Arefl`.
|*)
  rewrite <- zip_emptyr at 1 2.
  rewrite zip_step.
  apply Arefl...
Qed.

(*|
Pretty neat!
|*)