(** This assignment is due on {b Thursday 10/23 by 8:00PM}.
    You should put all of your programming solutions in a file called [assign6/lib/assign6.ml].
    See the file [test/test_assign6.ml] for example behavior of each function.
    You should put all your written solutions in a single pdf file.
 *)

(** {1 Programming (90%)} *)

(**
   This assignment is a miniature of the (mini-)projects you'll be doing is the second half of the course.
   We'll be building a tiny programming language.
   As we've said, a programming language has three components: syntax, typing, and semantics.
   These three parts correspond to parsing, type checking and evaluation, respectively.
*)


(** {2 Syntax} *)

(**
   Here is the syntax of the language you'll be implementing.

   {@text[
   <expr> ::= True | False | <int> | <var>
            | ( + <expr> <expr> )
            | ( - <expr> <expr> )
            | ( * <expr> <expr> )
            | ( / <expr> <expr> )
            | ( <= <expr> <expr> )
            | ( If <expr> <expr> <expr> )
            | ( Let <var> <expr> <expr> )
     <ty> ::= int | bool
   ]}

   These syntax rules (formally called a {i BNF Grammar}) tell us the following.

   - [True] is a well-formed expression.
   - [False] is a well-formed expression.
   - Integer literals are well-formed expressions.
     We will take an integer literal be any string [s] such that [int_of_string_opt s] is not [None].
   - A variable is a well-formed expression.
     {b We require that variables are made up of all lower case letters.}
     See the function [is_valid_var] in the starter code.
   - If [e1] and [e2] are well-formed expression then so are [(+ e1 e2)], [(- e1 e2)], [( * e1 e2)], [(/ e1 e2)], and [(<= e1 e2)].
   - If [e1], [e2], and [e3] are well-formed expressions the so is [(If e1 e2 e3)].
   - If [x] is a variable and [e1] and [e2] are well-formed expressions then so is [(Let x e1 e2]).

   Well-formed expressions are S-expressions with a subset of atoms that are meaningful in our language.
   Parsing an expression in this language should be nearly identical to what we did in Assignment 4.
   The result of parsing a well-formed expression should be an [expr] as defined below (formally, values of type [expr] are {i abstract syntax trees} for expression in our language).
 *)

type expr =
  | True
  | False
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lte of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

val parse : string -> expr option
(**
   Implement the function [parse] so that [parse s] is [Some e] if [s] is a well-formed expression represented by [e], and [None] otherwise.
   For example, parsing the expression
   {@text[
     (Let x 2
     (Let y (+ x x)
     (Let z (- ( * y x) 1)
     (Let q (If (<= z 0) (- 0 z) z)
     (/ q 3)))))
   ]}
   should result in [Some e] where [e] is
   {[
     Let ("x", Int 2,
     Let ("y", Add (Var "x", Var "x"),
     Let ("z", Sub (Mul (Var "y", Var "x"), Int 1),
     Let ("q", If (Lte (Var "z", Int 0), Sub (Int 0, Var "z"), Var "z"),
     Div ("q", Int 3)))))
   ]}
   Note that this expression can be written in OCaml syntax as
   {[
     let x = 2 in
     let y = x + x in
     let z = x * y - 1 in
     let q = if z <= 0 then 0 - z else z in
     q / 3
   ]}
*)

(** {2 Typing} *)
(**
   Here are the typing rules of our language.
   In them we introduce two types: [int] and [bool].
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {}
   {\Gamma \vdash \code{True} : \code{bool}}
   \text{(true)}
   \qquad
   \frac
   {}
   {\Gamma \vdash \code{False} : \code{bool}}
   \text{(false)}
   \qquad
   \frac
   {\side{n {\text{ is an int literal}}}}
   {\Gamma \vdash n : \code{int}}
   \text{(int)}
   \qquad
   \frac
   {\side{(x : t) \in \Gamma}}
   {\Gamma \vdash x : t}
   \text{(var)}
   }
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {\Gamma \vdash e_1 : \code{int} \quad \Gamma \vdash e_2 : \code{int}}
   {\Gamma \vdash \code{(} \ \code{+} \ e_1 \ e_2 \ \code{)} : \code{int}}
   \text{(add)}
   \qquad
   \frac
   {\Gamma \vdash e_1 : \code{int} \quad \Gamma \vdash e_2 : \code{int}}
   {\Gamma \vdash \code{(} \ \code{-} \ e_1 \ e_2 \ \code{)} : \code{int}}
   \text{(sub)}
   \qquad
   \frac
   {\Gamma \vdash e_1 : \code{int} \quad \Gamma \vdash e_2 : \code{int}}
   {\Gamma \vdash \code{(} \ \code{*} \ e_1 \ e_2 \ \code{)} : \code{int}}
   \text{(mul)}
   \qquad
   \frac
   {\Gamma \vdash e_1 : \code{int} \quad \Gamma \vdash e_2 : \code{int}}
   {\Gamma \vdash \code{(} \ \code{/} \ e_1 \ e_2 \ \code{)} : \code{int}}
   \text{(div)}
   }
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {\Gamma \vdash e_1 : \code{int} \quad \Gamma \vdash e_2 : \code{int}}
   {\Gamma \vdash \code{(} \ \code{<=} \ e_1 \ e_2 \ \code{)} : \code{bool}}
   \text{(lte)}
   \qquad
   \frac
   {\Gamma \vdash e_1 : \code{bool} \quad \Gamma \vdash e_2 : t \quad \Gamma \vdash e_3 : t}
   {\Gamma \vdash \code{(} \ \code{If} \ e_1 \ e_2 \ e_3 \ \code{)} : t}
   \text{(if)}
   \qquad
   \frac
   {\Gamma \vdash e_1 : t_1 \quad \Gamma, x : t_1 \vdash e_2 : t_2}
   {\Gamma \vdash \code{(} \ \code{Let} \ x \ e_1 \ e_2 \ \code{)} : t_2}
   \text{(let)}
   }


*)

(**
   We represent types in our interpreter using the [ty] ADT.
*)
type ty =
  | IntTy
  | BoolTy


(**
   We represent contexts as maps (a.k.a. a dictionaries) defined using module functors (i.e., scary stuff we won't talk about in this course).
   The point is that a value of type [ctxt] is a map from variable names of type [string] to types of type [ty].
   We provide a convenient interface for working with contexts below.
*)
type ctxt = ty Map.Make(String).t

val empty_ctxt : ctxt
(**
   [empty_ctxt] is the empty context {m \varnothing}.
*)

val add_binding : string -> ty -> ctxt -> ctxt
(**
   [add_binding] {m x \ \tau \ \Gamma} is the result of adding the binding {m x : \tau} to {m \Gamma}.
   This will be useful for implementing rules which have the context {m \Gamma, x : \tau} in their premises.
*)

val check_binding : string -> ctxt -> ty option
(**
   [check_binding] {m x \ \Gamma} is [Some] {m \tau} if {m (x : \tau) \in \Gamma} and is [None] otherwise.
   This will be useful for implementing the (var) rule.
*)

val type_of : ctxt -> expr -> ty option
(** Implement the function [type_of] so that [type_of] {m \Gamma \ e} is [Some] {m \tau} if {m e} is well-typed in {m \Gamma} (i.e., if {m \Gamma \vdash e : \tau} is derivable according to the above typing rules) and [None] otherwise.
    For example, the expression [e]
    {@text[
    (Let x 2
    (Let y (+ x x)
    ( * y y)))
    ]}
    is well-typed in the empty context, and [type_of empty_ctxt e] is [Some IntTy], where as the expression [e']
    {@text[
    (Let x 2
    (Let y (If x 1 -1)
    ( * y y)))
    ]}
    is not well-typed in the empty context (because [x] is being used as the condition of an [If]-expression) so [type_of empty_ctxt e'] is [None].
*)

(** {2 Semantics} *)

(**
   Here are the semantic rules of our language.
   In them we introduce two kinds of values: integer values and Boolean value ({m \top} for truth and {m \bot} for falsity).

   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {}
   {\code{True} \Downarrow \top}
   \text{(trueEval)}
   \qquad
   \frac
   {}
   {\code{False} \Downarrow \bot}
   \text{(falseEval)}
   \qquad
   \frac
   {\side{n {\text{ is an int literal}}}}
   {n \Downarrow n}
   \text{(intEval)}
   }
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad \side{v = v_1 + v_2}}
   {\code{(} \ \code{+} \ e_1 \ e_2 \ \code{)} \Downarrow v}
   \text{(addEval)}
   \qquad
   \frac
   {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad \side{v = v_1 - v_2}}
   {\code{(} \ \code{-} \ e_1 \ e_2 \ \code{)} \Downarrow v}
   \text{(subEval)}
   }
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad \side{v = v_1v_2}}
   {\code{(} \ \code{*} \ e_1 \ e_2 \ \code{)} \Downarrow v}
   \text{(mulEval)}
   \qquad
   \frac
   {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad \side{v = v_1 / v_2}}
   {\code{(} \ \code{/} \ e_1 \ e_2 \ \code{)} \Downarrow v}
   \text{(divEval)}
   }
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad \side{v_1 \leq v_2}}
   {\code{(} \ \code{<=} \ e_1 \ e_2 \ \code{)} \Downarrow \top}
   \text{(lteTrue)}
                                                             \qquad
   \frac
   {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad \side{v_1 > v_2}}
   {\code{(} \ \code{<=} \ e_1 \ e_2 \ \code{)} \Downarrow \bot}
   \text{(lteFalse)}
                                                             }
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {e_1 \Downarrow \top \quad e_2 \Downarrow v_2}
   {\code{(} \ \code{If} \ e_1 \ e_2 \ e_3 \ \code{)} \Downarrow v_2}
   \text{(ifTrue)}
   \qquad
   \frac
   {e_1 \Downarrow \bot \quad e_3 \Downarrow v_3}
   {\code{(} \ \code{If} \ e_1 \ e_2 \ e_3 \ \code{)} \Downarrow v_3}
   \text{(ifFalse)}
   \qquad
   \frac
   {e_1 \Downarrow v_1 \quad \side{e = [v_1/x]e_2} \quad e \Downarrow v}
   {\code{(} \ \code{Let} \ x \ e_1 \ e_2 \ \code{)} \Downarrow v}
   \text{(let)}
   }
*)

(**
   We represent values with the [value] ADT.
*)
type value =
  | IntV of int
  | BoolV of bool

val eval : expr -> value
(**
   Implement the function [eval] so that [eval] {m e} is the result of evaluating {m e}, i.e., it should be the value {m v} such that {m e \Downarrow v} is derivable.
   {i The whole point, of all of this,} is that {m e \Downarrow v} is {i guaranteed} to be derivable if {m e} is well-typed in the empty context, so we don't need to do any error handling here.
   These rules correspond to the rules for analogous OCaml syntax.
   For example, the expression [e]
   {@text[
   (Let x 2
   (Let y (+ x x)
   ( * y y)))
   ]}
   is equivalent to the OCaml expression
   {[
     let x = 2 in
     let y = x + x in
     y * y
   ]}
   So we expected [eval e] to be [IntV 16].
   {i Note:}
   When you write an evaluator, you will find that some branches of your match statements are not reachable.
   For example, when you evaluate an addition expression, you don't need to worry about the case that one of the operands evaluates to a Boolean value.
   Formally speaking, [eval] is {i undefined} on these inputs, you can put whatever you want in these branches.
   Practically speaking, the "correct" thing to do according OCaml's spec is to use the expression [assert false] to indicate undefined branches.
*)

val expr_of_value : value -> expr
(**
   One last thing.
   Our semantics depends on {i substitution} which, as we've mentioned is a bit complicated.
   We'll be nice this time around and give you the implementation of substitution, but make sure you understand it, we'll be expecting you to implement it yourself later on (and we'll talk more about substitution in the coming weeks).
   One subtlety of substitution is that we define {m [e_1 / x] e_2} (i.e., substituting {m e_1} for {m x} in {m e_2}) so that {m e_1} is an {i expression}.
   But in the above rules, we substitute a {i value} into an expression.
   Fortunately, values very easily reify to expressions.

   Implement the function [expr_of_value] so that [expr_of_value v] is the expression representing [e].
   You should use this function along with [subst] (given in the starter code) in your implementation of [eval].
*)

val interp : ?print:bool -> string -> value option
(**
   [interp] puts together parsing, type checking, and evaluation.
   This has been implemented for you.
*)

(** {1 Written (10%)} *)

(** {2 Partial Derivations}

    Not all judgments are derivable, but it's often possible to write {e partial} derivations of underivable judgments.
    These partial derivations can give us insight into why we get certain type errors when type-checking.

    Formally, a typing derivation is {i partial} if its leaves are not necessarily empty.
    A partial derivation is {i maximal} if every leaf is empty (i.e., there are no judgments to derive) or is an axiom that is not derivable according to the rules of the system.
    For example:
    {@text[
                        ───────────(int)
        ∅ ⊢ 1 : bool    ∅ ⊢ 2 : int    ∅ ⊢ false : int
        ──────────────────────────────────────────────(if)
              ∅ ⊢ if 1 then 2 else false : int
    ]}
    The expression [if 1 then 2 else false] is not well-typed in the empty context, but it can be given a maximal partial derivation, in which the second premise is an axiom which is derivable, where as the first and third premises are axioms which cannot be derived

    Give maximal partial derivations of the following typing judgments.

    {math
    \varnothing \vdash \texttt{if true then 1 else false} : \texttt{int}
    }

    {math
    \varnothing \vdash \texttt{if true then false else 1} : \texttt{bool}
    }

    {math
    \varnothing \vdash \texttt{if true then false else x || 2} : \texttt{bool}
    }

    {math
    \varnothing \vdash \texttt{if true then false else 2 || x} : \texttt{bool}
    }

    In addition, write down the type error that OCaml gives you when trying to type-check each expression (and think about what the type-error tells you about the order in which OCaml type-checks sub-expressions).
*)
