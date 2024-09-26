(** {1 Assignment 4}

    This assignment is due on {b Thursday 10/3 by 11:59PM}.
    It include both a programming part and a written part which are to be submitted on Gradescope under {i assign04} and {i assign04 (written)}, respectively.

    In order to run the given test suite you have to define every required function in the assignment.
    {b We would recommend creating dummy values for each problem when you start} if you want to be able to run tests as you work.
 *)

(** {2 Programming Part} *)

(** {3 Problem 1: Last function standing}

    Given

    - a {b starting value} {m s} of type ['a]
    - a {b failing condition} {m P} of type ['a -> bool]

    we say that a function {m f} of type ['a -> 'a] {b survives for {m k} steps with respect to {m s} and {m P}} if {m P(f^i(s))} is false for every integer {m i} satisfying {m 1 \leq i \leq k}.
    In logical notation:
    {math
    \neg P(f(s)) \land \neg P(f(f(s))) \land \dots \land \neg P(f^k(s))
    }
    We define {m \mathsf{lifespan}(f, s, P)} to be the greatest number of steps {m f} survives with respect to {m s} and {m P}.
    Note that this value may be {m \infty}.

    Given a set of functions {m F}, we define the {b last function standing with respect to {m s} and {m P}} to be
    {math
    \mathsf{argmax}_{f \in F} \mathsf{lifespan}(f, s, P)
    }
    For this problem, we will say that the {m \mathsf{argmax}} is undefined if there are multiple functions in {m F} with the same lifespan, or if {m F} is empty.
 *)

val last_function_standing : ('a -> 'a) list -> 'a -> ('a -> bool) -> ('a -> 'a) option
(** Implement the function [last_function_standing] so that

    {[last_function_standing funcs start pred funcs]}

    is the last function standing in [funcs] with respect to the starting point [start] and failing condition [pred].
    The value should be returned as an option, where the value is [None] when [funcs] is empty, or there are multiple functions in [funcs] with the same {b finite} lifespan.
    The behavior of the implementation is undefined if [funcs] has multiple functions with infinite lifespan.

    Put your solution in a file called [assign04/lib/assign04_01.ml].
    See the file [assign04/test/test_suite/test01.ml] for example output behavior.
 *)

(** {3 Problem 2: Your first type checker}

    We're at a point in the course when we can implement our first type checker (technically, we're going to implement a type inference algorithm, but for very simple languages we can implement type checking in terms of type inference).
    As we've said many times, when we describe a programming language, we need to specify three things: syntax, typing rules and semantics.
    In this problem, we'll consider a very simple expression language for Boolean values and integers.

    We're going to ignore syntax for now (this will come when we start talking about parsing) and instead start with an ADT for expressions in our language (like we did in lecture).
 *)

type expr =
  | True
  | False
  | Num of int
  | Or of expr * expr
  | Add of expr * expr
  | IfThenElse of expr * expr * expr

(** For example, we can represent the expression

    {[
    if true || false then 3 else 3 + 3
    ]}

    with the value

    {[
    let e =
      IfThenElse
        ( Or (True, False)
        , Num 3
        , Add (Num 3, Num 3)
        )
    ]}

    (and the process of parsing is exactly the process of getting from the above string to the given value)

    We then define our type rules, when means first defining what a type is.
    For this simple language, every well-typed expression is either an integer or a Boolean value.
    And for the rules themselves, note that, since there are no variables in this language, there is no need for a context.
 *)

type ty =
  | Int
  | Bool

(**
    {math
    \frac
    {}{\texttt{true} : \texttt{bool}}
    \qquad
    \frac
    {}{\texttt{false} : \texttt{bool}}
    \qquad
    \frac
    {n \text{ is an integer}}
    {n : \texttt{int}}
    }

    {math \frac{}{}}

    {math
    \frac
    {e_1 : \texttt{int}
     \qquad
     e_2 : \texttt{int}
    }
    {e_1 \ \texttt{+} \ e_2 : \texttt{int}}
    \qquad
    \frac
    {e_1 : \texttt{bool}
     \qquad
     e_2 : \texttt{bool}
    }
    {e_1 \ \texttt{||} \ e_2 : \texttt{bool}}
    }

    {math \frac{}{}}

    {math
    \frac
    {e_1 : \texttt{bool}
     \qquad
     e_2 : \tau
     \qquad
     e_3 : \tau
    }
    {\texttt{if} \ e_1 \ \texttt{then} \ e_2 \ \texttt{else} \ e_3 : \tau}
    }
*)

val type_of : expr -> ty option
(** Implement a function [type_of] where [type_of e] is [Some t] if [t] is the type of [e] according to the above rules (i.e., there is a derivation of [e] : [t]) and [None] if [e] is not well-typed (i.e., there is no derivation of [e] : [t]).
    Try to think about how your implementation implicitly builds a typing derivation, if one exists.

    Put your solution in a file called [assign04/lib/assign04_02.ml].
    See the file [assign04/test/test02.ml] for example output behavior.
    {b Important:} You must include the definitions of [expr] and [ty] in your solution.
 *)

(** {3 Problem 3: Your first evaluator}

  We continue on with the language in the previous problem.
  The next thing we need is the semantic rules.

  {math
  \frac{}{\texttt{true} \Downarrow \texttt{true}}
  \qquad
  \frac{}{\texttt{false} \Downarrow \texttt{false}}
  \qquad
  \frac{n \text{ is a number}}{n \Downarrow n}
  }
  {math \frac{}{}}
  {math
  \frac
  {e_1 \Downarrow v_1
   \qquad
   e_2 \Downarrow v_2
   \qquad
   v = v_1 + v_2
  }
  {e_1 \ \texttt{+} \ e_2 \Downarrow v}
  \qquad
  \frac
  {e_1 \Downarrow v_1
   \qquad
   e_2 \Downarrow v_2
   \qquad
   v = v_1 \lor v_2
  }
  {e_1 \ \texttt{||} \ e_2 \Downarrow v}
  }
  {math \frac{}{}}
  {math
  \frac
  {e_1 \Downarrow \texttt{true}
   \qquad
   e_2 \Downarrow v_2
  }
  {\texttt{if} \ e_1 \ \texttt{then} \ e_2 \ \texttt{else} \ e_3 \Downarrow v_2}
  \qquad
  \frac
  {e_1 \Downarrow \texttt{false}
   \qquad
   e_3 \Downarrow v_3
  }
  {\texttt{if} \ e_1 \ \texttt{then} \ e_2 \ \texttt{else} \ e_3 \Downarrow v_3}
  }
 *)

type value =
  | VNum of int
  | VBool of bool

val eval : expr -> value
(** Implement the function [eval] so that [eval e] is the value of [e] according to the above rules (i.e., it's possible to derive [e] {m \Downarrow} [eval e]).
    The behavior of the implementation is undefined if [e] is not well-typed (this is one of the benefits of typing).

    Put your solution in a file called [assign04/lib/assign04_03.ml].
    See the file [assign04/test/test03.ml] for example output behavior.
    {b Important:} You should include the line [open Assign04_02.ml] so you have access to the type definitions in that file. You must also include the definition of [value] in your solution.
 *)

(** {3 Problem 4: Your second type checker}

    Another thing that we've said is that the {i only} reason we maintain a context in our typing derivations is to type expressions with variables.
    Let's see how this works by re-doing problem 2, but with a language that has variables and let-bindings.

    The following ADT for expressions is the same as the one previously defined but with variables and let-bindings.
    The type [ident] is just a synonym for strings, which are used to represent variable names.
 *)

type ident = string

type expr' =
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

(** We keep the same notion of a type, but introduce a type synonym for contexts, which are association lists that map identifiers to types. *)

type ty' =
  | Int
  | Bool

type context = (ident * ty') list

(** We also rewrite the above typing rules to include contexts, and include rules for variables and let-bindings.

    {math
    \frac
    {}{\Gamma \vdash \texttt{true} : \texttt{bool}}
    \qquad
    \frac
    {}{\Gamma \vdash \texttt{false} : \texttt{bool}}
    \qquad
    \frac
    {n \text{ is an integer}}
    {\Gamma \vdash n : \texttt{int}}
    }

    {math \frac{}{}}

    {math
    \frac
    {\Gamma \vdash e_1 : \texttt{int}
     \qquad
     \Gamma \vdash e_2 : \texttt{int}
    }
    {\Gamma \vdash e_1 \ \texttt{+} \ e_2 : \texttt{int}}
    \qquad
    \frac
    {\Gamma \vdash e_1 : \texttt{bool}
     \qquad
     \Gamma \vdash e_2 : \texttt{bool}
    }
    {\Gamma \vdash e_1 \ \texttt{||} \ e_2 : \texttt{bool}}
    }

    {math \frac{}{}}

    {math
    \frac
    {\Gamma \vdash e_1 : \texttt{bool}
     \qquad
     \Gamma \vdash e_2 : \tau
     \qquad
     \Gamma \vdash e_3 : \tau
    }
    {\Gamma \vdash \texttt{if} \ e_1 \ \texttt{then} \ e_2 \ \texttt{else} \ e_3 : \tau}
    }

    {math \frac{}{}}

    {math
    \frac
    {x : \tau \text{ appears in }\Gamma}
    {\Gamma \vdash x : \tau}
    \qquad
    \frac
    {\Gamma \vdash e_1 : \tau_1
     \qquad
     \Gamma, x : \tau_1 \vdash e_2 : \tau_2
    }
    {\Gamma \vdash \texttt{let} \ x \ \texttt{=} \ e_1 \ \texttt{in} \ e_2 : \tau_2}
    }

*)

val type_of' : context -> expr' -> ty' option
(** Implement a function [type_of'] where [type_of' gamma e] is [Some t] if [e] has type [t] in the context [gamma] (i.e., [gamma] {m \vdash} [e] {m : } [t] is derivable) and [None] if [e] is not well-typed in the context [gamma].

    Put your solution in a file called [assign04/lib/assign04_04.ml].
    See the file [assign04/test/test04.ml] for example output behavior.
    {b Important:} You must include the definitions of [expr'] and [ty'] in your solution.
 *)

(** {2 Written Part} *)

(** {3 Problem 1: Typing Derivations}

    The following typing judgment is not derivable.

    {math
    \cdot \vdash \texttt{let a = let b = 3 in a + b in 2 * a} : \texttt{int}
    }

    That said, we can try to construct the "most complete" typing derivation to see where trying to type the given expression fails.

    Construct a typing derivation from bottom-up until you reach a judgment of the form {m \Gamma \vdash x : \tau} which {i does not} follow from the {m \mathsf{Var}} rule.
    You final solution should be a valid typing derivation {i except} that one of its axioms (i.e., judgments with no premises) is not valid.
    {b Please circle or otherwise mark this invalid judgment.}

    {3 Problem 2: Semantic Derivations}

    Write a derivation for the semantic judgment

    {math
    \left(\texttt{let a = let b = 3 in b + b in 2 * a}\right) \Downarrow \texttt{12}
    }
*)
