(** This assignment is due on {b Thursday 1/30 by 8:00PM}.  You should
    put all of your solutions in a file called
    [assign1/lib/assign1.ml].  See the file [test/test_assign1.ml] for
    example behavior of each function.

 *)

(** {1 Programming} *)

(** {2 Practice Problems (Ungraded)}

    These problems come from a list of
    {{:https://ocaml.org/exercises}Exercises} on OCaml's webpage.  We
    won't grade these (the solutions are given with the problem
    statements) but we'll typically include a couple with each
    assignment for additional practice.

    - {{:https://ocaml.org/exercises#31}Determine Whether a Given Integer Number Is Prime} (compare with your [assign0] solution)
    - {{:https://ocaml.org/exercises#32}Determine the Greatest Common Divisor of Two Positive Integer Numbers}
    - {{:https://ocaml.org/exercises#33}Determine Whether Two Positive Integer Numbers Are Coprime}

 *)

(** {2 Perfect Numbers} *)

(** A positive integer is {e perfect} if it's equal to the sum of its
    proper divisors.  For example, the integer {m 6} is perfect since
    {m 6 = 1 + 2 + 3}.  Implement the function [is_perfect] so that
    [is_perfect n] is [true] if [n] is perfect and [false] otherwise.
    The behavior of the function is undefined if [n] is not positive.

 *)
val is_perfect : int -> bool

(** {2 Sum of Squares} *)

(** Implement the function [min_sos] so that [min_sos n] is the {e
    minimum} number of perfect squares required to sum up to [n].  For
    example, {m 8 = 2^2 + 2^2} and {m 8} is not a perfect square so
    [min_sos 8 = 2].  The behavior of the function is undefined if [n]
    is negative.

 *)
val min_sos : int -> int

(** {2 Number of Substring Occurrences} *)

(** Implement the function [num_occurs] so that [num_occurs ~sub:s t]
    is the number of occurrences of possibly overlapping instances of
    [s] in [t].  For example, the string ["AA"] appears in the string
    ["AAA"] twice as a substring so [num_occurs ~sub:"AA" "AAA" = 2].
    The behavior of the function is undefined if [s] is the empty
    string.  {i Hint.} You should use the function [String.sub].  See
    the
    {{:https://nmmull.github.io/CS320/landing/Spring-2025/Specifications/Stdlib320/String/index.html#val-sub}Stdlib320
    documentation} for more details.

    Note the use of
    {{:https://cs3110.github.io/textbook/chapters/basics/functions.html#labeled-and-optional-arguments}labeled
    arguments}.  Please reread this subsection of OCP.  This is not a
    core feature of OCaml but you should become comfortable with it.

 *)
val num_occurs : sub:string -> string -> int

(** {1 Written}

    {2 Typing Judgments to English}

    {math
    \def\code#1{\textcolor{purple}{\texttt{#1}}}
    \def\fun#1#2{#1 \ \code{->} \ #2}
    \def\comp{\ \code{>>} \ }
    \{ \
    \code{f} : \fun{\tau_1}{\tau_2}
    \ , \
    \code{g} : \fun{\tau_2}{\tau_3}
    \ \}
    \vdash
    \code{f} \comp \code{g} :
    \fun{\tau_1}{\tau_3}
    }

    Write down an English sentence which expresses the same thing as
    the above typing judgment. ({i Note:} This operators is not in the
    OCaml standard library)

    {2 Typing Rules to English}

    {math
    \def\code#1{\textcolor{purple}{\texttt{#1}}}
    \def\xor{\ \code{xor} \ }
    \frac
    {
    \Gamma \vdash e_1 : \code{bool}
    \qquad
    \Gamma \vdash e_2 : \code{bool}
    }
    {\Gamma \vdash e_1 \xor e_2 : \code{bool}
    }
    \
    (\mathsf{xor})
    }

    Write down an English sentence which expresses the same thing as
    the above typing rule. ({i Note:} This operators is not in the
    OCaml standard library)

    {2 Semantic Rules to English}

    {math
    \def\code#1{\textcolor{purple}{\texttt{#1}}}
    \def\xor{\ \code{xor} \ }
    \frac
    {
    e_1 \Downarrow \top
    \qquad
    e_2 \Downarrow v_2
    }
    {e_1 \xor e_2 \Downarrow \neg v_2}
    \
    (\mathsf{xorTrueEval})
    }

    Write down an English sentences which express the same things as
    the above semantic rules.  The symbol '{m \neg}' is Boolean
    negation (i.e., [not] in OCaml).

    {2 English to Semantic Rules}

    Write down the semantic rule which expresses the same thing as
    the following English sentence:

    {i If the expression {m e_1} evaluates to the Boolean value {m \bot}
    and the expression {m e_2} evaluates to the Boolean value {m v_2},
    then the expression {m e_1 \ \textcolor{purple}{\texttt{xor}} \ e_2} evaluates to {m v_2}.}

 *)
