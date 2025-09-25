(** This assignment is due on {b Thursday 10/2 by 8:00PM}.
    You should put all of your programming solutions in a file called [assign4/lib/assign4.ml].
    See the file [test/test_assign4.ml] for example behavior of each function.
    You should put all your written solutions in a single pdf file.
 *)

(** {1 Programming (100%)} *)

(** {2 Parsing S-expressions} *)

type sexpr =
  | Atom of string
  | List of sexpr list

(** An {b S-expression} is one of two things:

    - an {b atom}, which we will take to be any nonempty sequence of non-whitespace-non-parentheses characters;
    - an expression of the form {m \texttt( e_1 \ \dots \ e_k \texttt)}, where {m e_1} through {m e_k} are S-expressions.

    Recursive algebraic data types are perfect for representing S-expressions.  [(+ (/ 1 2) (- 3 4))] is a simple S-expression which would be represented as the following value of the above ADT.
    {[
      List
        [
          Atom "+";
          List [Atom "/"; Atom "1"; Atom "2"];
          List [Atom "-"; Atom "3"; Atom "4"];
        ]
    ]}

    S-expressions are used for the surface syntax of several programming languages, primarily LISP dialects like Racket.
    This is how we will be using them in this assignment.
    They also offer a simple alternative to human-readable data-interchange formats like XML and JSON.
    [dune] files, for example, are S-expression; take a look at [assign4/lib/dune].

    The key feature of S-expressions is that they are {i hierarchical}, whereas text on it's own is flat.
    Parsing an S-expression means converting a string representing a S-expression into an [sexpr] as defined above.
    This is easier to do if we ignore low level concerns like whitespace; this is the purpose of {b lexical analysis} or {b tokenizing}.
*)

type token =
  | Lparen
  | Rparen
  | Atom of string

(**
    We've implemented lexing for you in the starter code under the name [tokens_of_string].
    The example above as tokens becomes
    {[
      [
        Lparen;
          Atom "+";
          Lparen; Atom "/"; Atom "1"; Atom "2"; Rparen;
          Lparen; Atom "-"; Atom "3"; Atom "4"; Rparen;
        Rparen;
      ]
    ]}
*)

val sexpr_of_tokens : token list -> (sexpr * token list) option
(**
   Implement the function [sexpr_of_tokens] so that [sexpr_of_tokens toks] is

    - [Some (e, rest)] if [toks] has a prefix [pre] that represents the S-expression [e] and [pre @ rest = toks]
    - [None] otherwise.

    This function should depend on [sexprs_of_tokens].
*)

val sexprs_of_tokens : token list -> sexpr list * token list
(**
   Implement the function [sexprs_of_tokens] so that [sexprs_of_tokens toks] is [(es, rest)] where [toks] as a {i maximal} prefix [pre] that represents {i a list} of S-expressions, and [pre @ rest = toks].
   Note there is no need for [option] here; if there is no S-expression at the beginning of [toks] then the output should be [([], toks)].

   This functions should depend on [sexpr_of_tokens] (yes, they're mutually recursive).

   {b Hint:} The challenge with mutual recursion is convincing yourself that it works.
   I always recommend: don't think about it too hard.
   {i Assume} that both functions work as they are supposed to, and then implement them under this assumption.
*)

val parse_sexpr : string -> sexpr option
(**
   Implement the function [parse_sexpr] so that [parse_sexpr s] is [Some e] if [s] represents the S-expression [e], and [None] otherwise.
   {b Hint:} This should be one match using previously defined functions.
*)

(** {2 Parsing Arithmetic Expression} *)

(**
   We can also use ADTs to represent arithmetic expressions.
*)

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

(**
   The example above, which can be written in more familiar infix notation as [(1 / 2) + (3 * 4)], is represented by the following value of the above ADT.
   {[
     Add
       ( Div (Int 1, Int 2)
       , Mul (Int 3, Int 4)
       )
   ]}
   Like with parsing S-expressions, the hard part of parsing arithemtic expressions is getting the {i heirarchical} structure of the expression from the flat string representation.
   But once we have a parse for S-expressions, we have a parser for any programming language (even one for humble arithmetic expressions) which uses S-expressions as a surface language.
   All we have to do is convert between the two representations.
*)

val expr_of_sexpr : sexpr -> expr option
(**
   Implement the function [expr_of_sexpr] so that [expr_of_sexpr se] is [Some e] if [se] represents the arithmetic expression [e], and [None] otherwise.
*)

val parse : string -> expr option
(**
   Implement the function [parse] so that [parse s] is [Some e] is [s] represents the arithmetic expression [e], and [None] otherwise.
   {b Hint:} This should just be the composition of parsing an S-expression and converting it into an arithemtic expression, with some [option] pattern matching in between.
*)

(** {2 Evaluating Arithmetic Expressions} *)

(** Once we have an ADT for arithmetic expressions, we can evaluate them.
    The evaluation rules for arithemtic expressions are as follows.

    {math
    \frac
    {\texttt{n} \text{ is an integer literal for } n}
    {\texttt{n} \Downarrow n}
    \text{(intEval)}
    \qquad
    \frac
    {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad v = v_1 + v_2}
    {\texttt{( + } e_1 \ e_2 \texttt{ )} \Downarrow v}
    \text{(addEval)}
    \qquad
    \frac
    {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad v = v_1 - v_2}
    {\texttt{( - } e_1 \ e_2 \texttt{ )} \Downarrow v}
    \text{(subEval)}
    }
    {math
    \frac
    {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad v = v_1 v_2}
    {\texttt{( * } e_1 \ e_2 \texttt{ )} \Downarrow v}
    \text{(mulEval)}
    \qquad
    \frac
    {e_1 \Downarrow v_1 \quad e_2 \Downarrow v_2 \quad v_2 \neq 0 \quad v = v_1 / v_2}
    {\texttt{( / } e_1 \ e_2 \texttt{ )} \Downarrow v}
    \text{(divEval)}
    }
    where "{m /}" is integer division.
*)

val eval : expr -> int
(** Implement the function [eval] so that [eval e] is the result of evaluating [e] according to the above rules.  That is, if {m e \Downarrow v}, then [eval] {m e} should be {m v}.  Your function should raise a [Division_by_zero] exception in the case that there is a division by zero in [e] (you don't need to do anything for this, integer division does this already).

    This is not a complicated function.
    The point is to recognize that there is a tight correspondence between the evaluation rules and implementation of an evaluator.
*)

val interp : string -> int option
(**
   Implement the function [interp] so that [interp s] is [Some (eval e)] if [s] represents an arithmetic expression [e], and [None] otherwise.
*)

(*
(** {1 Written (50%)} *)

(** {2 If-Without-Else-Expressions} *)

(**
   Many programming languages support if-statements without else-branches.
   This isn't exactly possible in OCaml because every expression must have a value, including if-expression.
   Without an else-branch, it's not clear what the value of an if-expression would be.

   One possibility is to let the value of an if-without-else-expressions be an [option], and let the implicit else-branch have the value [None].
   Suppose we wanted to introduce this language construct to OCaml, with the syntax:

   {[
   <expr> ::= if <expr> then <expr>
   ]}

   so that the typing and semantic behavior of

   {math
   \texttt{if } e_1 \texttt{ then } e_2
   }

   is exactly the same as the typing and semantic behavior of

   {math
   \texttt{if } e_1 \texttt{ then Some } e_2 \texttt{ else None}
   }

   Write the typing and semantic rules for if-then-else expressions, and then give a derivation of the following judgments:

   {math
   \cdot \vdash \texttt{match if true then 1 with | None -> 2 | Some x -> x + 1} : \texttt{int}
   }

   {math
   \texttt{match if true then 1 with | None -> 2 | Some x -> x + 1} \Downarrow 2
   }

*)
   *)
