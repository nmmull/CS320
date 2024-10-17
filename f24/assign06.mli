(** {1 Assignment 6}

    This assignment is due on {b Thursday 10/24 by 11:59PM}.

    In order to run the given test suite you have to define every required function in the assignment.
    {b We would recommend creating dummy values for each problem when you start} if you want to be able to run tests as you work.

    The purpose of this assignment is to give you a taste of the mini-projects.
    It's also an opportunity to implement some simple parsing (something we won't do in the second half of the course).

    In this assignment, you will be building an interpreter for expressions in reverse Polish notation (RPN).
    In reverse Polish notation, all operators are {i postfix}, meaning they come after their arguments.
    For example, we will write [2 3 +] for the sum of [2] and [3].
    One benefit of reverse polish notation is that it doesn't require parentheses (make sure to convince yourself of this).
    Another is that it's easy to parse.

    Here is a simple example of an expression in this language.

    {[
    2 3 + 3 3 + < 0 1 1 + ?
    ]}

    This expression has type {m \texttt{int}} and evaluates to {m \texttt{0}}.

    In more familiar OCaml-like syntax, this would look like:

    {[
    if 2 + 3 < 3 + 3 then 0 else 1 + 1
    ]}

    In the mini-projects, you're going to be given three things about a language: the syntax, the typing rules, and the semantics.
    It will then be your task to implement an interpreter.

    Here's the syntax of our toy language.

    {math
    \begin{align*}
      \textcolor{blue}{\texttt{<expr>}} &::= \textcolor{blue}{\texttt{<int>}} \\
      &\hspace{3mm}| \hspace{2.5mm} \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{red}{\texttt{+}} \\
      &\hspace{3mm}| \hspace{2.5mm} \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{red}{\texttt{<}} \\
      &\hspace{3mm}| \hspace{2.5mm} \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{red}{\texttt{?}} \\
    \end{align*}
    }

    We'll talk more about this notation very soon, but hopefully the idea is clear.
    An expression is either number, or some expressions followed by an operator symbol.

    Next, the typing rules.
    Expressions can either be integers or Boolean values.
    These should feel familiar.

    {math

    \frac
    {n \text{ is an integer literal}}
    {n : \texttt{int}}
    \qquad
    \frac
    {e_1 : \texttt{int}
     \qquad
     e_2 : \texttt{int}
    }
    {e_1 \ e_2 \ \texttt{+} : \texttt{int}}
    \qquad
    \frac
    {e_1 : \texttt{int}
     \qquad
     e_2 : \texttt{int}
    }
    {e_1 \ e_2 \ \texttt{<} : \texttt{bool}}
    \qquad
    \frac
    {e_1 : \texttt{bool}
     \qquad
     e_2 : \tau
     \qquad
     e_3 : \tau
    }
    {e_1 \ e_2 \ e_3 \ \texttt{?} : \tau}
    }

    Finally, the semantics.
    These should also feel familiar.

    {math
    \frac
    {n \text{ is an integer literal}}
    {n \Downarrow n}
    \qquad
    \frac
    {e_1 \Downarrow v_1 \qquad e_2 \Downarrow v_2 \qquad v_1 + v_2 = v}
    {e_1 \ e_2 \ \texttt{+} \Downarrow v}
    \qquad
    \frac
    {e_1 \Downarrow v_1 \qquad e_2 \Downarrow v_2 \qquad v_1 < v_2}
    {e_1 \ e_2 \ \texttt{<} \Downarrow \texttt{true}}
    }

    {math
    \frac
    {e_1 \Downarrow v_1 \qquad e_2 \Downarrow v_2 \qquad v_1 \geq v_2}
    {e_1 \ e_2 \ \texttt{<} \Downarrow \texttt{false}}
    \qquad
    \frac
    {e_1 \Downarrow \texttt{true} \qquad e_2 \Downarrow v_2}
    {e_1 \ e_2 \ e_3 \ \texttt{?} \Downarrow v_2}
    \qquad
    \frac
    {e_1 \Downarrow \texttt{false} \qquad e_3 \Downarrow v_3}
    {e_1 \ e_2 \ e_3 \ \texttt{?} \Downarrow v_3}
    }
 *)

(** {2 Problem 1: Your First Lexer}

    In rough terms, lexing is the process of converting a string into a list of {i tokens}.
    Tokens are the {i abstract units} of a programming language.
    We convert our input program (a string) into a list of tokens so that we don't need to deal with low-level concerns like whitespace when parsing.
    For our simple language, we will assume that every symbol or number {b must be separated by at least one whitespace character.}
    Therefore, lexing is a matter of converted each whitespace-separated string into its corresponding abstract respresentation, e.g., ["+"] becomes [TAdd] and ["-123"] becomes [TNum (-123)].
 *)

type tok =
  | TNum of int
  | TAdd
  | TLt
  | TIte

val lex : string -> tok list option

(** Implement the function [lex] so that [lex s] is

    - [Some tks] where [tks] is the list of tokens representing [s], given that every whitespace-separated string in [s] represents a token;
    - [None] otherwise.

    You can do this any way you'd like, but it would be simplest to use the functions [split] and [tok_of_string_opt] in [utils.ml].

    - [split] separates a string into words separated by whitespace, dropping all the whitespace.
    - [tok_of_string_opt] converts a [string] into a token if it stand for a token in our language, e.g., ["?"] stand for if-then-else so [tok_of_string_opt "?"] is [Some TIte] whereas [tok_of_string_opt "asdf"] is [None]).

    Put your solution into a file called [lib/assign06_01.ml].
    See the file [test/test_suite/test01.ml] for example behavior.
    Remember that you need to include the line [open Utils] if you want to use the above mentioned functions.
*)

(** {2 Problem 2: Your First (RPN) Parser}

    Ultimately, we will use an ADT to represent our expressions, just as we did for our OCaml-like toy languages from previous assignments.
 *)

type expr =
  | Num of int
  | Add of expr * expr
  | Lt of expr * expr
  | Ite of expr * expr * expr

val parse : tok list -> expr option

(** Implement the function [parse] so that [parse toks] is

    - [Some e] if [toks] represented a well-formed program;
    - [None] otherwise.

    You can do this any way you'd like, but I'd recommend the following.
    It's natural to implement a parser for RPN expressions using a stack, i.e., processing tokens from left to right,

    - when you see a number push it to the stack;
    - when you see an operator, apply it to the elements at the top of the stack (in reverse order);

    For example:

    {[
    EXPRESSION        STACK
    2 3 + 5 <         []
    3 + 5 <           [Num 2]
    + 5 <             [Num 3; Num 2]
    5 <               [Add (Num 2, Num 3)]
    <                 [Num 5; Add (Num 2, Num 3)]
                      [Lt (Add (Num 2, Num 3), Num 5)]
    ]}

    {i Hint:} You can implement [parse] tail-recursively and maintain the stack as an accumulator.

    Put your solution into a file called [lib/assign06_02.ml].
    See the file [test/test_suite/test02.ml] for example behavior.
    Remember that you need to include the line [open Utils] to use the types defined there.
 *)

(** {2 Problem 3: A Type Checker} *)

type ty =
  | TInt
  | TBool

val type_of : expr -> ty option

(** Implement the function [type_of] so that [type_of e] is

    - [Some t] where [t] is the type of [e] if [e] is well-typed;
    - [None] otherwise.

    Put your solution into a file called [lib/assign06_03.ml].
    Remember that you need to include the line [open Utils] to use the types defined there.
    There are no provided tests this week, you will have to test this function yourself.

 *)

(** {2 Problem 4: An Evaluator} *)

type value =
  | VNum of int
  | VBool of bool

val eval : expr -> value

(** Implement the function [eval] so that [eval e] is [v] is the value of [e] given that [e] is well-typed.
    The behavior of the implementation is undefined if [e] is not well-typed (i.e., if [type_of e] is [None]).

    Put your solution into a file called [lib/assign06_04.ml].
    See the file [test/test_suite/test04.ml] for example behavior.
    Remember that you need to include the line [open Utils] to use the types defined there.
 *)

(** {1 The Executable}

    Once you've finished the above problems, you can run your interpreter on an {i actual program} (whoa).
    There are several example programs in the directory [examples].
    If you've done everything correctly, you should be able to run the command

    {[
    dune exec assign06 examples/file_name
    ]}

    (replace [file_name] with the name of one of the files in that directory) which will print out the value of the expression in the file.
    You can even write your own programs if you'd like.


    To be clear, there are no tasks for the assignment in this section, this is just for fun.
    That said, {b please look through the file [bin/main.ml].}
    This file chains together the lexer, parser, type checker, and evaluator.
    In the future, we will be asking you to write this part.

 *)
