(** This assignment is due on {b Thursday 3/20 by 8:00PM}.  You should
    put all of your solutions in [assign6/lib/assign6.ml].  See the
    file [test/test_assign6.ml] for example behavior of each function.

 *)

(** {1 Programming} *)

(** The purpose of this assignment is to give you a taste of the
    mini-projects by building an S-expressions calculator. Here is a
    simple example of an expression in this language.

    {[
    (?
      (< (+ 2 3) (+ 3 3))
      0
      (+ 1 1))
    ]}

    This expression has type {m \texttt{int}} and evaluates to {m \texttt{0}}.

    In more familiar OCaml-like syntax, this would look like:

    {[
    if 2 + 3 < 3 + 3 then 0 else 1 + 1
    ]}

    In the mini-projects, you'll be given three things: syntax, typing
    rules, and semantics.  It will then be your task to implement an
    interpreter.  {i Note that several of the parts below require little to no
    new code.}

    Here's the syntax of our toy language.

    {math
    \begin{align*}
      \textcolor{blue}{\texttt{<expr>}} &::= \textcolor{blue}{\texttt{<int>}} \\
      &\hspace{3mm}| \hspace{2.5mm} \textcolor{red}{\texttt{(}} \ \textcolor{red}{\texttt{+}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{red}{\texttt{)}} \\
      &\hspace{3mm}| \hspace{2.5mm} \textcolor{red}{\texttt{(}} \ \textcolor{red}{\texttt{<}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{red}{\texttt{)}} \\
      &\hspace{3mm}| \hspace{2.5mm} \textcolor{red}{\texttt{(}} \ \textcolor{red}{\texttt{?}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{blue}{\texttt{<expr>}} \ \textcolor{red}{\texttt{)}} \\
    \end{align*}
    }

    Next, the typing rules.  Expressions can either be integers or
    Boolean values.  These should feel familiar.  We'll elide names
    because we won't be writing derivations for this system.  Note
    that there is no need for a context because there are no local
    variables!

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
    {\texttt{(} \ \texttt{+} \ e_1 \ e_2 \ \texttt{)} : \texttt{int}}
    \qquad
    \frac
    {e_1 : \texttt{int}
     \qquad
     e_2 : \texttt{int}
    }
    {\texttt{(} \ \texttt{<} \ e_1 \ e_2 \ \texttt{)} : \texttt{bool}}
    \qquad
    \frac
    {e_1 : \texttt{bool}
     \qquad
     e_2 : \tau
     \qquad
     e_3 : \tau
    }
    {\texttt{(} \ \texttt{?} \ e_1 \ e_2 \ e_3 \ \texttt{)} : \tau}
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
    {\texttt{(} \ \texttt{+} \ e_1 \ e_2 \ \texttt{)} \Downarrow v}
    \qquad
    \frac
    {e_1 \Downarrow v_1 \qquad e_2 \Downarrow v_2 \qquad v_1 < v_2}
    {\texttt{(} \ \texttt{<} \ e_1 \ e_2 \ \texttt{)} \Downarrow \top}
    }

    {math
    \frac
    {e_1 \Downarrow v_1 \qquad e_2 \Downarrow v_2 \qquad v_1 \geq v_2}
    {\texttt{(} \ \texttt{<} \ e_1 \ e_2 \ \texttt{)} \Downarrow \bot}
    \qquad
    \frac
    {e_1 \Downarrow \top \qquad e_2 \Downarrow v_2}
    {\texttt{(} \ \texttt{?} \ e_1 \ e_2 \ e_3 \ \texttt{)} \Downarrow v_2}
    \qquad
    \frac
    {e_1 \Downarrow \bot \qquad e_3 \Downarrow v_3}
    {\texttt{(} \ \texttt{?} \ e_1 \ e_2 \ e_3 \ \texttt{)} \Downarrow v_3}
    }
 *)

(** {2 Problem 1: Parsing}

    In rough terms, lexing (or tokenizing) is the process of
    converting a string into a list of {i tokens}.  Tokens are the {i
    abstract units} of a programming language.  We convert our input
    program (a string) into a list of tokens so that we don't need to
    deal with low-level concerns like whitespace when parsing.  You're
    given an implementation of a lexer.

 *)

type tok =
  | LParen
  | RParen
  | TNum of int
  | TAdd
  | TLt
  | TIf

val lex : string -> tok list option
(** [lex s] is [Some tks] where [tks] is the list of tokens
    representing [s] and [None] otherwise.  It's a variant of
    [tokenize] from Lab 4, which you can check for more details.

    We'll use an ADT to represent expressions in our language, similar
    to previous assignments.

 *)

type expr =
  | Num of int
  | Add of expr * expr
  | Lt of expr * expr
  | If of expr * expr * expr

val parse : string -> expr option
(** Implement the function [parse] so that [parse str] is

    - [Some e] if [str] represents a well-formed program;
    - [None] otherwise.

    You've already built a parser for S-expressions in Lab 4.
    Repurpose that code to work for this new [expr] ADT (as opposed to
    nonempty trees) and compose it with the given function [lex].

 *)

(** {2 Problem 2: Type-Checking} *)

type ty =
  | TInt
  | TBool

val type_of : expr -> ty option
(** Implement the function [type_of] so that [type_of e] is

    - [Some t] where [t] is the type of [e] if [e] is well-typed;
    - [None] otherwise.

    An expression {m e} is well-typed if if {m e : \tau} is derivable
    according to the above typing rules for some type {m \tau}.

 *)

(** {2 Problem 3: Evaluating} *)

type value =
  | VNum of int
  | VBool of bool

val eval : expr -> value
(** Implement the function [eval] so that [eval e] is [v] is the value
    of [e] given that [e] is well-typed.  The behavior of the
    implementation is undefined if [e] is not well-typed (i.e., if
    [type_of e] is [None]).
 *)

(** {1 The Executable}

    Once you've finished the above problems, you can run your
    interpreter on an actual program.  There are several example
    programs in the directory [examples].  If you've done everything
    correctly, you should be able to run the command

    {[
    dune exec assign6 examples/file_name
    ]}

    (replace [file_name] with the name of one of the files in that directory) which will print out the value of the expression in the file.
    You can even write your own programs if you'd like.


    To be clear, there are no tasks for the assignment in this section, this is just for fun.
    That said, {b please look through the file [bin/main.ml].}
    This file chains together the lexer, parser, type checker, and evaluator.
    In the future, we will be asking you to write this part.
 *)

(** {1 Written} *)

(** {2 Ambiguity} *)
(**

   {@text[
   <s> ::= A <s> B
         | <a>
   <a> ::= A <a>
         | B <b>
   <b> ::= B B <b>
         | B B
   ]}

   Determine a sentence {i with fewer than 8 symbols} which has two
   distinct leftmost derivations in the above grammar.  Also write
   down these two leftmost derivations.

 *)

(** {2 Designing Grammars} *)

(** Write down a grammar over the terminal symbols [A] and [B] such
    that every sentence recognized by the grammar is any number of
    [A]s, followed by a sequence of at least 2 [B]s.  For example
    [AAAABBB], [BBB], and [AABB] should be sentences recognized by the
    grammar, whereas [AAAB], [AB], and [A] should not. You may introduce
    any nonterminal symbols that you want.  *)
