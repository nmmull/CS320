(** {1 Assignment 7}

    This assignment is due on {b Thursday 10/31 by 11:59PM}.

    {2 Programming Part}

    There is a single task in the programming part of this assignment: generate a parser using Menhir for the following grammar.

    {@text[
    <prog>  ::= <expr>

    <expr>  ::= if <expr> then <expr> else <expr>
              | let <var> = <expr> in <expr>
              | fun <var> -> <expr>
              | <expr2>

    <expr2> ::= <expr2> <bop> <expr2>
              | <expr3> { <expr3> }

    <expr3> ::= () | true | false
              | <num> | <var>
              | ( <expr> )

    <bop>   ::= + | - | * | / | mod | < | <= | > | >= | = | <> | && | ||

    <num>   ::= handled by lexer

    <var>   ::= handled by lexer
    ]}

    A couple notes about the this grammar:
    - The last alternative for [<bop>] is [||], as in Boolean disjunction in OCaml.
    - The second alternative of [<expr2>] represents application, and the curly braces refer to repetition in EBNF syntax.
      Remember that application is {i left associative}, so given a sequence like [e1 e2 e3], this should parse to [(App (App(e1, e2), e3)].
      See the example from lecture for more details.

    In the following table the operators of the above language are given alongside their associativity, in order of increasing precedence.

    {t | Operator | Associativity |
       |----------|----------|
       | [||] | right   |
       | [&&] | right |
       | [<], [<=], [>], [>=], [=], [<>] | left |
       | [+], [-] | left |
       | [*], [/], [mod] | left |}

    Your parser should target the following ADT, which is given in [lib/utils/utils.ml].

    {[
    type bop =
      | Add | Sub | Mul | Div | Mod
      | Lt | Lte | Gt | Gte | Eq | Neq
      | And | Or

    type expr =
      | Num of int
      | Var of string
      | Unit | True | False
      | App of expr * expr
      | Bop of bop * expr * expr
      | If of expr * expr * expr
      | Let of string * expr * expr
      | Fun of string * expr

    type prog = expr
    ]}

    In this assignment, you are given some skeleton code (you won't be given any skeleton code in the mini-projects).
    Your task is to fill in [lex.mll] and [par.mly] so [My_parser.parse] is a parser for the above grammar.
    See [test/test_suite/test_parser.ml] for example behavior.
    {b Note.} The tests given there are far from exhaustive. You should do a bit of testing yourself.

 *)

 (** {2 Written Part}

     Consider the following toy grammar for match statements in OCaml.
     Note that we use ['|'] to distinguish between the alternative symbol and the syntax for matches.

     {@text[
     <expr>    ::= match <expr> with <matches>
                 | <var>
                 | <num>

     <match>   ::= '|' <pat> -> <expr>
     <matches> ::= <match> | <match> <matches>

     <pat>     ::= <var> | <num>
     <var>     ::= x
     <num>     ::= 0
     ]}

     {3 Problem 1: Derivations}

     Give a leftmost derivation of the sentence [match x with | 0 -> x | x -> 0] in the above grammar
     Remember that in a leftmost derivation, you must expand {i exactly one} nonterminal symbol in each line.

     {3 Problem 2: Ambiguity}

     Demonstrate that the above grammar is ambiguous by presenting a sentence which has two distinct parse trees according to the grammar.
     You do not need to present the parse trees, but you should explain why the sentence you give is ambiguous.
  *)


