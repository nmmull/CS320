(** This assignment is due on {b Thursday 10/9 by 8:00PM}.
    You should put all of your programming solutions in a file called [assign5/lib/assign5.ml].
    See the file [test/test_assign5.ml] for example behavior of each function.
    You should put all your written solutions in a single pdf file.
 *)

(** {1 Programming (50%)} *)

(** {2 Practice Problems (Ungraded)}

    These problems come from a list of
    {{:https://ocaml.org/exercises}Exercises} on OCaml's webpage.  We
    won't grade these (the solutions are given with the problem
    statements).

    - {{:https://ocaml.org/exercises#15}Replicate the Elements of a List a Given Number of Times}
    - {{:https://ocaml.org/exercises#16}Drop Every N'th Element From a List}
    - {{:https://ocaml.org/exercises#22}Create a List Containing All Integers Within a Given Range}

    Where possible, try to use higher-order functions, like [map] and [fold_left].

 *)

(** {2 Basic Higher Order Functions} *)

val curry3 : (('a * 'b * 'c) -> 'd) -> 'a -> 'b -> 'c -> 'd
(** Implement the function [curry3] which converts a "multi-argument"
    function (i.e., a function which takes a tuple as an argument) into
    nested single-argument functions.

 *)

val uncurry3: ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) -> 'd
(** Implement the function [uncurry3], which is the inverse of [curry3],
    i.e., it converts a Curried three-argument function into a function
    which take a tuple as an argument.
 *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** Implement the function [filter_map] so that [filter_map p l] is
    the result of applying [p] to every element of [l], filtering out
    the [None] elements, and keeping only the arguments to the [Some]
    elements.
 *)

(* val prefix_map : ('a list -> 'b option) -> 'a list -> ('b * 'a list) option *)
(* (\** Implement the function [prefix_map] so that [prefix_map f l] is *)

(*     - [Some (b, rest)] if [pre] is the shortest list such that [f pre] is not [None] and [f pre = Some b] and [pre @ rest = l]. *)
(*     - [None] if [f pre = None] for every prefix [pre] of [l]. *)
(* *\) *)

(** {2 Rose Trees} *)

type 'a rtree = Node of 'a * 'a rtree list

(**
   {b Rose trees} are represented by an ADT with a single constructor, a [Node] with a value and a list of children.
   Note that it is possible for a [Node] to have no children. An example:

   {[
     let example : int rtree =
       Node
         ( 1
         , [
             Node (2, []);
             Node (3, [Node 4 []; Node 5 []]);
             Node (6, []);
           ]
         )
   ]}
*)

val map_rtree : ('a -> 'b) -> 'a rtree -> 'b rtree
(** Implement [map_rtree] so that [map_rtree f t] is the result of applying [f] every value of [t], maintaining the structure of [t].
*)

val filter_rtree : ('a -> bool) -> 'a rtree -> 'a rtree option
(** Implement [filter_rtree] so that [filter_rtree f t] is the result to filtering out those value [v] of [t] for which [f v = false].
    In particular, for a subtree [Node (v, ts)], if [f v = false] then {i the whole subtree} should be removed.
*)

(** {2 Transpilation} *)

(**
   Suppose you want to implement a programming language with an fancy type system and some cool new compile-time-checked features, but which is semantically identical to Python.
   In this kind of scenario, instead of building an evaluator for your programming language, you can just {i translate} programs in your language into Python programs and then run those.

   This may seem a bit silly (implementing a programming language that's essentially a "wrapper" for another programming language) but it's not an uncommon practice.
   Typescript, for example, is essentially a superset of JavaScript which gets checked at compile time and then translated into JavaScript.
   Elm is another programming language that transpiles to JavaScript, but in a functional (i.e., better) way.
   There are a number of reasons to do this, but the main reason is that we might want to run the programs we write in a particular programming ecosystem, but also want the guarantees that come from the type checker (or whatever else) we implement.

   Consider the following collection of ADTs representing a toy imperative program.
*)

type op = Add | Sub | Mul | Div

type expr =
  | Int of int
  | Var of string
  | Call of string * expr list
  | Bop of op * expr * expr

type stmt =
  | FunDef of string * string list * stmt list
  | Assign of string * expr
  | Print of expr
  | Return of expr

type prog = stmt list



(** Here's an example of a program represented as a value of this ADT:

    {[
      let example : prog =
        [
          FunDef ("foo", ["x"; "y"],
                  [ Return (Bop (Add, Var "x", Var "y")) ]);
          FunDef ("bar", ["x"],
                  [ Assign ("y", Bop (Mul, Int 2, Var "x"));
                    Return (Bop (Mul, Var "y", Var "y"));
                  ]);
          Print (
            Bop
              (Sub
              , Call ("foo", [Int 2; Int 3])
              , Call ("bar", [Int 5])
              )
          )
        ]
    ]}

    This program might get translated to Python as:

    {@text[
    def foo(x, y):
        return (x + y)
    def bar(x):
        y = (2 * x)
        return (y * y)
    print((foo(2, 3) - bar(5)))
    ]}

    In this problem, you'll be implementing this translation.
*)

val string_of_expr : expr -> string
(** Implement the function [string_of_expr] so that [string_of_expr e] is a string represent of [e] as a Python expression.
    In order to make things simple, you must:

    - wrap every binary operator in parentheses;
    - use exactly 1 space around binary operators;
    - use exactly 1 space after commas;
    - otherwise, use no whitespace.
*)

val string_of_stmt : stmt -> string
(** Implement the function [string_of_stmt] so that [string_of_stmt s] is a string representation of [s] as a Python statement.
    In order to make things simpler, you must:

    - use exactly 1 space after the keyword [return];
    - use exactly 1 space after the keyword [def];
    - use exactly 1 space after commas;
    - use exactly 1 space around the assignment operator [=];
    - separate each function statement with a single line break;
    - use 4 spaces for indenting the body of a function;
    - otherwise use no whitespace (except for what is also required by expressions).
*)

val string_of_prog : prog -> string
(** Implement the function [string_of_prog] so that [string_of_prog p] is a string representation of [p] as Python program.
    In order to make things simpler, you must:

    - separate every statement with a single line break.
    - otherwise use no white space (except for what is also required by expressions and statements).
*)

(** A couple closing comments:

    - Make sure to take a look at the test cases. These are going to be the most useful for checking your solution.
    - The most difficult part will be dealing with indentation. Remember that functions can be nested!
    - Make sure to look at the [String] module in the standard library for potentially useful functions.
*)

(** {1 Written (50%)} *)
(** {2 Results}

    We saw the syntax, typing rules, and semantic rules for options and option matching during lab.
    {b The Task:} Write the syntax, typing rules, and semantic rules for results and result matching, in analogy with the rules for options.
    Your semantic rules should introduce new values of the form {m \mathsf{Ok}(v)} and {m \mathsf{Error}(v)} (where {m v} can be any value).

    Use these rules to give derivations for the following judgments.

    {math
    \{ \texttt{r} : \texttt{(int, bool) result} \} \vdash \texttt{match r with | Ok x -> Error (x + 1) | Error b -> Ok b} : \texttt{(bool, int) result}
    }

    {math
    \texttt{match Ok (1 + 1) with | Ok x -> Error (x + 1) | Error b -> Ok b} \Downarrow \mathsf{Error}(3)
    }
*)

(** {2 If-Without-Else-Expressions} *)

(**
   Many programming languages support if-statements without else-branches.
   This isn't exactly possible in OCaml because every expression must have a value, including if-expression.
   Without an else-branch, it's not clear what the value of an if-expression would be.

   One possibility is to let the value of an if-without-else-expression be an option, and let the implicit else-branch have the value [None].
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

   Write the typing and semantic rules for if-without-else-expressions, and then give derivations of the following judgments:

   {math
   \varnothing \vdash \texttt{match if true then 1 with | None -> 2 | Some x -> x + 1} : \texttt{int}
   }

   {math
   \texttt{match if true then 1 with | None -> 2 | Some x -> x + 1} \Downarrow 2
   }

*)
