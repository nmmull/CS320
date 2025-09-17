(** This assignment is due on {b Thursday 9/18 by 8:00PM}.  You should
    put all of your programming solutions in a file called
    [assign2/lib/assign2.ml].  See the file [test/test_assign2.ml] for
    example behavior of each function. You should put all your written
    solutions in a single pdf file.

 *)

(** {1 Programming (40%)} *)

(** {2 Practice Problems (Ungraded)}

    These problems come from a list of
    {{:https://ocaml.org/exercises}Exercises} on OCaml's webpage.  We
    won't grade these (the solutions are given with the problem
    statements) but we'll typically include a couple with each
    assignment for additional practice.

    - {{:https://ocaml.org/exercises#6}Palindrome}
    - {{:https://ocaml.org/exercises#10}Run-Length Encoding}
    - {{:https://ocaml.org/exercises#15}Replicate the Elements of a List a Given Number of Times}

 *)

(** {2 Drop Leading/Trailing} *)

val drop_leading : int -> int list -> int list
(**
   Implement the function [drop_leading] so that [drop_leading k l] is the result of removing all instances of [k] from the beginning of the list [l].
*)

val drop_trailing : int -> int list -> int list
(**
   Also implement the function [drop_trailing] so that [drop_trailing k l] is the result of removing all instances of [k] from the {i end} of the list [l].
   {i Hint.} Don't re-implement the same logic as above, use [drop_leading] and another common list function.
*)

(** {2 Split on Character} *)

val split_on_char : char -> string -> string list
(**
   Implement the function [split_on_char] so that [split_on_char sep s] is the list of all (possibly empty) substrings of [s] that are delimited by the character [sep].
   If [s] is empty, the result is the singleton list [[""]] (this description is taken from the OCaml standard library documentation).

   {b Note.} Working with strings in OCaml is different from working with lists (in a sense they might seem more familiar if you're coming from a language with arrays).  We cannot destruct strings. Rather we use the functions [String.get] and [String.sub] to access parts of the string.
   See the {{:https://nmmull.github.io/stdlib320/stdlib320/Stdlib320/String/index.html}Standard library documentation on strings} for more details.

*)

(** {2 FRACTRAN} *)

(**
   In this problem we're going to implement an interpreter for FRACTRAN.
   Interpreting a FRACTRAN program means working with very large numbers, and integers in OCaml are not arbitrary-precision.  Therefore, we begin by installing a library for arbitrary-precision integer arithmetic. In a terminal, run the following:
   {[
   eval $(opam env)
   opam install zarith
   ]}
   This library will give us access to two new types: [Z.t] for arbitrary-precision integers and [Q.t] for arbitrary-precision rationals.
   {b Remember:} There is no operator overloading in OCaml.
   We cannot use the [+] or [*] operator on values of these types.
   We will be clear in each subproblem below which functions you'll want to use in your implementation.
*)

val parse_fractran : string -> Q.t list
(**
   Implement the function [parse_fractran] so that [parse_fractran s] is a FRACTRAN program (i.e., a list of fractions) according to the following rules:

   - The input string must be a sequence of fractions in a single line separated by exactly one space.
   - A fraction must consist of two integers separated by a single ['/'] character and no spaces.

   See the example FRACTRAN programs in the given implementations of [max_fractran] and [fib_fractran] below.
   For this problem, we will assume the input is well-formed.
   In particular, we will consider the behavior of the function undefined otherwise.
   You'll likely want to use the following functions in your implementation.

   - [Z.of_string s] is the arbitrary-precision integer represented by the string [s].
   - [Q.make num den] is the arbitrary precision fraction with numerator [num] and denominator [den].
   - [List.nth l i] is the [i]th element of [l] assuming [0 <= i] and [i < List.length l].
   - [split_on_char] from the previous problem will also naturally be useful.
*)

val eval_fractran : Q.t list -> Z.t -> Z.t
(**
   Implement the function [eval_fractran] so that [eval_fractran p i] evaluates the FRACTRAN program [p] on the given input [i].
   See the previous assignment for what this means, along with the written problem below.
   You'll likely want to use the following functions in your implementation.

   - [Q.mul a b] is the the result of multiplying [a] and [b].
   - [Q.num a] is the numerator of [a] and is an aribtrary-precision integer (i.e., of type [Z.t])
   - [Q.den a] is the denominator of [a] and is an arbitrary-precision integer.
   - [Z.one] is the number one as an arbitrary-precision integer.

   {i Important:} Arbitrary-precision rationals are always given in reduced form.
   This makes it easy to determine if a rational can be converted into an integer.
*)

(** The remaining functions are implemented for you. *)

val interp_fractran : string -> Z.t -> Z.t
(**
   [interp_fractran] is [parse_fractran] followed by [eval_fractran]. *)

val max_fractran : int -> int -> int
(**
   [max_fractran i j] is the same as [max i j] but implemented in a very round-about way, i.e., by writing the program in FRACTRAN, running our interpreter and extracting the output.
*)

val fib_fractran : int -> int
(**
   [fib_fractran n] is the [n]th value of the Fibonacci sequence, implemented using our FRACTRAN interpreter.
*)

(** {1 Written (60%)} *)

(** {2 Grammar Rule to English} *)

(**
   {math
   \def\nt#1{\textcolor{blue}{\texttt{<}\texttt{#1}\texttt{>}}}
   \def\tm#1{\textcolor{red}{\texttt{#1}}}
   \nt{expr} \Coloneqq \nt{expr}\tm{.[} \nt{expr} \tm{]}
   }

   Write down an English sentence which expresses the same thing as the above grammar rule (this is actual OCaml syntax).
*)

(** {2 Typing Judgment to English} *)

(**
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \{ \code{s} : \code{string}, \code{i} : \code{int} \} \vdash
   \code{s.[i]} : \code{char}
   }

   Write down an English sentence which expresses the same thing as the above typing judgment.
*)

(** {2 Typing Rule to English} *)

(**
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \frac
   {\Gamma \vdash e_1 : \code{string} \qquad \Gamma \vdash e_2 : \code{int}}
   {\Gamma \vdash e_1\code{.[}e_2\code{]} : \code{char}}
   \ (\text{string-index})
   }

   Write down an English sentence which expresses the same thing as the above typing rule.
*)

(** {2 Typing Judgments in OCaml} *)

(**

   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \Gamma \vdash \code{let rec f x = f (x + g (f y)) in h (g (x = y)) = f x} : \tau
   }

   Determine the type {m \tau} and the {i smallest} context {m \Gamma} such that the above typing judgment holds in OCaml. ({i Hint.} Come up with dummy values for the unknown variables in the above expression so that you can type it into UTop and it will type-check.)
*)


(** {2 FRACTRAN} *)


(**

   (Another problem about FRACTRAN? Probably the last one...) In this problem, we will look at
   the semantics rules for FRACTRAN.

   The semantic rules for FRACTRAN are given in terms of {i
   configurations}.
   A configuation consists of two FRACTRAN
   programs {m P} and {m Q}, and an integer value {m n}, written {m \langle \ P \ , \ Q \ , \ n \ \rangle}.
   We think of {m P} as the input program, {m Q} as the part of the program we haven't yet processed, and {m n} as the input that is updated throughout the evaluation process.
   An {i evaluation judgment} is of the form

   {math
   \langle \ P \ , \ Q \ , \ n \ \rangle
   \longrightarrow
   \langle \ P' \ , \ Q' \ , \ n' \ \rangle
   }

   and it means that the configuration the the left-hand-side {i
   evaluates to} the configuration on the right-hand-side in one step.
   There are two rules for evaluating a FRACTRAN program.

   {math
   \frac
   {\textcolor{green}{\frac{pn}{q} \text{ is an integer}}}
   {\left\langle \ P \ , \ \frac{p}{q} :: Q \ , \ n \ \right\rangle
   \longrightarrow \left\langle \ P \ , \ P \ , \ \frac{pn}{q} \ \right\rangle}
   \ (\text{int})
   \qquad
   \qquad
   \frac
   {\textcolor{green}{\frac{pn}{q} \text{ is not an integer}}}
   {\qquad\qquad???\qquad\qquad}
   \ (\text{not-int})
   }

   Recall that the green parts of the rules are called {i side-conditions} because they are not evaluation judgments, they are just conditions that need to hold in order for the rule to be applied.
   The rule (int) says that if the next fraction {m p / q} in {m Q} times our value {m n} is an integer, then we reset {m Q} to {m P} and set {m n} to {m pn / q}.
   The rule (not-int) is missing its conclusion, but should express that if the next fraction {m p / q} in {m Q} times {m n} is {i not} an integer, then we should remove it from {m Q} and keep {m n} as it is.

   {b The Task.} Write {b (not-int)} as a formal inference rule.

*)
