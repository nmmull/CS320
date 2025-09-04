(** This assignment is due on {b Thursday 9/11 by 8:00PM}.  You should
    put all of your solutions in a file called
    [assign1/lib/assign1.ml].  See the file [test/test_assign1.ml] for
    example behavior of each function.

 *)

(** {1 Programming (70%)} *)

(** {2 Practice Problems (Ungraded)}

    These problems come from a list of
    {{:https://ocaml.org/exercises}Exercises} on OCaml's webpage.  We
    won't grade these (the solutions are given with the problem
    statements) but we'll typically include a couple with each
    assignment for additional practice.

    - {{:https://ocaml.org/exercises#31}Determine Whether a Given Integer Number Is Prime}
    - {{:https://ocaml.org/exercises#32}Determine the Greatest Common Divisor of Two Positive Integer Numbers}
    - {{:https://ocaml.org/exercises#33}Determine Whether Two Positive Integer Numbers Are Coprime}

 *)

(**
   {2 Number of factors}
*)

val num_factors : int -> int
(**
   Implement [num_factors] so that [num_factors n] is the
   number of prime factors of [n].  For example, [num_factors 18] is
   [3] because {m 18 = 2 * 3 * 3}.  The behavior of the function is
   undefined if [n] is not positive (i.e., there will be no test cases
   in which [n] is not positive).
*)

(**
   {2 Perfect Power}
*)

val perfect_power : int -> int -> bool
(**
   Implement [perfect_power] so that [perfect_power i n] is [true] if
   there is a integer {m k} such that {m k^i = n}, and [false]
   otherwise.  For example, [perfect_power 3 (-8)] is [true] since {m -8 = (-2)^3}.
*)

(** {2 Collatz} *)

val collatz : int -> int
(**
   The Collatz conjecture asks if the following procedure halt for all
   positive integers {m n}:

   - if {m n = 1} then {b HALT};
   - if {m n} is even, divide {m n} by {m 2};
   - if {m n} is odd, multiply {m n} by {m 3} and add {m 1}.

   Implement [collatz] so that [collatz n] is the number
   of steps it takes for the above procedure to halt.  For example
   [collatz 5] is [5] because the above procedure has the following behavior:

   {math
   5 \longrightarrow
   16 \longrightarrow
   8 \longrightarrow
   4 \longrightarrow
   2 \longrightarrow
   1
   }

   Note that, if the conjecture is false, your function is not
   guaranteed to halt!

 *)

(** {2 Total Stopping Time Records} *)

val tst_records : int -> int
(**
   In this problem, we define a different sequence based on the
   [collatz] function from the previous problem.  The number [collatz
   n] is sometimes called the {b total stopping time} of [n].
   We say that [n] is a {b total stopping time record} if its total
   stopping time is greater than that of any previous numbers. That is,
   [n] is a total stopping time record if [collatz m < collatz n] for
   all [m] satisfying [1 <= m && m < n].

   Implement [tst_records] so that [tst_records n] is the {m n}th
   largest number with total stopping time record (by 0-indexing).

*)

(* (\** {1 Path Records} *\) *)

(* val path_records : int -> int *)
(* (\** *)
(*    We consider one last variation on the [collatz] function.  The {b *)
(*    path value} of a number [n] is the largest number that appears *)
(*    through the [collatz] procedure, starting at [n]. For example, the *)
(*    path value of [5] is [16] (see above).  We say that [n] is a {b *)
(*    path record} if its path value is greater than that of any previous *)
(*    numbers (similar to the previous problem). *)

(*    Implement [path_records] so that [path_records n] is the {m n}th *)
(*    largest path record (by 0-indexing). *)
(* *\) *)

(** {1 Written (30%)} *)

(** {2 FRACTRAN} *)

(**
   In this course, we take up the programming language as an object of
   formal study.  We will focus on "good" programming languages.  But
   before we do this, here is a puzzle about a "bad" programming
   language. {{:https://en.wikipedia.org/wiki/FRACTRAN}FRACTRAN} is an
   esoteric (joke) programming language in which programs are
   sequences of fractions (i.e., this is the {i syntax} of the
   language).  A FRACTRAN program {m (a_1, \dots, a_k)} is run on a
   positive integer {m n} as follows (i.e., the {i semantics} of the
   language is given by the following procedure):

   {ol

   {- Start with a program counter {m i \gets 1};}

   {- Determine if {m a_in} is an integer. If it is, set {m n \gets a_in}
   and reset {m i \gets 1}. If it isn't, increment {m i}, i.e.,
   set {m i \gets i + 1}; }

   {- If {m i > k} then {b HALT} and output {m n};}

   {- Go back to step 2.}

   }

   We encode the input of a program in the exponents of the prime
   factorization of {m n}.  If we want to run a program on two
   positive integers {m j} and {m k}, we run it on {m n = 2^j3^k}.
   For example, the program

   {math
   \left( \frac{3}{2} \right)
   }

   computes addition; given an input of the form {m 2^{j}3^k},
   it halts with output {m 3^{j + k}} and the exponent has the sum of {m j} and {m k}.

   Determine the output of the following program on an input of the
   form {m 2^{j}3^{k}}. Describe in English what this program
   computes.

   {math
   \left(
   \frac{5}{6},
   \frac{5}{2},
   \frac{5}{3}
   \right)
   }


*)
