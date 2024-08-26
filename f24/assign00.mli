(** {1 Assignment 0}

    This assignment is due on {b Friday 9/6 by 11:59PM}.
    It's not graded, it's just a practice run so you can verify that everything is set up correctly on your computer.

 *)

(** {2 Problem 1: Square Root} *)

val sqrt : int -> int
(** Implement the function [sqrt] which, given a nonnegative integer {m n}, returns the {i smallest} value {m k} such that {m n \leq k^2 }.

    Put your solution into the file [assign00_01.ml] in the directory [assign00/lib].  See the file [test/test_suite/test_01.ml] for example output behavior.
 *)

(** {2 Problem 2: Primality} *)

val is_prime : int -> bool
(** Implement the function [is_prime] which, given an integer {m n}, returns whether or not {m n} is prime as a boolean value.  Recall that all integers less than {m 2} are {i not} prime.

    Put your solution into the file [assign00_02.ml] in the directory [assign00/lib].
    See the file [test/test_suite/test_02.ml] for example output behavior.
 *)
