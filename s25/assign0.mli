(** This assignment is due on {b Thursday 1/23 by 8:00PM} and is {b
    not} graded.  It's a practice run so you can verify that
    everything is set up correctly on your machine, and you can submit
    to Gradescope.

    You should put all of your solutions in the file
    [assign0/lib/assign0.ml].  See the file [test/test_assign0.ml] for
    example behavior of each function.

 *)

(** {1 Problem 1: Square Root} *)

val sqrt : int -> int
(** Implement the function [sqrt] so that [sqrt n] is the {e
    smallest} integer [k] where [n <= k * k]. The behavior of the
    function is undefined if [n] is negative. *)

(** {1 Problem 2: Primality} *)

val is_prime : int -> bool
(** Implement the function [is_prime] so that [is_prime n] is [true]
    if [n] is prime, and [false] otherwise.  Remember that all
    integers less than two are {e not} prime. *)
