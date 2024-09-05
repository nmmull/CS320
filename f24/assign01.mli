(** {1 Assignment 1}

    This assignment is due on {b Thursday 9/12 by 11:59PM}.  It's the
    first {i graded} assignment of the semester.

 *)

(** {2 Problem 1: Integer powers} *)

val pow : int -> int -> int
(** Perhaps somewhat surprisingly, there is no function for integer
    powers in the OCaml standard library (nor the course standard
    library.

    Implement the function [pow] which, given an integer {m n} and a
    {i nonnegative} integer {m k}, returns the integer {m n^k}.

    {[
    let _ =
      let _ = assert (pow 2 3 = 8) in
      let _ = assert (pow (-2) 3 = -8) in
      let _ = assert (pow 3 4 = 81) in
      ()
    ]}

    The behavior of the implementation is undefined if the given
    exponent is negative.

    Put your solution in a file called [assign01/lib/assign01_01.ml].  See
    the file [assign01/test/test_suite/test_01.ml] for example output
    behavior.

 *)

(** {2 Problem 2: Primes again} *)

val nth_prime : int -> int
(** In Assignment 0, you wrote a predicate [is_prime] for determining if an integer is prime.

    This time, implement the function [nth_prime] which, given a nonnegative integer {m n}, returns the {m n}th prime number (with repsect to 0-indexing).

    {[
    let _ =
      let _ = assert (nth_prime 0 = 2) in
      let _ = assert (nth_prime 1 = 3) in
      let _ = assert (nth_prime 2 = 5) in
      let _ = assert (nth_prime 998 = 7907) in
      let _ = assert (nth_prime 999 = 7919) in
      ()
    ]}

    The behavior of the implementation is undefined if the argument is negative.

    Put your solution in a file called [assign01/lib/assign01_02.ml].
    See the file [assign01/test/test_suite/test_02.ml] for example output behavior.

 *)

(** {2 Problem 3: Integers encoding sequences} *)

val nth : int -> int -> int
(** It's possible to encode sequences of {b positive} integers as integers themselves (this is a trick commonly used in CS theory when you want to work with sequences but only have numbers).

    We encode a sequence of positive integers {m (n_0, n_1, n_2, \dots, n_{k - 1})} by making them the exponents of the first {m k} prime numbers {m p_0 = 2}, {m p_1 = 3}, {m p_2 = 5}, and so on, and then multiplying them together:

    {math p_0^{n_0} p_1^{n_1} p_2^{n_2}\dots p_{k - 1}^{n_{k-1}}}

    So, for example, the sequence {m (5, 3, 2, 2)} is encoded as

    {math 2^5 3^3 5^2 7^2 = 1058400}

    Implement the function [nth] which, given an integers {m s} and {m i}, returns the {m i}th element of the sequence encoded by {m s}.

    {[
    let _ =
      let l = 1058400 in
      let _ = assert (nth l 0 = 5) in
      let _ = assert (nth l 1 = 3) in
      let _ = assert (nth l 2 = 2) in
      let _ = assert (nth l 3 = 2) in
      ()
    ]}

    The behavor of the implementation is undefined if the first argument is not a valid encoding of a sequence, or if the second argument is out-of-bounds.

    Put your solution in a file called [assign01/lib/assign01_03.ml].
    See the file [assign01/test/test_suite/test_03.ml] for example output behavior.

    {b IMPORTANT.} If you want access to the function you wrote in the previous problem, you need to put [open Assign00_02] at the top of the file, or refer to the function as [Assign00_02.nth_prime].
    This is a feature of the module system in OCaml, which we will cover more formally in a couple weeks.

 *)

(** {2 Problem 4: Sequences to strings} *)

val to_string : int -> string
(** Implement the function [to_string] which, given an integer which is a valid encoding of a sequence (according to the previous problem), returns a string representation of the sequence in the style of an OCaml list, i.e.,

    - the elements of the sequence are surrounded by square brackets
    - the elements are interposed with semicolons and a single whitespace character (there is no other whitespace)

    {[
    let _ =
      let _ = assert (to_string 1 = "[]") in
      let _ = assert (to_string 8 = "[3]") in
      let _ = assert (to_string 24 = "[3; 1]") in
      let _ = assert (to_string 1058400 = "[5; 3; 2; 2]") in
      ()
    ]}

    The behavior of the implementation is undefined if the argument is not a valid encoding of a sequence.

    Put your solution in a file called [assign01/lib/assign01_04.ml].
    See the note in the previous problem about how to use the function [nth] in this file.
    See the file [assign01/test/test_suite/test_04.ml] for example output behavior.

 *)
