(** The following is the standard library we'll use for the first half
    of {i CAS CS 320: Concepts of Programming Languages} at Boston
    University (Spring 2025).  It's a {i very} small subset of the
    OCaml Standard Library with a bit more documentation (it also
    gives us more control over what functions are "allowed" for
    assignments).  Nearly everything here is also available in the
    OCaml Standard Library, so check the
    {{:https://ocaml.org/manual/5.2/api/Stdlib.html}[Stdlib]
    documentation} for more information on what you see below.

 *)

(** {1 Integer arithmetic}

    Like most programming languages, OCaml has infix arithmetic
    operators. That said, the following operators can only be applied
    to integers.  Arithmetic operations for floating-point numbers are
    defined in the next section.  {b There are no arithmetic operators
    for any other type (strings, lists, etc.)}

    In OCaml, infix operators can be used as (Curry-ed) prefix
    operators by surrounding them in parentheses, e.g., [(+) 11 13]
    has the same value as [11 + 13]. Any function below whose name is
    surrounded in parentheses can be used as an infix binary operator.

 *)

val ( + ) : int -> int -> int
(** Integer addition *)

val ( - ) : int -> int -> int
(** Integer subtraction *)

val ( * ) : int -> int -> int
(** Integer multiplication *)

val ( / ) : int -> int -> int
(** Integer division in OCaml is implemented as Euclidean division,
    which rounds towards [0] (this differs from integer division in
    Python, which is implemented as floored division).

    {[
    let _ = assert (5 / 3 = 1)
    let _ = assert ((-5) / 3 = -1)
    let _ = assert (5 / (-3) = -1)
    ]}

    Raises a [Division_by_zero] exception if the second argument
    evaluates to [0].

 *)

val ( mod ) : int -> int -> int
(** Integer remainder is implemented so as to satisfy the following
    properties:

    - [x mod y = x mod (abs y)]
    - [(- x) mod y = - (x mod y)]

    For positive integers it's the remainder of [x / y] in the grade
    school sense:

    - [x / y * y + x mod y = x]

    Raises a [Division_by_zero] exception if the
    second argument evaluates to [0].

 *)

val abs : int -> int
(** Integer absolute value *)

(** {2 Precedence and Associativity}

    Once we have binary operators, we have to discuss precedence and
    associativity.  This will be a formal topic when we cover parsing.
    For now, we'll piggy-back on your intuitions about mathematical
    expressions from grade school.

    We can surround an expression with parentheses in order to use it
    as an operand of an infix binary operator, e.g., the expression
    [(1 + 2) * 3] has the subexpression [1 + 2] as its left operand.
    If we leave out the parentheses, the expression is ambiguous; it's
    unclear {i a priori} if [1 + 2 * 3] should have the same value as
    [(1 + 2) * 3] or [1 + (2 * 3)].  We assign operators {i
    precedence} and {i associativity} in order to resolve ambiguities.

    One way to eliminate the ambiguity in the example above it to rank
    operators by {i precedence}.  Operators are evaluated in order of
    decreasing precedence, i.e., operators with higher precedence are
    evaluated first.  It's also said that operators of higher
    precedence "bind tighter" than operators of lower precedence.
    More familiarly, you may have learned the acronym PEMDAS, which
    defines the order of operations (i.e., relative precedence of
    operators) for arithemtic expressions.  For the above example, the
    expression [1 + 2 * 3] should have the same value as the
    expression [1 + (2 * 3)].

    Given precedence, it's still possible for there to be ambiguity
    with operators of the same precedence, e.g., it's unclear {i a
    priori} if [1 - 2 - 3] should have the same value as [(1 - 2) - 3]
    or [1 - (2 - 3)].  We eliminate this ambiguity by assigning
    operators {i associativity}, either left, right, or no
    associativity.  For left associative operators, parentheses group
    to the left, i.e., operators furthest left are evaluated first.
    So [1 - 2 - 3] has the same value as [(1 - 2) - 3].  The analogous
    is true for the right associativity operators.

    We're eliding some details, but hopefully this should make
    sufficiently clear how to read expressions with binary operators
    in OCaml.  For full details on precedence and associativity of
    built-in operators in OCaml, see the
    {{:https://ocaml.org/manual/5.3/api/Ocaml_operators.html}OCaml
    manual page on operators}.

 *)

(** {1 Floating-point arithmetic}

    From the OCaml Standard Library: "OCaml's floating-point numbers
    follow the IEEE 754 standard, using double precision (64 bits)
    numbers. Floating-point operations never raise an exception on
    overflow, underflow, division by zero, etc. Instead, special IEEE
    numbers are returned as appropriate..." See
    {{:https://ocaml.org/manual/5.2/api/Stdlib.html#1_Floatingpointarithmetic}this
    paragraph} for more details.

    Note that the first four arithmetic operators have a '[.]'
    following them. This distinguishes them from the corresponding
    arithmetic operators for integers.  {b OCaml does not support
    operator overloading.}

 *)

val ( +. ) : float -> float -> float
(** Floating-point addition *)

val ( -. ) : float -> float -> float
(** Floating-point subtraction *)

val ( *. ) : float -> float -> float
(** Floating-point multiplication *)

val ( /. ) : float -> float -> float
(** Floating-point division *)

val ( ** ) : float -> float -> float
(** Exponentiation *)

val sqrt : float -> float
(** Square root *)

val exp : float -> float
(** Exponential *)

val log : float -> float
(** Natural logarithm *)

val ceil : float -> float
(** Round up to the nearest integer (returned as a [float]) *)

val floor : float -> float
(** Round down to the nearest an integer *)

val abs_float : float -> float
(** Floating-point absolute value *)

(** {1 Boolean operations} *)

val not : bool -> bool
(** Boolean negation *)

val ( && ) : bool -> bool -> bool
(** Boolean conjunction (and) *)

val ( || ) : bool -> bool -> bool
(** Boolean disjunction (or) *)

(** {1 Comparisons}

    The comparison operators in OCaml are {i polymorphic}.  This means
    any two expressions of the same type may be compared, with the
    exception of functions; comparison operators raise an
    [Invalid_argument] exception if two functions are
    compared. Comparison operators behave as expected on Boolean
    values, strings, lists, options, etc.

 *)

val ( = ) : 'a -> 'a -> bool
(** Structural equality. This is [==] in most other languages.  As an
    aside, you should generally avoid using this for floating-point
    numbers, and should instead compare floating point numbers up to a
    given tolerance.

 *)

val ( <> ) : 'a -> 'a -> bool
(** Structural inequality, i.e., [x <> y] is equivalent to [not (x = y)] *)

val ( < ) : 'a -> 'a -> bool
(** Structural less-than *)

val ( > ) : 'a -> 'a -> bool
(** Structural greater-than *)

val ( <= ) : 'a -> 'a -> bool
(** Structural less-than-or-equal-to, i.e., [x <= y] is equivalent to [not (x > y)] *)

val ( >= ) : 'a -> 'a -> bool
(** Structural greater-than-or-equal-to *)

val min : 'a -> 'a -> 'a
(** Minimum of its two arguments *)

val max : 'a -> 'a -> 'a
(** Maximum of its two arguments *)

(** {1 Conversions}

    Unlike many other programming languages, {b OCaml does not support
    implicit type conversion}; all conversions must be done explicitly.
    The OCaml standard library includes a small collection of useful
    conversions.

 *)

val float_of_int : int -> float
(** Converts an integer into a floating-point number *)

val int_of_float : float -> int
(** Converts a floating-point number into an integer by rounding
    towards [0] *)

val int_of_char : char -> int
(** Converts a character into its ASCII code *)

val char_of_int : int -> char
(** Converts an ASCII code into a character, raising an
    [Invalid_argument] exception if its argument is not a valid ASCII
    code *)

val string_of_bool : bool -> string
(** Converts a Boolean value to a string *)

val bool_of_string : string -> bool
(** Converts a string to a Boolean value, raising a [Failure]
    exception if the argument doesn't evaluate to ["true"]
    or ["false"] *)

val bool_of_string_opt : string -> bool option
(** Same as the previous function, but is [None] in the case of
    failure *)

val string_of_int : int -> string
(** Converts an integer to a string *)

val int_of_string : string -> int
(** Converts a string to an integer, raising a [Failure] exception if
    the argument doesn't represent a integer *)

val int_of_string_opt : string -> int option
(** Same as the previous function, but is [None] in the case of
    failure *)


val string_of_float : float -> string
(** Converts an floating-pofloat number to a string *)

val float_of_string : string -> float
(** Converts a string to an floating-pofloat number, raising a
    [Failure] exception if the argument doesn't represent a
    floating-pofloat number *)

val float_of_string_opt : string -> float option
(** Same as the previous function, but is [None] in the case of
    failure *)

(** {1 Input/Output} *)

(** {2 Printing}

    One complaint we frequently receive about OCaml is that it's
    difficult to do intermediate printing.  If you want to print
    something in the definition of a function, you just have to create
    a dummy variable and use a printing function.

    {[
    let add_one (x : int) : int =
      let _ = print (string_of_int x) in
      x + 1
    ]}

    There are better ways of doing this but I recommend this approach
    to beginners.

    There is no generic printing function (we don't want one).  That
    said, we've included additional printing functions in the
    individual modules for each types, e.g.,

    {[
    let add_one (x : int) : int =
      let _ = Int.print x in
      x + 1
    ]}

    We've included the following printing functions because they're
    included in the standard library.

 *)

val print_int : int -> unit
(** Prints an integer to standard output *)

val print_float : float -> unit
(** Prints a floating-point number to standard output *)

val print_string : string -> unit
(** prints a string to standard output *)

val print_endline : string -> unit
(** Prints a string followed by a newline character to standard output *)

val print : string -> unit
(** [print] is an alias for [print_endline] *)

(** {2 Reading}

    We'll primarily be using reading functions to read in files when
    we build interpreters.

 *)

val read_line : unit -> string
(** Reads a line (upto a newline character) from standard input, where
    the output does not include a the newline character *)

val read : unit -> string
(** Reads everything from standard input *)

(** {1 Basic OCaml Types} *)

(** {2 Functions} *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** [x |> f |> g] is equivalent to [g (f x)].  This operator is useful
    for
    {{:https://cs3110.github.io/textbook/chapters/basics/functions.html#pipeline}pipelining} *)

(** {2 Integers} *)

module Int : sig
  val sprint : int -> string
  val print : int -> unit
end
(** A module containing printers for integers *)

(** {2 Floats} *)

module Float : sig
  val pi : float
  val sprint : float -> string
  val print : float -> unit
end
(** A module containing printers for floats, and the value [pi] *)

(** {2 Booleans} *)

module Bool : sig
  val sprint : bool -> string
  val print : bool -> unit
end
(** A module containing printers for Boolean values *)

(** {2 Characters} *)

module Char : sig
  val sprint : char -> string
  val print : char -> unit
end
(** A module containing printers for characters *)

(** {2 Strings}

    Strings in OCaml are sequences of bytes. It's important to note
    that string are {i not} lists of characters, and it is generally
    good practice not to think of them as such.  We don't do much
    string manipulation in this course (but we will do some).  See the
    {{:https://ocaml.org/manual/5.3/api/String.html}OCaml docs on
    Strings} for more details.

 *)


val ( ^ ) : string -> string -> string
(** String concatenation

    {[
    let _ = assert ("hello" ^ "world" ^ "!" = "hello world!")
    ]}

 *)

(** A module containing basic string operations *)
module String : sig

  (** {1 Basic functions} *)

  val init : int -> (int -> char) -> string
  (** Creates a string based on an index function, i.e., [init n f] is
      a string of length [n] whose [i]th character is [f i]

      {[
      let _ = assert (init 3 (fun x -> char_of_int (x + 65)) = "ABC")
      let _ = assert (init 5 (fun _ -> 'z') = "zzzzz")
      ]}

   *)

  val length : string -> int
  (** [length s] is the number of characters in [s]

      {[
      let _ = assert (length "ABC" = 3)
      ]}

   *)

  val get : string -> int -> char
  (** Gets the character in a string at a given index, i.e., [get s i]
      is the [i]th character of [s].  It's equivalent to writing
      [s.[i]]

      Raises an [Invalid_argument] exception if [i < 0] and [i >= length s]

   *)

  val concat : string -> string list -> string
  (** Combines a list of string with a given delimiter

      {[
      let _ = assert (concat "," ["A";"B";"C"] = "A,B,C")
      let _ = assert (concat "" ["1";"2";"3"] = "123")
      ]}

   *)

  val trim : string -> string
  (** Removes whitespace from the beginning and end of its argument

      {[
      let _ = assert ("  xyz  \n" = "xyz")
      ]}

   *)

  val sub : string -> int -> int -> string
  (** Gets a substring, i.e., [sub s p l] is the substring of [s] of
      length [l] starting at position [p], where a position is a value
      between [0] and [length s] which marks the beginning of a
      substring

      {[
      let _ = assert (sub "ABC" 0 2= "AB")
      let _ = assert (sub "ABC" 3 0 = "")
      let _ = assert (sub "ABC" 1 1 = "B")
      ]}

      Raises an [Invalid_argument] exception if [p < 0] or
      [p + l > length s]
   *)

  (** {1 Higher-order functions}

      These functions work the same as they would on a [char list].

   *)

  val map : (char -> char) -> string -> string
  val fold_left : ('acc -> char -> 'acc) -> 'acc -> string -> 'acc
  val fold_right : (char -> 'acc -> 'acc) -> string -> 'acc -> 'acc
end

(** {2 Lists}

    Lists are ordered sequences of elements of the same type.  They
    are {i not} the same as arrays.  They are defined by the following ADT.

    {[
    type 'a t = 'a list =
    | []
    | (::) of 'a * 'a list

    let empty_list = []
    let example_cons = 1 :: 2 :: 3 :: []
    let example_lit = [1;2;3]
    let _ = assert (example_cons = example_lit)
    ]}

 *)


val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation

    {[
    let _ = assert ([[1;2;3] @ [4;5;6] @ [7;8;9] = [1;2;3;4;5;6;7;8;9]])
    ]}

 *)

(** A module containing basic list operations *)
module List : sig

  (** {1 Basic functions} *)

  val length : 'a list -> int
  (** [length l] is the number of elements in [l]

      {[
      let _ = assert (length [1;2;3] = 3)
      ]}

   *)

  val nth : 'a list -> int -> 'a
  (** [nth l i] is the [i]th element of [l] if [i >= 0] and [i < length l]

      {[
      let _ = assert (nth [1;2;3] 1 = 2)
      ]}

      Raises a [Failure] exception if [length l < i] and an
      [Invalid_argument] exception if [i < 0]

   *)

  val nth_opt : 'a list -> int -> 'a option
  (** Same as the previous function, but is [None] in the case of failure. *)

  val rev : 'a list -> 'a list
  (** Reverses a list

      {[
      let _ = assert ([rev [1;2;3] = [3;2;1]])
      ]}

   *)

  val concat : 'a list list -> 'a list
  (** Combines a list of lists from left to right

      {[
      let _ = assert ([concat [[1;2;3];[4;5;6];[7;8;9]] = [1;2;3;4;5;6;7;8;9]])
      ]}

   *)

  (** {1 Higher-order functions} *)

  val map : ('a -> 'b) -> 'a list -> 'b list
  (** [map f l] is [l] but with [f] applied to every element

      {[
      let _ = assert (map abs [-1;-2;-3] = [1;2;3])
      ]}

   *)

  val filter : ('a -> bool) -> 'a list -> 'a list
  (** [filter f l] is the elements of [l] (in order) which satisfy the
      predicate [f]

      {[
      let _ = assert ([filter ((<) 5) [3;4;5;6;7] = [6;7]])
      ]}

   *)

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
  (** Left-associative folding function for lists. [fold_left op init [x_1;x_2;...;x_n]] is equivalent to [op (...(op (op init x_1) x_2)...) x_n]
   *)

  val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
  (** Right-associative folding function for lists. [fold_right op [x_1;x_2;...;x_n]] is equivalent to [op x_1 (op x_2 (...(op x_n base)...))]
   *)

  (** {1 Finding} *)

  val mem : 'a -> 'a list -> bool
  (** Membership predicate for lists. [mem x l] is
      - [true] if [x] appears in [l]
      - [false] otherwise *)

  val find : ('a -> bool) -> 'a list -> 'a
  (** Finds based on a predicate.  [find f l] is the first appearance of an element of [l] which satisfies [f].

      Raises a [Not_found] exception otherwise.
   *)

  val find_opt : ('a -> bool) -> 'a list -> 'a option
  (** Same as the previous function, but is [None] in the case of failure. *)

  (** {1 Sorting} *)

  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  (** Generic sorting function for lists. [sort f l] has the same elements as [l], but sorted according to the comparing function [f] *)

  (** {1 Association lists} *)

  val assoc : 'a -> ('a * 'b) list -> 'b
  (** Membership function for association lists. [assoc x l] is equivalent to

      [snd (find (fun (k, v) -> k = x))] *)

  val assoc_opt : 'a -> ('a * 'b) list -> 'b option
  (** Same as the previous function, but is [None] in the case of failure *)

  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  (** [remove_assoc k l] is [l] but with the first appearance of a pair of the form [(k, v)] removed *)


  (** {1 Printers} *)

  val sprint : ('a -> string) -> 'a list -> string
  val print : ('a -> string) -> 'a list -> unit

end

(** {2 Options}

    An option is like a box that might contain a value.  It is given
    by the following ADT:

    {[
    type 'a option = None | Some of 'a
    ]}

    They are used for defining partial functions, i.e., functions
    which are not defined on all inputs.  For example, we can write
    the following function which returns the first element of a list,
    given the list is nonempty.

    {[
    let first (l : 'a list) : 'a option =
      match l with
      | [] -> None
      | x :: _ -> Some x
    ]}

    If [l] is empty, [first l] is [None].  This is {i not} the same
    thing as a null pointer.  [None] is a constructor for a datatype.
    Same with [Some]; in particular, [Some 2] is not the same as [2]
    (they're not even the same type).

 *)

(** A module containing basic operations for options. *)
module Option : sig
  val is_none : 'a option -> bool
  (** [is_none o] is
      - [true] if [o = Some x]
      - [false] if [o = None]
   *)

  val is_some : 'a option -> bool
  (** [is_some o] is equivalent to [not (is_none o)] *)

  val map : ('a -> 'b) -> 'a option -> 'b option
  (** Mapping function for options.  It applies a function to the
      value of the option if the option is not [None]. [map f o] is

      - [Some (f x)] if [o = Some x]
      - [None] if [o = None]

   *)

  val bind : 'a option -> ('a -> 'b option) -> 'b option
  (** Monadic bind for options.  It "does something" to the value of
      the option if it is not [None], and passes along the [None]
      otherwise.  [bind o f] is

      - [f x] if [o = Some x]
      - [None] if [o = None]

   *)

  (** {1 Printers} *)

  val sprint : ('a -> string) -> 'a option -> string
  val print : ('a -> string) -> 'a option -> unit
end

(** {2 Results} *)

(**
    A result is the same as an option except that the "none" case
    carried data. It's given by the following ADT.  As with options, they
    are used to define partial functions, but particularly in the case
    that we want more information in the error case.

 *)

type ('a, 'b) result = Ok of 'a | Error of 'b

(** A module containing basic result operations *)
module Result : sig
  val is_ok : ('a, 'e) result -> bool
  (** [is_ok r] is
      - [true] if [r = Ok x]
      - [false] if [r = Error e]
   *)

  val is_error : ('a, 'e) result -> bool
  (** [is_error r] is equivalent to [not (is_ok e)] *)

  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
  (** Mapping function for result values.  It applies a function to the
      value of the result if the result is not an error and passes along
      the error otherwise. [map f r] is

      - [Ok (f x)] if [r = Ok x]
      - [Error e] if [r = Error e]
   *)

  val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
  (** Mapping function for result errors, similar to the previous
      function but on errors, i.e. [map f r] is

      - [Ok x] if [r = Ok x]
      - [Error (f e)] if [r = Error e]

   *)

  val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  (** Monadic bind for results.  It "does something" to the value of a
      result if it is not an error, and passes along the error
      otherwise. [bind r f] is

      - [f x] if [r = Ok x]
      - [Error e] if [r = Error e]
   *)
end

(** {2 Nonempty Trees}

    Nonempty trees are not a part of the OCaml standard library, but
    they're useful to have.  In particular, we've included a printer
    for nonempty trees which will be useful for visualizing
    heirarchical data.  Trees are given by the following ADT.

    These are nonempty {m n}-ary trees.  Note that there is no "leaf"
    case; a leaf is a node with an empty list of children.

 *)

type 'a ntree =
  | Node of 'a * 'a ntree list

module Ntree : sig
  val sprint : ?unicode:bool -> ('a -> string) -> 'a ntree -> string
  val print : ?unicode:bool -> ('a -> string) -> 'a ntree -> unit
end
(** A module containing printers for trees *)

(** {1 Exception Handling}

    We won't do too much error handling in this course, but we will
    occasionally throw exceptions in the interpreters we write.  See
    the {{:https://ocaml.org/docs/error-handling}OCaml docs on error
    handling} and
    {{:https://cs3110.github.io/textbook/chapters/data/exceptions.html}OCP
    section on Exceptions} for more details.

 *)

val raise : exn -> 'a
(** Raise an exception *)

val failwith : string -> 'a
(** Raise a [Failure] exception with a given message *)

(** {1 Extra Utilities} *)

(** {2 Trigonometric Functions } *)


val cos : float -> float
val sin : float -> float
val tan : float -> float
val acos : float -> float
val asin : float -> float
val atan : float -> float
val atan2 : float -> float -> float
val hypot : float -> float -> float
val cosh : float -> float
val sinh : float -> float
val tanh : float -> float
val acosh : float -> float
val asinh : float -> float
val atanh : float -> float

(** {2 Randomness} *)

(** A module containing basic operations for randomness *)
module Random : sig
  val int : int -> int
  (** [int n] is a random integer chosen uniformly from the range {m [0, n)} *)

  val float : float -> float
  (** [float n] is a random float chosen from the range {m [0, n)} *)

  val bool : unit -> bool
  (** [bool ()] is a random Boolean value *)
end
