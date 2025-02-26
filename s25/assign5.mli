(** This assignment is due on {b Thursday 3/6 by 8:00PM}.  You should
    put all of your solutions in [assign5/lib/assign5.ml].  See the
    file [test/test_assign5.ml] for example behavior of each function.

 *)

(** {1 Programming} *)


(** {2 Practice Problems (Ungraded)}

    These problems come the textbook
    {{:https://cs3110.github.io/textbook/cover.html}OCP}.

    - {{:https://cs3110.github.io/textbook/chapters/data/exercises.html#:~:text=Exercise:%20powerset}powerset}
    - {{:https://cs3110.github.io/textbook/chapters/data/exercises.html#:~:text=Exercise:%20safe%20hd%20and%20tl}safe hd and tl}
    - {{:https://cs3110.github.io/textbook/chapters/data/exercises.html#:~:text=Exercise:%20list%20max%20exn%20string}list max exn string}
    - {{:https://cs3110.github.io/textbook/chapters/data/exercises.html#:~:text=Exercise:%20list%20max%20exn%20ounit}list max exn ounit}
    - {{:https://cs3110.github.io/textbook/chapters/hop/exercises.html#:~:text=Exercise:%20account%20balance}account balance}
    - {{:https://cs3110.github.io/textbook/chapters/hop/exercises.html#:~:text=Exercise:%20valid%20matrix}valid matrix}
    - {{:https://cs3110.github.io/textbook/chapters/modules/exercises.html#:~:text=Exercise:%20fraction}fraction}
 *)

(** {2 Evaluation with Errors} *)

(** In this problem we'll be looking at a common pattern for
    representing expressions with metadata.  We'll look at
    arithmetic expressions with addition, subtraction, multiplication,
    division, and exponentiation.

    Expressions are defined mutually recursively so that all
    expressions, including subexpressions, carry metadata of a given
    type parameter.

 *)

type op = Add | Sub | Mul | Div | Pow

type 'a expr =
  {
    expr : 'a _expr;
    meta : 'a
  }
and 'a _expr =
  | Num of int
  | Op of op * 'a expr * 'a expr

(** Note the very cool use of the [and] keyword, which allows use to
    define mutually recursive types (and functions). We'll be
    implementing an evaluator for expressions with metadata, and for
    which errors cases are handled using the [result] type.

    There are two kinds of errors that could occur when evaluating an
    arithmetic expression of this form:

    - The second argument of a division evaluates to zero, in which case evaluation should result in a [DivByZero] error.
    - The second argument of an exponentiation evaluates to a negative number, in which case evaluation should result in a [NegExp] error.

    Like expressions, errors carry metadata.  As an example, we can
    imagine expressions and errors carrying data describing the line
    number at which an expression/error occurs, so that we can present
    a better error message to the user.

 *)

type error_kind =
  | DivByZero
  | NegExp

type 'a error =
  {
    error: error_kind;
    meta : 'a;
  }

val eval : 'a expr -> (int, 'a error) result
(** Implement the function [eval] so that [eval e] is [Ok n] if [e]
    has no division-by-zero or negative-exponent errors and has the
    value [n].  The expression [eval e] should be [Error {error;meta}]
    otherwise, where [error] describes the kind of error in [e] and
    [meta] is the metadata of the {b leftmost division expression
    whose right-hand side evaluates to zero, or the leftmost
    exponentiation expression whose right-hand side evaluates to a
    negative number}.

    You can implement this function however you'd like but we
    recommend {e trying} to use the monadic [let*] notation, since the
    solution done in this way is quite clean.

    We've included the function [guard], which may be helpful.  It can
    be used as follows:

    {[
    let ( let* ) = Result.bind

    let guard b error = if b then Error error else Ok ()

    let check x_res error =
      let* x = x_res in
      let* _ = guard (x = 0) error in
      Ok x

    let _ = assert (check (Ok 2) () = Ok 2)
    let _ = assert (check (Error ()) () = Error ())
    let _ = assert (check (Ok 0) () = Error ())
    ]}

 *)

(** {2 Exception Handling} *)

exception ListTooShort
exception InvalidArg

val prefix : int -> 'a list -> 'a list
(** Implement the function [prefix] so that [prefix k l] is the list
    consisting of the first [k] elements of [l]. It should raise a
    [ListTooShort] exception if [k] is greater than the length of [l],
    and should raise an [InvalidArg] exception if [k] is negative.
 *)

type prefix_error =
  | ListTooShort
  | InvalidArg

val prefix_res : int -> 'a list -> ('a list, prefix_error) result
(** Implement the function [prefix_res] so that [prefix k l] is [Ok p]
    where [p] is the list consisting of the first [k] elements of [l],
    given [k] is at most the length of [l] and [k] is nonnegative.  It
    should return an error otherwise (I'll let you determine what kind
    of error). {i Hint:} Don't reimplement the function.
 *)

(** {2 Deques} *)

(** A {e double-ended queue}, or dequeue, is a list-like data
    structure for which is possible to pop from and push to both the
    front and back. For our purposes a dequeue implements the
    following signature.

 *)

module type DEQUEUE = sig
  type 'a t
  (** The underlying type representing a double-ended queue. *)

  val empty : 'a t
  (** An empty dequeue. *)

  val push_front : 'a -> 'a t -> 'a t
  (** [push_front x q] is the dequeue gotten by prepending [x] to the front of [q]. *)

  val pop_front : 'a t -> ('a * 'a t) option
  (** [pop_front q] is

      - [Some (x, r)] where [x] is the first element of [q] and [r] is the remaining element of [q];
      - [None] if [q] is empty.

   *)

  val push_back : 'a -> 'a t -> 'a t
  (** [push_back x q] is the dequeue gotten by appending [x] to the back of [q].

   *)

  val pop_back : 'a t -> ('a * 'a t) option
  (** [pop_back q] is

    - [Some (x, r)] where [x] is the last element of [q] and [r] is the remaining element of [q];
    - [None] if [q] is empty.

   *)

  val to_list : 'a t -> 'a list
  (** Converts a dequeue into a list with the same elements (in the same order). *)

end

module ListDequeue: DEQUEUE with type 'a t = 'a list
module DoubleListDequeue: DEQUEUE with type 'a t = 'a list * 'a list

(**
   You need to implement two modules with this signature.

   - [ListDequeue], which takes [t] to be ['a list], and is implemented in the expected way. For this implementation [push_back] and [pop_back] will be inefficient.
   - [DoubleListDequeue], which takes [t] to be a ['a list * 'a list].

   For the [DoubleListDequeue] implementation, we keep track of a
   [front] and [back] list, which we can pop from and push to.  This
   means the the [back] list holds elements in {e reverse}.  In the
   case that the [front] list is empty when [pop_front] is called, the
   [front] and [back] list should be {e balanced}. This means [front]
   and [back] after the call should have the property that

   {[List.length back - List.length front <= 1]}

   The same is true for a call to [pop_back], the roles of [front] and
   [back] reversed.  For example:

   {@text[
   front: []                     front: [1;2;3;4;5;6]
    back: [5;4;3;2;1]             back: []

   ⇓ pop_front                   ⇓ pop_back

   front: [2;3]                  front: [1;2;3]
    back: [5;4]                   back: [5;4]
   ]}

 *)

(** {2 Mapping Keys} *)

module StringMap : Map.S with type key = string
module IntMap: Map.S with type key = int
module StringSet: Set.S with type elt = string

(** In the second half of the course, we'll make use of maps and sets,
    which are implemented in OCaml using {e module functors}. In the
    case of maps, we create a module for the type of we want to use
    for keys.  For example, we use [Map.Make] to create structures for
    maps with a given type of key, e.g., [StringMap] and [IntMap] for
    maps with [string] keys and [int] keys, respectively.  In the case
    of sets, we use [Set.Make] to create structures for sets with a
    given type of element.

 *)

val flip_keys_and_values : int StringMap.t -> StringSet.t IntMap.t
(** Implement the function [flip_keys_and_values] so that
    [flip_keys_and_values m] is the map [m'] gotten by making the
    value of [m] into keys.  The value associated with the key [v] in
    [m'] is the set of keys in [m] which map to the value [v].

    This function will require looking in the the OCaml standard
    library (not [stdlib320]) on
    {{:https://ocaml.org/manual/5.3/api/Map.S.html}Maps} and
    {{:https://ocaml.org/manual/5.3/api/Set.S.html}Sets}.  *)

(** {1 Written} *)

(** {1 Partial Derivation} *)

(** Not all judgments are derivable, but it's possible to write {e
    partial} derivations of underivable judgments.  These partial
    derivations can give us insight into why we get certain type
    errors when type-checking.

    Formally, a typing derivation is {i partial} if its leaves are not
    necessarily axioms. A partial derivation is {i maximal} if every
    leaf of the derivation is either an axiom or a judgment which
    cannot be derived by any rule in our specification from any
    judgments.

    Give maximal partial derivations of the following typing
    judgments.

    {math
    \varnothing \vdash \texttt{if true then 1 + 3 else x / false} : \texttt{int}
    }

    {math
    \varnothing \vdash \texttt{if true then 1 + 3 else false / x} : \texttt{int}
    }


    In addition, write down the type error which OCaml gives you when
    trying to type-check each expression.  What does the type-error
    tell you about the order in which OCaml type-checks
    sub-expressions?



 *)

(** {1 Typing Derivation}

    So far, we haven't introduced any rules for typing recursive functions.
    Here's such a rule.

    {math
    \frac
    {\Gamma, f : \tau \to \tau_1, x : \tau \vdash e_1 : \tau_1
     \qquad
     \Gamma, f : \tau \to \tau_1 \vdash e_2 : \tau_2
    }
    {\Gamma \vdash \texttt{let rec} \ f \ x \ \texttt{=} \ e_1 \ \texttt{in} \ e_2 : \tau_2}
    }

    Note that this rule puts {m f} into the context when determining
    the type of {m e_1}, the {e body} of {m f}, which means that {m f}
    can appear in its own body (and, hence, may be recursive).  Using
    this rule, give a derivation of the following typing judgment.

    {math
    \varnothing \vdash \texttt{let rec sum l = match l with | [] -> 0 | h :: t -> h + sum t in sum} : \texttt{int list → int}
    }

    This will be a somewhat wide derivation. You may use arrows as
    we've done in lecture to make the derivation less wide, but please
    make sure your solution is very clear.

 *)

(** {1 Semantic Derivation} *)

(**
    {math
    \texttt{match 1 :: 2 :: [] with | [] -> true | h :: t -> false} \Downarrow \bot
    }

    Give a derivation of the above semantic judgment.

 *)
