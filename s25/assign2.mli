(** This assignment is due on {b Thursday 2/6 by 8:00PM}.  You should
    put all of your solutions in a file called
    [assign2/lib/assign2.ml].  See the file [test/test_assign2.ml] for
    example behavior of each function.

 *)

(** {1 Programming} *)

(** {2 Practice Problems (Ungraded)}

    These problems come from a list of
    {{:https://ocaml.org/exercises}Exercises} on OCaml's webpage.  We
    won't grade these (the solutions are given with the problem
    statements).

    - {{:https://ocaml.org/exercises#6}Palindrome}
    - {{:https://ocaml.org/exercises#10}Run-Length Encoding}
    - {{:https://ocaml.org/exercises#13}Run-Length Encoding of a List (Direct Solution)}
    - {{:https://ocaml.org/exercises#15}Replicate the Elements of a List a Given Number of Times}

 *)

(** {2 Integers or Strings} *)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

val convert : int_or_string list -> int_list_or_string_list list
(**
    Implement the function [convert] so that [convert l] is an
    [int_list_or_string_list list] in which adjacent [string] values
    and [int] values are grouped together.  {b Important.} Make sure
    to include the definitions of [int_or_string] and
    [int_list_or_string_list] in your solution.

 *)

(** {2 Recipes by Ingredients} *)

type recipe = {
  name : string;
  ingrs : string list;
}

val recipes_by_ingrs : recipe list -> string list -> recipe list
(**
   Implement the function [recipes_by_ingrs] such that
   [recipes_by_ingrs recs ingrs] is the sublist of recipes from [recs]
   (preserving their order in [recs]) whose ingredients are entirely
   included in [ingrs].  {b Important.} Make sure to include the
   definition of [recipe] in your solution.

 *)

(** {2 Memory Allocator} *)

type mem_status =
  | Free
  | Occupied

type memory = (mem_status * int) list

(**
   In this problem, you will be building an allocator for a very
   simple model of (unlimited) memory.  In this model, we represent
   memory as a list of pairs of values, each representing a chunk of
   memory.  The first entry of the pair tells us if the chunk is
   [Free] or [Occupied], and the second tells us how many units of
   memory the chunk occupies.  Furthermore, we require that memory has
   the following properties:

   - There are no adjacent [Free] chunks of memory (there may be adjacent chunks of [Occupied] memory)
   - There is no [Free] chunk of memory at the end of the list, i.e., either the list is empty or the last element is an [Occupied] block.

   For example, the list:

   {[
   [Free 3; Occupied 1; Occupied 3; Free 2; Occupied 1]
   ]}

   would represent the layout:

   {@text[
   Status:   | F | F | F | O | O | O | O | F | F | O | F | F | ...
   Position: 0   1   2   3   4   5   6   7   8   9   10  11  12...
   ]}

   Note that positions do not correspond to indices of chunks in the
   list.

   You will be implementing two functions. The function [allocate]
   allocates memory by placing an [Occupied] chunk of memory in the {i
   leftmost} available position.  It's possible to allocate a chunk of
   memory at position [i] of size [size] if the positions [i], [i + 1], ..., [i + size - 1] are [Free].  After allocation, these positions
   become [Occupied].  In the above example, it should be possible to
   allocate a chunk of memory of size [2] at position [0] giving us
   the list:

   {[
   [Occupied 2; Free 1; Occupied 1; Occupied 3; Free 2; Occupied 1]
   ]}

   whereas the leftmost place to allocate a chunk of memory of size
   [5] is at position [10], giving us the list:

   {[
   [Free 3; Occupied 1; Occupied 3; Free 2; Occupied 1; Occupied 5]
   ]}

   The second function [free] should make an [Occupied] block of
   memory into a [Free] block, given its position. In addition it {b
   must} maintain the invariants described above.

 *)

type alloc_result =
  | Success of int * memory
  | Invalid_size

val allocate : int -> memory -> alloc_result
(** Implement the function [allocate] so that [allocate size mem]
    is an [alloc_result] according to the following allocation
    strategy:

    - If [size] is not positive, then it is invalid.
    - Otherwise, [allocate size mem] should be the same as [mem]
    except with an [Occupied] chuck of memory of size [size] in the {i
    leftmost} possible position, together with the position itself.

 *)

type free_result =
  | Success of memory
  | Invalid_position

val free : int -> memory -> free_result
(** Implement the function [free] so that [free pos mem] is a
    [free_result] such that:

    - If [pos] is negative, or is not the beginning of an [Occupied]
    chunk of memory, then the position is invalid
    - The chunk starting at [pos] in [mem] should be converted to a
    [Free] block and the result should be updated to maintain the
    above invariants (no adjacent [Free] chunks, no ending [Free]
    chunks.

 *)

(** {1 Written} *)

(** {2 Typing in OCaml (1)} *)

(**
   {math
   \{ \texttt{z} : \texttt{int} \} \vdash \texttt{fun x -> fun y -> x y z + y z + z} : \tau
   }

   Is there a type {m \tau} such that the above typing judgment holds in
   OCaml? If so, determine the type.  If not, justify your answer.

 *)

(** {2 Typing in OCaml (2)} *)

(**
   {math
   \{ \texttt{g} : \texttt{int -> bool} \} \vdash \texttt{let rec f x = g (f (x + 1))} : \tau
   }

   Is there a type {m \tau} such that the above typing judgment holds in
   OCaml? If so, determine the type.  If not, justify your answer.

 *)

(** {2 For Loops} *)

(**
   Suppose you're interested in adding a kind of functional
   for-loop into an OCaml-like language.  You decide on the syntax

   {[
   <expr> ::= repeat <expr> times <expr> from <expr>
   ]}

   That is, if {m e_1}, {m e_2}, and {m e_3} are well-formed expressions, then so is

   {math
   \texttt{repeat} \ e_1 \ \texttt{times} \ e_2 \ \texttt{from} \ e_3
   }

   The hope is that you can write code like

   {[
   let fact n =
     let loop =
       repeat n times
         let (i, m) = acc in
         (i + 1, m * i)
       from
         (1, 1)
     in let (_, out) = loop in out
   ]}
   or even:
   {[
   let fact n =
     let loop =
       repeat n times
         { i = acc.i + 1; out = acc.out * acc.i }
       from
         { i = 1; out = 1 }
     in loop.out
   ]}

   Intuitively, we need to requires in [repeat n times e1 from e2] that [n] is an [int] and the [e1] and [e2] have the same type.
   Furthermore we require that [e1] is well-typed a context which
   {b includes a variable [acc] which is the same type as that of [e1]
   and [e2]}.

   In English, the typing rule for this new construct is as follows:
   if {m e_1} is of type [int] in the context {m \Gamma}, and {m e_2}
   is of type {m \tau} in the context {m \Gamma} with the additional declaration
   that the variable {m acc} is of type {m \tau}, and {m e_3} is of type {m \tau}
   in the context {m \Gamma}, then {m \texttt{repeat} \ e_1 \ \texttt{times} \ e_2 \ \texttt{from} \ e_3} is of type {m \tau}.

   Write this typing rule as a formal inference rule.

 *)
