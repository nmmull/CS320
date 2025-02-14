(** This assignment is due on {b Thursday 2/20 by 8:00PM}.  You should
    put all of your solutions in [assign4/lib/assign4.ml].  See the
    file [test/test_assign4.ml] for example behavior of each function.

 *)

(** {1 Programming} *)

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

val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
(** Implement the function [curry] which converts a "multi-argument"
    function (i.e., a function which takes a tuple as an argument) into
    nested single-argument functions.

 *)

val uncurry: ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Implement the function [uncurry], which is the inverse of [curry],
    i.e., it converts a Curried two-argument function into a function
    which take a tuple as an argument.

 *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** Implement the function [filter_map] so that [filter_map p l] is
    the result of applying [p] to every element of [l], filtering out
    the [None] elements, and keeping only the arguments to the [Some]
    elements.

 *)

val split_list: ('a * 'b) list -> 'a list * 'b list
(** Implement the function [split_list] so that [split_list l] is a
    pair of lists [(lefts, rights)] where [lefts] consists of the
    first elements of every pair in [l] (in order) and [rights]
    consists of the second elements of every pair in [l].

 *)


val split_tree: ('a * 'b) Stdlib320.ntree -> 'a Stdlib320.ntree * 'b Stdlib320.ntree
(** Implement the function [split_tree] so that [split_tree t]
    constructs two trees of the same structure as [t], one with the
    left element of the tuple in each node of [t], and one with the
    right element of the tuple in each node.

 *)

(** {2 Tree Filtering} *)

(** Defining filters on trees is a bit more complicated than on lists
    because we have to determine what structure should be maintained
    if the value at a node is removed.  One way of defining filters on
    tree is to {e promote} the leftmost child of a node that is
    filtered out by the given predicate.

 *)

val tree_filter: ('a -> bool) -> 'a Stdlib320.ntree -> 'a Stdlib320.ntree option
(** Implement the function [tree_filter] so that [tree_filter p t] is
    the result of keeping nodes in [t] according to the following
    rules:

    - If the value at a node satisfies [p], then it should be left
    as it is and it's children should be recursively filtered
    - If a node does not satisfy [p] and it has at least one child,
    then it should be replaced with it's leftmost child [l] and it's
    remaining children should be should be appended to the {b front}
    of the children of [l]. The resulting tree should then be
    recursively filtered.
    - If the value at a node does not satisfy [p] and it has no
    children, then the result should be [None].

 *)

(** {2 Random Walks} *)

(** We can think of a function [p] of type [int -> int] as a {i path
    generator} in the (infinite) complete graph on integers. Given a
    starting value [start], the path of length 3 generated by [p]
    starting at [start] is

    {[
    [start; p start; p (p start); p (p (p start))]
    ]}

    For example, the function [fun x -> x + 2] generates the length 5
    path [[0;2;4;6;8;10]] starting at [0].

    We can analogously, think of a function [r] of the similar type
    [int -> int list] as a {e random walk generator} in the same
    graph.  Rather than taking a single step from [x] to [r x], we can
    choose of the vertices uniformly at random from [r x].

    For example, the classic
    {{:https://en.wikipedia.org/wiki/Random_walk}drunkards walk} can
    be represented by the function

    {[
    let drunkard x = [x + 1; x - 1]
    ]}

    As we've learned from several of the prerequistes for this course,
    taking a random walk for a number of steps [n] from a starting
    point [start] defines a distribution over the vertices in our
    graph, telling us the probability of being at a each vertex
    after taking [n] steps from [start].

 *)

type rat = {
    num : int;
    denom : int;
  }

type distr = (int * rat) list
(** We can define a (discrete) probability distribution to be a
    collection of values paired with rational numbers, representing
    the probability of each value. If a value does not appear in this
    list, then it has probability [0]. We maintain that:

    - the sum of the probabilites is [1];
    - there is at most one pair [(v, prob)] for any value [v];
    - the values are in increasing order;
    - the rational numbers in the distribution are in reduced form.

 *)

val random_walk : (int -> int list) -> int -> int -> distr
(** Implement the function [random_walk] so that the distribution
    [random_walk walk start num_steps] is the one defined over
    vertices of the complete graph on integers after taking an
    [num_steps] length random walk starting at [start].  Specifically,
    if the probability of being at vertex {m n} is {m p}, then {m (n, p)} should appear in the resulting distribution.

    {[
    assert (random_walk drunkard 0 0 = [(0, {num=1;rat=1})])
    assert (random_walk drunkard 0 1 = [(-1, {num=1;rat=2}); (1, {num=1;rat=2})])
    assert (random_walk drunkard 0 2
            = [(-2, {num=1;rat=4}); (0, {num=1;rat=2}), (2, {num=1;rat=4})])
    ]}
 *)

(** {1 Written} *)

(** {2 An OCaml Puzzle} *)

(**
   {[
   type 'a foo = Foo of ('a foo -> 'a)
   let bar (Foo f) = f
   let baz x = bar x x
   let out = baz ???
   ]}

   Consider the above OCaml program. Given an expression to put in
   place of [???] so that [out] evaluates to the integer [42].
   Explain your answer.

 *)

(** {2 Typing Derivation} *)

(**
   {math
   \varnothing \vdash
   \texttt{let a = let b = 3 in b + b in (a, true)} : \tau
   }

   Determine a type {m \tau} such that the above typing judgment is
   derivable, and give the derivation.

 *)


(** {2 Semantic Derivation} *)

(**
   {math
   \varnothing \vdash
   \texttt{let a = let b = 3 in b + b in 4 * a} \Downarrow v
   }

   Determine a value {m v} such that the above semantic judgment is
   derivable, and give the derviation.

 *)
