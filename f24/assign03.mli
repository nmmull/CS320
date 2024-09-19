(** {1 Assignment 3}

    This assignment is due on {b Thursday 9/26 by 11:59PM}.
    It include both a programming part and a written part which are to be submitted on Gradescope under {i assign03} and {i assign03 (written)}, respectively.

    {b A Note on Testing:} You'll notice this time around that the tests for this assignment are divided per problem.
    If you want to run just the tests for problem 2, for example, you can run
    {[
    dune exec -- ./test/test_assign03_02.exe
    ]}
    Note the [.exe] extension. Running [dune test] will run all tests.
    In order to run the tests you still have to have defined every function in the assignment.
    {b We would recommend creating dummy values for each problem when you start} if you want to be able to run tests as you work.
 *)

(** {2 Programming Part} *)

(** {3 Problem 1: Unique Keys}

    An association list in OCaml is not required to have unique keys.
    The function [List.assoc] looks up elements from left to right, so that if there are keys mapped to multiple values, only the most recently added value is returned.

    However, it can be useful to assume that an association list has unique keys.
 *)

val mk_unique_keys : (string * int) list -> (string * int) list
(** Implement the function [mk_unique_keys] which given an association list [alst] with potentially repeating keys, returns a new association list with unique keys, and the property that {i the value associated with a key is the {b sum} of all the values it is mapped to in [alst]}.
    There is no requirement on the order of key-value pairs in the resultant association list.

    {[
    let alst = [("the", 1); ("cat", 1); ("in", 1); ("the", 1); ("hat", 1)]
    let alst = mk_unique_keys alst
    let _ = assert (List.assoc "cat" alst = 1)
    let _ = assert (List.assoc "the" alst = 2)
    ]}

    Put your solution in a file called [assign03/lib/assign03_01.ml].
    See the file [assign03/test/test_suite/test01.ml] for more example output behavior.
 *)

(** {3 Problem 2: Generalized Fibonacci (Tail-Recursion)}
    The Fibonacci sequence is defined as:
    {math
    F_n =
    \begin{cases}
    1 & n < 2 \\
    F_{n - 1} + F_{n - 2} & n \geq 2
    \end{cases}
    }

    This sequence can be generalized as follows. Given a list of starting values {m l}, we define the sequence
    {math
    G^l_n =
    \begin{cases}
    l[i] & i < \mathsf{len}(l) \\
    \sum_{i = 1}^{\mathsf{len}(l)} G^l_{n - i} & i \geq \mathsf{len}(l)
    \end{cases}
    }

    This is exactly the Fibonacci sequence when {m l} is [[1; 1]].
 *)

val gen_fib : int list -> int -> int
(** Implement the function [gen_fib] which, given a list of starting values [l] and an index [k], returns {m G^l_k} as defined above.
    Furthermore, {b your solution must be tail recursive}.
    Note that the operator [@] is technically not tail recursive, but you are allowed to use it in your solution.
    (For an added challenge, try to make the running time of your implementation independent of {m \mathsf{len}(l)} asymptotically.)
    The behavior of the implementation is undefined if the starting list [l] is empty or the index [k] is negative.

    Put your solution in a file called [assign03/lib/assign03_02.ml].
    See the file [assign03/test/test_suite/test03.ml] for more example output behavior.
 *)

(** {3 Problem 3: Tree Collapsing}

    One way to represent trees in OCaml is with the following recursive ADT.
    In this representation, a node may have zero or more children (as opposed to exactly two in the case of binary trees).

 *)

type tree =
  | Leaf of int
  | Node of tree list

(** The height of a tree is defined as the maximum number of steps from the root to a leaf or node.
    If a tree is just a leaf or a node with no children, then it has height {m 0} (this may be different from other definitions of height).
    So, for example, the tree
    {[
    let t =
      Node
        [ Node
          [ Leaf 1
          ; Node []
          ; Leaf 100
          ]
        ; Leaf 1
        ]
    ]}
    has height 2 ({i Hint.} See the file [test/test_suite/test03.ml] for an implement of the [height] function).

    The height of an element in a tree (i.e., a node or a leaf in a tree) is the number of steps from the root to that element.
    Note that the height of an element is at most the height of the tree.

    An element in a tree is {i terminal} if it a leaf or a node with no children.
    Note that terminal elements are the only elements whose height can be the same as the height of the tree.

    The process of {i collapsing} a tree to height {m i} (where {m i > 0}) is defined formally as follows: for every node {m N} of height {m i - 1}, replace its children with the terminal elements in the subtree rooted at {m N}, in order from left to right.
    So for example, collapsing [t] above to height {m 1} would yield the tree
    {[
    let t =
      Node
        [ Leaf 1
        ; Node []
        ; Leaf 100
        ; Leaf 1
        ]
    ]}
 *)

val collapse : int -> tree -> tree
(** Implement a function [collapse] which, given a {i positive} integer [h], and a tree [t], returns the same tree collapsed to height [h].
    The behavior of the implementation is undefined if [h] is not positive.

    Put your solution in a file called [assign03/lib/assign03_03.ml].
    See the file [assign03/test/test_suite/test03.ml] for example output behavior.
    {b Important.} You {i must} include the definition of [tree] in your solution.
 *)


(** {3 Problem 4: Grouping}

    A list of integers [l] is {i valid} if it satisfies the following properties:
    - every [0] appearing in [l] {b must} have nonzero integers to the left and right of it {b of opposite signs}
    - for every nonzero entry [x] of [l], if there is an entry adjacent to [x], then it must be the same sign as [x] or [0]

    Lists which do not not satisfy these properties are {i invalid}.
    So, for example
    {[
    let l1 = [1;2;3;0;-1;-2;-3;0;1]
    ]}
    is valid whereas
    {[
    let l1 = [1;2;3;0;1;2;3;0;1]
    let l2 = [0;1;2;3]
    let l3 = [1;0;0;-1]
    ]}
    are invalid.

    Intuitively a list is valid it it is made of nonempty sublists of same-sign integers, switching between positive and negative, separated by zeros (see how much nicer the formal description is?)

    The {i grouping} of a valid list of integers is a list of integer {i lists} which groups together the adjacent nonzero entries (and drops the zero entries).
    So, for example,
    {[
    let l1_grouping = [[1;2;3];[-1;-2;-3];[1]]
    ]}
    is the grouping for the valid list [l1] above.
 *)

val group : int list -> int list list option
(** Implement a function [group] which given a list of integers [l], returns the grouping of [l] given [l] is valid. The grouping should be returned as an [option], where the output is [None] if the list is invalid.

    Put your solution in a file called [assign03/lib/assign03_04.ml].
    See the file [assign03/test/test_suite/test04.ml] for example output behavior.
 *)

(** {2 Written Part} *)

(** {3 Problem 1: Typing Derivations}
    Write a derivation for the following typing judgment

    {math
    \{ \texttt{b} : \texttt{bool} \} \vdash
    \left(\texttt{let l i = [i; i + 1] in if b then l 0 else l 1}\right) \ : \ \texttt{int list}
    }
 *)

(** {3 Problem 2: Semantic Derivations}
    Write a derivation for the following semantic judgment
    {math
    \left(\texttt{let (x, b) = (3, false) in let l = [x; x + x] in if b then [] else l}\right) \Downarrow \texttt{[3; 6]}
    }
 *)
