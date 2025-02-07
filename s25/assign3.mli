(** This assignment is due on {b Thursday 2/13 by 8:00PM}.  You should
    put all of your solutions in a file called
    [assign3/lib/assign3.ml].  See the file [test/test_assign3.ml] for
    example behavior of each function.

 *)

(** {1 Programming} *)

(** {2 Practice Problems (Ungraded)}

    These problems come from a list of
    {{:https://ocaml.org/exercises}Exercises} on OCaml's webpage.  We
    won't grade these (the solutions are given with the problem
    statements).

    - {{:https://ocaml.org/exercises#56}Symmetric Binary Trees}
    - {{:https://ocaml.org/exercises#57}Binary Search Trees}
    - {{:https://ocaml.org/exercises#62B}Collect the Nodes at a Given Level in a List}
    - {{:https://ocaml.org/exercises#68}Preorder and Inorder Sequences of Binary Trees}
    - {{:https://ocaml.org/exercises#70B}Count the Nodes of a Multiway Tree}

    Note that a we call a multiway tree a {e nonempty tree} ([ntree]) in [stdlib320].

 *)

(** {2 Tree Conversion} *)

type 'a tree =
  | Leaf
  | Node2 of 'a * 'a tree * 'a tree

val ntree_of_tree : 'a tree -> 'a Stdlib320.ntree option
(** Implement the function [ntree_of_tree] so that [ntree_of_tree t]
    is [None] if [t] is empty (i.e., it's just a leaf) and, otherwise,
    is [Some t'] where [t'] is a representation of [t] as a nonempty
    (i.e., multiway) tree.

 *)


(** {2 Tail-Recursive Fibonacci Variant} *)

(** The standard Fibonacci sequence is define by the following
    recurrence relation.

    {math
    \begin{align*}
    F_0 &= 1 \\
    F_1 &= 1 \\
    F_n &= F_{n - 1} + F_{n - 2}
    \end{align*}
    }

    We will update this definition so that we have {e three} initial values
    (which are parameters of the function below):

    {math
    \begin{align*}
    G_0 &= a \\
    G_1 &= b \\
    G_2 &= c \\
    G_n &= G_{n - 1} + G_{n - 2} + G_{n - 3}
    \end{align*}
    }

 *)


val fib3_tail : (int * int * int) -> int -> int
(** Implement the function [fib3_tail] so that [fib (a, b, c) n] is
    the {m G_n} as defined above.  Your solution {b must} be tail
    recursive.

 *)

(** {2 File Trees} *)

(** We can think of a file system as tree of directories and files.
    In this problem we will be converting a list of files paths into
    an [ntree].  For example, given the paths:

    {@text[
    dir1/file1
    dir1/file2
    dir2/dir3/file1
    dir2/file1
    ]}

    we can construct the nonempty tree [file_tree "." paths]:

    {[
    Node (".", [
      Node ("dir1", [
        Node ("file1", []);
        Node ("file2", []);
      ]);
      Node ("dir2", [
        Node ("dir3", [
          Node ("file1", []);
        ]);
        Node ("file1", []);
      ]);
    ])
    ]}

    which we can then print with [NTree.print (fun x -> x)]:

    {@text[
    .
    ├──dir1
    │  ├──file1
    │  └──file2
    └──dir2
       ├──dir3
       │  └──file1
       └──file1
    ]}

    In this representation, a file is a leaf node, i.e., a node with
    no children.  We will take a very relaxed definition of a file
    name: {i a file name can be any sequence of characters not
    including ['/'].}  This means that every string is a valid path
    (even ["foo/bar///baz"]. In particular [""] represents the file
    with the empty string as its name (this is not realistic, but it
    make the problem easier).  This definition allows us to use the
    function [split_on_char] from Lab 3 to help us split a path into
    its individual components.

 *)

val file_tree : string -> string list -> string Stdlib320.ntree
(** Implement the function [file_tree] so that [file_tree root
    file_paths] is an [ntree] consisting of all the paths given in
    [file_paths], and whose root value is [root].  It is possible for
    [file_paths] to contain duplicates, but if it does not, there
    should be a one-to-one correspondence between paths from the root
    to the leaves of the tree and paths in [file_paths].

    To aid you, we'll provide the following function:

    {[
    let rec files_of_tree (Node (root, children)) =
      let rec expand children =
        match children with
        | [] -> []
        | c :: cs -> files_of_tree c @ expand cs
      in
      let rec add_root fs =
        match fs with
        | [] -> []
        | f :: fs -> (root ^ "/" ^ f) :: add_root fs
      in
      if children = []
      then [root]
      else add_root (expand children)
    ]}

    If you apply this function to the output of [file_tree], you
    should get back the same list of file paths (potentially in a
    different order) but with the root directory append to the front
    of each one.  *)

(** {2 Expressions (with Variables)} *)

(** In lecture, we saw a representation of arithmetic expressions as
    values of an ADT.  In this problem we'll be working with a similar
    ADT, representing arithmetic expressions (over addition and
    multiplication only) with variables.

 *)

type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

val subst : expr -> string -> expr -> expr
(** Implement the function [subst] so that [subst e1 x e2] is the
    result of replacing every instance of the variable identified by
    [x] in [e2] with the expression [e1].  In mathematical notation,
    this would be {m [e_1 / x] e_2}. So, for example, we would have
    that {m [y + 4 / z](z * 3) = (y + 4) * 3}.

    {i Note:} We've mentioned that substitution is tricky to implement
    correctly. This is {e not} the case for simple arithmetic
    expressions. In this case, substitution is exactly replacement of
    variables, no tricky cases.

 *)

val string_of_expr : expr -> string
(** Implement the function [string_of_expr] so that [string_of_expr e]
    is a string representation of [e] according to the following rules.

    - Every operator is surrounded by exactly one space.  There is no
    other whitespace in the string.
    - Multiplication has higher precedence than addition. This means that the expression [Mul (Num 2, Add (Num 3, Num 4))] should be represented by the string ["2 * (3 + 4)"] and not ["2 * 3 + 4"].
    - Sequences of the same operator should not have parentheses. For example, [Add (Num 2, Add (Num 3, Num 4))] should be represented by the string ["2 + 3 + 4"] and not ["(2 + 3) + 4"] or ["2 + (3 + 4)"].
    - Parentheses should be included only when necessary for disambiguation. For example, [Add (Mul (Num 2, Num 3), Num 4)] should be represented by the string ["2 * 3 + 4"] and not ["(2 * 3) + 4"]
    - A negative number should be surrounded by parentheses {e unless} it is the first number of a (possibly empty) sequence of operations at the top-level, or within parentheses. For example [Add (Num (-2), Add (Num (-3), Num 4))] should be represented as the string ["-2 + (-3) + 4"] whereas [Mul (Num (-2), Add (Num (-3), Num 4))] should be represented by the string ["-2 * (-3 + 4)"].  Note that [-3] is not surrounded by parentheses in this last example.

 *)


(** {1 Written} *)

(** All of the problems in this section are based on the inferences
    rules we've given in class, compiled {{:https://nmmull.github.io/PL-at-BU/320Caml/notes.html}here}.
 *)

(** {2 Typing Derivation to English} *)

(**
   {@text[

                                   ──────────────────────(iL) ──────────────────────(intLit)
                                   {f:int→bool} ⊢ 2 : int    {f:int→bool} ⊢ 3 : int
    ─────────────────────────(var) ────────────────────────────────────────────────(addInt)
    {f:int→bool} ⊢ f:int→bool                 {f:int→bool} ⊢ 2+3 : int
    ───────────────────────────────────────────────────────────────────(if)
                      {f:int→bool} ⊢ f (2+3) : bool
   ]}

    Write an English argument which expresses the same thing as the
    derivation above. {i Note.} It's very subjective what constitutes
    {e enough} detail. You'll be graded primarily on effort, please
    just be as descriptive as you can, using your best judgment.

 *)

(** {2 Typing Derivation} *)

(**
   {math
   \{ \texttt{f} : \texttt{int -> bool} \} \vdash \texttt{(if f 2 then (fun x -> true) else f) 3} : \texttt{bool}
   }

   Give a derivation of the above typing judgment. {i Note.} This will
   take a bit of space. Please write as neatly as possible.

 *)

(** {2 Semantic Derivation} *)

(**
   {math
   \texttt{let x = true in 3 + (if false || x then 2 else 1)} \Downarrow 5
   }

   Give a derivation of the above semantic judgment.

 *)
