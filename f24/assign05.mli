(** {1 Assignment 5}

    This assignment is due on {b Thursday 10/10 by 11:59PM}.
    It includes both a programming part and a written part which are to be submitted on Gradescope under {i assign05} and {i assign05 (written)}, respectively.

    In order to run the given test suite you have to define every required function in the assignment.
    {b We would recommend creating dummy values for each problem when you start} if you want to be able to run tests as you work.
 *)

(** {2 Programming Part} *)

(** {3 Problem 1: Folding and OUnit2}

    Roughly speaking, test suites in OUnit2 are represented by the following ADT.

 *)

type 'a test =
  | TestCase of 'a
  | TestList of 'a test list

(** A test suite is either a test case (created using the [(>::)] operator) or made up of a list of other test suites (created using the [(>:::)] operator).
    The ADT is parametrized by the type variable ['a] because there are multiple kinds of test cases.

    When we check a test suite, we run each test in that suite from left to right (i.e., in-order traversal) and collect all the results.
    This is most naturally represented by a {i fold} operation over the test suite.
 *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b test -> 'a
(** Implement the function [fold_left] so that [fold_left op base test] is the result of applying [op] left-associatively to the test cases in [test] from left to right, starting with [base].

    Put your solution in a file called [assign05/lib/assign05_01.ml].
    See the file [assign05/test/test_suite/test01.ml] for example output behavior.
    In particular, it may be useful to look at the function [all_results] (which is not unlike how we calculate grades from an OUnit2 test suite).
    {b IMPORTANT:} You must include the definition of the type [test] in your solution.
 *)

(** {3 Problem 2: Continuation Passing Style {b (Extra Credit)}}

    We won't have an opportunity to discuss more deeply the notion of continuation passing style in this course, but we'll get a taste of it in this problem.

    When we introduced tail recursion, we said that {i any} function can be made tail recursive.
    This might be difficult to imagine when it comes to trees.
 *)

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(** Take for example the function which sums the elements of a tree.

    {[
    let rec sum t =
      match t with
      | Leaf -> 0
      | Node (x, l, r) -> x + sum l + sum r
    ]}

    A first attempt at a tail recursive version of this function might look like:

    {[
    let sum_tr =
      let rec go acc t =
        match t with
        | Leaf -> acc
        | Node (x, l, r) -> go (acc + x) ???
      in go 0
    ]}

    But it's unclear what we should recurse on, the left child, the right child, some combination?
    And does it really make sense to return the accumulated value at any leaf?

    {b Continuation passing style} (CPS) is a general technique for making any function tail recursive.
    The rough idea is to take as an argument a {i continuation} which is a {i function} that's called to "continue" the computation after we've computed our desired value.

    This gives recursive functions access to what will happen {i after} their done being evaluated.
    We can leverage this by pushing some of the computation into {i the continuation itself}.
    Since the body of a function isn't evaluated until the function is called, we can essentially "hide" recursive calls in the continuation.

    Let's consider an example.
    Here's the standard tail recursive sum function for lists as well as the CPS version.

    {[
    let list_sum_tr =
      let rec go acc l =
        match l with
        | [] -> acc
        | x :: xs -> go (x + acc) xs
      in go 0

    let list_sum_tr_cont l =
      let rec go l cont =
        match l with
        | [] -> cont 0
        | x :: xs -> go xs (fun sum_xs -> cont (x + sum_xs))
      in go l (fun x -> x)
    ]}

    Let's break this down. The function [go] takes as arguments [l] of type [int list] and [cont] of type [int -> 'a].
    The argument [cont] is the continuation. It's the function we call {i on the sum of the elements of [l]} once we've figured that out.
    So:

    - If [l] is empty, then the sum is [0] and we continue by calling [cont] on [0].
    - If [l] is nonempty of the form [x :: xs], then we figure out the sum of the elements of [xs], and continue by calling the continuation on the sum of the elements of [xs] {i plus [x]}.
      That way, we're ultimately calling the continuation on the sum of the elements of [l].

    Finally, we call [go] on the identity continuation [fun x -> x] so that what we do after we get the sum of the elements in [l] is just return the sum.

    In a bit more detail, the evaluation goes as follows:

    {[
    go [1;2] (fun x -> x)                                                        ---->
    go [2] (fun sum_2 -> (fun x -> x) (1 + sum_2))                               ---->
    go [] (fun sum_nil -> (fun sum2 -> (fun x -> x) (1 + sum_2)) (2 + sum_nil))  ---->
    (fun sum_nil -> (fun sum2 -> (fun x -> x) (1 + sum_12)) (2 + sum_nil)) 0     ---->
    (fun sum2 -> (fun x -> x) (1 + sum_2)) (2 + 0)                               ---->
    (fun x -> x) (1 + (2 + 0))                                                   ---->
    1 + (2 + 0)
    ]}

    In essence, we're encoding the sum in a sequence of function calls.
*)

val sum_tr : int tree -> int
(** Implement the function [sum_tr] so that [sum_tr t] is the sum of the elements in [t].
    Your implementation {b must} be tail recursive.
    You should use the above example as a prototype for continuation passing style.
    A rough outline of the function:

    - If the tree is empty, call your continuation on [0].
    - If the tree is nonempty, then recurse on the left child, and then {i in the continuation} recurse on the right child.

    Put your solution in a file called [assign05/lib/assign05_02.ml].
    See the file [assign05/test/test_suite/test02.ml] for example output behavior.
    {b IMPORTANT:} You must include the definition of the type [tree] in your code.
    {b IMPORTANT:} Even if you don't want to attempt this problem, you should fill in a dummy value, something like
    {[
    type 'a tree =
      | Leaf
      | Node of 'a * 'a tree * 'a tree

    fun sum_tr _ = 12345
    ]}
 *)

(** {3 Problem 3: Anonymous Functions}

    Consider the following ADT for expressions in a language with typed anonymous functions (this is sometimes called the {i simply typed lambda calculus}).
 *)

type ident = string

type ty =
  | Unit
  | Arr of ty * ty

type expr =
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

(** For example, the expression

    {[
    (fun (f : unit -> unit) -> fun (x : unit) -> f x) (fun (y : unit) -> y)
    ]}

    can be represented by the value

    {[
    App
    ( Fun
      ( "f"
      , Arr (Unit, Unit)
      , Fun
        ( "x"
        , Unit
        , App (Var "f", Var "x")
        )
      )
    , Fun ("y", Unit, Var "y")
    )
    ]}

    We type expressions according to the following rules.

    {math
    \frac{x : \tau \in \Gamma}{\Gamma \vdash x : \tau}
    \qquad
    \frac
    {\Gamma, x : \tau_1 \vdash e : \tau_2}
    {\Gamma \vdash \texttt{fun} \ \texttt{(} x \ \texttt{:} \ \tau_1 \texttt{)} \ \texttt{->} \ e : \tau_1 \to \tau_2}
    \qquad
    \frac
    {\Gamma \vdash e_1 : \tau_2 \to \tau
     \qquad
     \Gamma \vdash e_2 : \tau_2
    }
    {\Gamma \vdash e_1 \ e_2 : \tau}
    }
 *)

type ctxt = (ident * ty) list

val type_of : ctxt -> expr -> ty option
(** Implement the function [type_of] so that [type_of gamma e] is [Some t] if [e] has type [t] in the context [gamma], and is [None] otherwise.

    Put your solution in a file called [assign05/lib/assign05_03.ml].
    See the file [assign05/test/test_suite/test03.ml] for example output behavior.
    {b IMPORTANT:} You must include the definitions of [type] and [expr] in your code.
 *)

(** {3 Problem 4: Sets}

    Consider the following signature for a finite integer sets (click [INT_SET] to expand).
 *)

module type INT_SET = sig
  type t
  (** [t] is the type used to represent a finite set of integers. *)

  val empty : t
  (** [empty] is the empty set {m \emptyset} (the set with no elements). *)

  val singleton : int -> t
  (** [singleton n] is the set {m \{n\}}. *)

  val mem : int -> t -> bool
  (** [mem x s] is [true] if and only if [x] is a member of [s]. *)

  val card : t -> int
  (** [card s] is the number of elements in [s]. *)

  val union : t -> t -> t
  (** [union s1 s2] is the set in contains the elements which appear in [s1] or [s2]. *)
end

type set_info =
  { ind : int -> bool
  ; mn : int
  ; mx : int
  }

(** In this problem, you need to implement two modules with the above signature.

    - [ListSet], which takes [t] to be [int list], and represents finite sets of integer as {b sorted} lists of {b distinct} integers (i.e., no repeats).
    - [FuncSet], which takes [t] to be the record type [set_info] defined above.
      [ind] is an indicator for which elements appear in the set, and [mn] and [mx] are lower and upper bounds, respectively, for the elements in the set.
      You {b must} maintain the invariant that if the set is not empty, then [mn] and [mx] are elements of the set.
      If the set is empty, then [mn] and [mx] may be any values such that [mn > mx].

    Put your solution in a file called [assign05/lib/assign05_04.ml].
    See the file [assign05/test/test_suite/test04.ml] for example output behavior.

    {b IMPORTANT:} You must include the definition of [set_info] in your code.
    You should {b not} include the module signature [INT_SET] and you should {b not} or give a signature to the modules [ListSet] or [FuncSet].
    That is, you should write
    {[
      module ListSet = struct
       (* ... *)
      end
    ]}
    and you should {b not} write
    {[
      module ListSet : INT_SET = struct
        (* ... *)
      end
    ]}
    The signatures of [ListSet] and [FuncSet] are already given in the [.mli] file.
    Finally, you should leave the files [assign05_04.mli] and [assign05_04_intf.ml] as they are.
 *)

(** {2 Written Part} *)

(** {3 Problem 1: Recursive Functions}

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

    The difference between this and our {m \textsf{LetFunc}} rule is that the variable {m f} appears in the context of the left premise.
    Using this rule, give a typing derivation to the following judgment.

    {math
    \cdot \vdash \texttt{let rec f n = if n = 0 then 1 else f (n - 1) in f 10} : \texttt{int}
    }

    {3 Problem 2: Anonymous Functions}

    We also haven't given a rule for evaluating anonymous function applications.
    Here's such a rule.

    {math
    \frac{e_2 \Downarrow v_2 \qquad [v_2 / x] e_1 \Downarrow v}
    {(\texttt{fun} \ x \ \texttt{->} \ e_1) \ e_2 \Downarrow v}
    }

    Using this rule, give a derivation for the following judgment

    {math
    \texttt{(fun x -> x + 1) ((fun x -> x * x) 3)} \Downarrow \texttt{10}
    }

    {i Remark:} We're being a little informal here, things get a bit more complicated when we start evaluating higher-order functions.
*)
