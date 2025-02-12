(**
   In the previous lab, we built a tool to build structured data (a
   table) from unstructured data (text in TSV format).  In this lab,
   we'll expand on this idea by building a tool to build {e
   hierarchical} data (a tree) from text (you may be familiar with
   file formats like {{:https://en.wikipedia.org/wiki/JSON}JSON} for
   storing hierarchical data in text) We'll be working with
   {{:https://en.wikipedia.org/wiki/S-expression}S-expressions}, which
   have also historically been used for the syntax of programming
   languages, e.g., dialects of
   {{:https://en.wikipedia.org/wiki/Lisp_(programming_language)}LISP}.

   Make sure to pull down the course repository to get access to
   starter code for this lab in the directory [labs/lab4].

 *)

(** {1 S-Expressions} *)

(** An {b S-expression} is defined to be one of two things:

    - An {b atom}, which we will take to be any nonempty sequence of
    non-whitespace-non-parentheses characters.

    - An expression of the form {m \texttt( e_1 \ e_2 \ \dots \ e_k \texttt)}, where {m e_1} through {m e_k} are S-expressions.  In
    this lab, we will make the simplifying assumption that {m k} must
    be at least {m 1} and that {m e_1} is an atom, so that {m \texttt{()}} is not a valid
    S-expression, and neither is {m \texttt{((foo) bar)}}.

    Note that this is a {e recursive} definition, and the data an
    S-expression denotes can be represented as a [string ntree].

    {i Example.} the S-expression [(+ (+ 3 4) (+ 5 5))] is a valid
    S-expression, which denotes the tree

    {[
    let example =
      Node "+" [
        Node "+" [ Node "3" [], Node "4" []];
        Node "+" [ Node "5" [], Node "5" []];
      ]
    ]}

    which could be further processed into a value of a recursive ADT:

    {[

    type expr =
      | Num of int
      | Add of expr * expr

    let example = Add (Add (Num 3, Num 4), Add (Num 5, Num 5))
    ]}

    {i Another Example.} We've actually already come across
    S-expression in this course in passing: dune files are written as
    S-expressions.  Take a look at (part of) the dune file in this directory:

    {[
    (library
     (public_name lab4)
     (libraries stdlib320))
    ]}

    This S-expression could be represented by the following nonempty tree:

    {[
    Node "library" [
      Node "public_name" [Node "lab4" []];
      Node "libraries" [Node "stdlib320" []];
    ]
    ]}

    The goal of this lab is: {i construct nonempty trees of strings
    from S-expressions}.

 *)

(** {1 Tokenizing/Lexing} *)

(** When we're converting text to some kind of structured data,
    it's useful to first break up the text into its "relevant"
    parts.  As we will see in the second half of the course, this
    process is called {e lexing} or {e tokenizing}, and is a
    preprocessing step of {e parsing}.

    For example, if we're looking at the string:

    {["(defun foo (args x y) (+ (/ x y) (/ y x)))"]}

    we don't care so much that the third character is ['e'], as much
    as we care that the ['e'] is part of an atom ["defun"] which is
    the second "unit" of the given expression.  Likewise, we don't
    care {e how many} spaces or what kind of spaces there are between
    atoms, just that there is whitespace.  *)

type token =
  | Lparen
  | Rparen
  | Atom of string

val tokenize : string -> token list
(** The function [tokenize] separates out the parentheses and groups
    characters that correspond to a single atom in its input string.
    It also drops all whitespace.  For example, the above example
    would be tokenized as

    {[
    [Lparen; Atom "defun"; Atom "foo"; Lparen; Atom "args"; Atom "x";...]
    ]}

    Note that, at this point, we don't care if the input is a {e
    well-formed} S-expression, we just care about the valid "pieces"
    of the expression, e.g.,

    {[
    let _ = assert (tokenize "))asdf((" = [Rparen; Rparen; "asdf"; LParen; LParen]
    ]}

    You don't need to do anything for this part of the lab, we've
    provided the function [tokenize] for you. But, if you have time,
    take a look at it's implementation and make sure you know what's
    going on in it.

 *)

(** {1 Parsing} *)

(** The bulk of the today's lab work will be the following function.
 *)

val ntree_of_toks : token list -> string Stdlib320.ntree option
(** Implement the function [ntree_of_toks] so that [ntree_of_toks toks] is [None] if [toks] is not a valid S-expression, and is
    [Some t] otherwise, where [t] is a nonempty tree representation of the data in the S-expression
    [toks]. Some examples:

    {[
    let _ = assert (ntree_of_toks [Lparen; Atom "+"; Atom "2"; Atom "3"; Rparen]
                    = Some (Node "+" [Node "2" []; Node "3" []]))
    let _ = assert (ntree_of_toks [Lparen; Atom "+"; Atom "2"; Atom "3"; Rparen; Rparen]
                    = None)
    let _ = assert (ntree_of_token [] = None)
    let _ = assert (
      ntree_of_toks
        [Lparen; Atom "+"; Atom "2"; Lparen; Atom "foo"; Atom "3"; Rparen; Rparen]
        = Some (Node "+" [Node "2" []; Node "foo" [Node "3" []]])
    )
    ]}

    Implementing this function will require you to have a strong grasp
    on recursion.  {e Please spend some real time working on this,
    knowing how to implement a function like this is crucial for this
    course.}  A hint (in addition to asking the lab TF/TA for help):
    it's useful to first implement a function of type:

    {[
    token list -> (string ntree * token list) option
    ]}

    As you recurse, you'll want to keep track of the "progress" you've
    made, so you can return both the tree {e and} the part of the
    input list that {e hasn't} been processed yet, e.g.

    {[
    let _ = assert (helper [Lparen; Atom "+"; Atom "2"; Atom "3"; Rparen; Rparen]
                    = Some (Node "+" [Node "2" []; Node "3" []], [Rparen]))
    ]}

    That way, you can "continue" on the part that hasn't been
    processed.

 *)

val parse : string -> string Stdlib320.ntree option
(** Once you've finished this [ntree_of_toks], you should be able to
    parse and print S-expressions:

    {@text[
    utop>
    match Lab4.parse
    "
    (library
     (public_name lab4)
     (libraries stdlib320))
    "
    with
    | None -> ()
    | Some t -> Stdlib320.Ntree.print (fun x -> x) t;;

    library
    ├──public_name
    │  └──lab4
    └──libraries
       └──stdlib320
    - : unit = ()
    ]}

 *)

(** {1 Extra} *)

(** If you finish [ntree_of_toks], then there are a couple extensions you could work on:

    - Fill in [bin/main.ml] to take input at stdin as in Lab 3.
    - Implement the function [sexpr_of_ntree] described below.
    - Implement a function that takes as input S-expressions that represent dune files, addes dependencies to the [libraries] part of the S-expression, and then prints out the updated dune file.
    - Implement a function [json_of_ntree] so that you can convert S-expressions to JSON.
    - Implement a function which converts an S-expression into an arithmetic expression (if it has the right form) as the basis of a calculator with LISP-like input syntax.

    {e The point is:} once you can read in and represent heirarchical
    data, you can start processing and reformatting this data (a very
    common task).

 *)

val sexpr_of_ntree : string Stdlib320.ntree -> string
(** [sexpr_of_ntree t] is [t] given as an S-expression. Up to
    formatting and the [Some] constructor, it is the inverse of
    [parse] on well-formed S-expressions.

 *)
