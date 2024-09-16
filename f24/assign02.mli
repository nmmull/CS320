(** {1 Assignment 2}

    This assignment is due on {b Thursday 9/19 by 11:59PM}.
    It include both a programming part and a written part which are to be submitted on Gradescope under {i assign02} and {i assign02 (written)}, respectively.

 *)

(** {2 Programming Part} *)

(** {3 Problem 1: Tic-tac-toe}

    The following is a small interface for a tic-tac-toe board.
    A board is represented as a tuple of rows (from top to bottom), a row is represented as a tuple of [pos]itions (from left to right) and a [pos]ition either has a [piece] or is [Blank].

    We will use [row_index] and [col_index] to refer to particular positions in the board.
 *)

type piece = X | O
type pos
  = Piece of piece
  | Blank

type board =
  (pos * pos * pos)
  * (pos * pos * pos)
  * (pos * pos * pos)

type row_index
  = Top
  | Middle
  | Bottom

type col_index
  = Left
  | Middle
  | Right

type pos_index = row_index * col_index

val get_pos : board -> pos_index -> pos
(** Implement the function [get_pos], which given a board and a position index, returns the position at that index. *)

val winner : board -> bool
(** Also implement the function [winner] which determines if there is a winner on a given board.
    Recall that a board has a winner if there are three of the same kind of piece in any row, column, or diagonal.
    It is not required that the board is valid.
    In particular, it doesn't matter if there are two winners, it just matters that the winning condition is met.

    There is a long way to implement these functions.
    See if you can implement it a bit more elegantly.
 *)

(** Put your solution in a file called [assign02/lib/assign02_01.ml].
    See the file [assign02/test/test_suite/test01.ml] for example output behavior.
    {b Important:} Make sure to include the definitions the types above in your in your solution.
 *)

(** {3 Problem 2: Matrices}

    The following is a simple record type for matrices.
    The (floating-point) entries of the matrix are stored in the field [entries] as a list of rows of equal length.
    The record type also has a field [rows] for the number of rows and [cols] for the number of columns.
 *)

type matrix =
  { entries : float list list
  ; rows : int
  ; cols : int
  }

val mk_matrix : float list -> int * int -> matrix
(** Implement the function [mk_matrix] which, given a list of floating-point [entries] and a pair of integers [(r, c)] representing a number of rows and a number of columns, constructs a matrix with [r] rows and [c] columns using the numbers in [entries].
    That is, the first [c] numbers in [entries] make up the first row, the next [c] numbers make up the second row, and so on.
    You may assume that [entries] is length [r * c].
    The behavior of the implementation is undefined otherwise.

    {[
    let _ =
      let a = mk_matrix [1.;0.;0.;1.] (2, 2) in
      let b = {entries = [[1.;0.];[0.;1.]]; rows = 2; cols = 2} in
      assert (a = b)
    ]}

    Put your solution in a file called [assign02/lib/assign02_02.ml].
    See the file [assign02/test/test_suite/test02.ml] for more example output behavior.
    {b Important:} Make sure to include the definition of [matrix] in your solution.

 *)

(** {3 Problem 3: Walking Distance}

    The following is a simple variant type for the cardinal directions, along with a type synonym for a path represented as a list of directions.
    We think of a path as representing a walk along a grid, with each direction in the path representing one step in this grid.
    To be concrete, we will presume that one "step" is 1 meter (i.e., [North] is interpreted to mean "walk 1 meter north").
    Note that a path may backtrack and intersect with itself.
 *)


type dir
  = North
  | South
  | East
  | West

type path = dir list

val dist : path -> float
(** Implement the function [dist] which, given a list of directions [dirs], computes the {m \ell_2} distance (i.e., "as the crow flies") in meters between the starting point and ending point of the path given by [dirs].
    Note that it doesn't matter where you start, the distance between the starting and ending points will always be the same.

    {[
    (* We'll use an unnecessarily large error margin *)
    let is_close f1 f2 = abs_float (f1 -. f2) < 0.000001

    let _ = assert (is_close (dist [North; North; South; East]) (sqrt 2.))
    ]}

    Put your solution in a file called [assign02/lib/assign02_03.ml].
    See the file [assign02/test/test_suite/test03.ml] for more example output behavior.
    {b Important:} Make sure to include the definitions of [dir] and [path] in your solution.
 *)

(** {2 Written Part} *)

(** {3 Problem 1: Typing Derivations}

    Write a derivation of the typing judgment

    {math
    \{\texttt{z} : \texttt{int}\} \vdash \left(\texttt{let x = z + 5 in let y = "five" in (x + z, y)}\right) : \texttt{int * string}
    }

    {3 Problem 2: Semantic Derivations}

    Write a derivation of the semantic judgment

    {math
    \left(\texttt{let x = 5 in let z = x + x in (x * z, z)}\right) \Downarrow \texttt{(50, 10)}
    }
 *)
