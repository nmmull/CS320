(** In this lab, we'll be building a
    {{:https://en.wikipedia.org/wiki/Tab-separated_values}TSV reader}.
    This will require us to work with [string]s and [list]s.  It will
    also give us an opportunity to do some IO.

    This time around we've included some starter code for you,
    primarily so that your [dune] files are correctly configured.  If
    you pull down the course repo, you'll see a dune project at the
    directory [labs/lab3].  Unless otherwise specified, you should
    implement your functions in a file called [lab3/lib/lab3.ml].

 *)

(** {1 Splitting Strings} *)

(** We won't work directly with [string]s too often in this course,
    but it's valuable some experience working with [string]s in OCaml.
    First, it's important to note that [string]s are {i not} the same
    as [list]s of [char]acters.  As we did in Assignment 1, we work
    with [string]s primarily by grabbing individual [char]acters or
    slices of the [string] using [String.sub].  This is due to how
    [string]s are stored in memory.  We won't dwell on this, but OCaml
    discourages users from working with [string]s as [char list]s
    because doing so will likely be inefficient; we'll follow suit in
    this lab.

    TSV stand for {e tab-separated value} and is a file format similar
    to CSV, except with tabs instead of commas.  In a TSV file, each
    line consists of text separated by tabs, and the entire file
    represents a table for which each line represents a row.  This
    means reading a TSV file consists of two basic steps: split the
    file into lines by newline [char]acters (['\n']) so that you have a
    [string list] consisting of the rows of the table, and then split
    each row (of type [string]) by tab [char]acters (['\t']) into a
    [string list] consisting of its individual entries.  In both
    steps, we need a function that can split a [string] with a given
    [char]acter as a delimiter.

 *)

val split_on_char : ?ignore_trailing:bool -> char -> string -> string list
(** Implement the function [split_on_char] so that [split_on_char ~ignore_trailing:b c s] is a [string list] consisting of all
    substrings of [s] delimited by the [char]acter [c].  The role of
    [ignore_trailing] is to determine the behavior of splitting the
    empty string [""].  Some examples:

    {[
    let _ = assert (split_on_char 'c' "acbcdccec") = ["a"; "b"; "d"; ""; "e"; ""]
    let _ = assert (
      split_on_char ~ignore_trailing:true 'c' "acbcdccec"
      = ["a"; "b"; "d"; ""; "e"]
    )
    let _ = assert (split_on_char ' ' "hello  world" = ["hello"; ""; "world"])
    let _ = assert (
      split_on_char ~ignoree_trailing:true ' ' "hello  world"
      = ["hello"; ""; "world"])
    let _ = assert (split_on_char 'c' "" = [""])
    let _ = assert (split_on_char ~ignore_trailing:true 'c' "" = [])
    let _ = assert (split_on_char 'c' "c" = [""; ""])
    let _ = assert (split_on_char ~ignore_trailing:true 'c' "c" = [""])
    ]}

    A couple things to note here. First off, we're using an
    {{:https://cs3110.github.io/textbook/chapters/basics/functions.html#labeled-and-optional-arguments}optional
    argument} for the [ignore_trailing] parameter.  This is an
    advanced feature of OCaml that you are not required to use, but
    its good to know it's there.  Second, the reason we include the
    [ignore_trailing] parameter at all is because some editors
    automatically include a trailing newline [char]acter at the end of
    file on saving, and if we split a file into lines with this
    trailing ['\n'], we'll end up with a empty string at the end of our
    list.  This paramter allows us to decide whether or not we want
    that last empty string.
 *)

(** {1 Building Tables} *)

(** Once we have a way of splitting a string on a given [char]acter,
    it's pretty easy to write function that will convert a [string] into
    a table of [string]s, represented as a [string list list].  For
    simplicity, we won't require that rows in our table have the same
    length.  *)

val table_of_string : string -> string list list
(** Use [split_of_char] to implement the function [table_of_string] so
    that [table_of_string s] is a [string list list] gotten by first
    splitting [s] on newline [char]acters (['\n']) ignoring the last
    trailing newline character (if present), and then splitting each
    line on tab [char]acters (['\t]).  *)

(** {1 Extracting a Column} *)

(** Once we have a table, we should do something with it.  One
    useful operation is to extract a single column from a table.  But
    in order to define this function, we need to specify a couple
    things.
    - We will determine columns of our table by identifiers given in the first row of the table.  If the table is empty, the the function should fail (evaluate to [None]).
    - If there are multiple column with the same identifier, we'll always extract the leftmost column.
    - If a row does not have a value in the column we're extracting (e.g., we're extracting the 5th column and the 10th row is of length 3) the we'll assume the value in this column is the empty string [""]

 *)

val get_col : string list list -> string -> string list option
(** Implement the function [get_col] so that [get_col table col_id] is

    - [None] if the table is empty or of [col_id] is not an member of
    the first row of [table]
    - [Some col] where [col] is a list of all entries of the table in
    the column given by [col_id], given [col_id] is a member of the
    first row

    In other words, if [col_id] is the {m n}th member of the first row
    of [table], then [col] is a list of the {m n}th members of every
    row except the first row.

 *)

(** {1 IO (Putting everything together)} *)

(** We've provided code for you in the file [bin/main.ml], but you
    should take a look at it because you may need to set this up
    yourself in the future.  The contents should look something like:

    {[
    let usage = "USAGE: dune exec lab3 col_id < tsv_file"

    let () =
      if Array.length (Sys.argv) <> 2
      then print_endline usage
      else
        let col_id = Sys.argv.(1) in
        let table_str = Stdlib320.read () in
        let table = Lab3.table_of_string table_str in
        let col = Lab3.get_row table col_id in
        match col with
        | None -> print_endline (col_id ^ " is not a column of the input table")
        | Some col -> print_endline (String.concat "\n" col)
    ]}

    Everything in the file [bin/main.ml] is executed when we run

    {@text[
    dune exec lab3
    ]}

    so you can think of last expression [let () = ...] as the "main"
    function in other languages.  In this expression, we take a column
    identifer as a command line argument and a TSV file at stdin.  The
    [<] operator is a form of {e redirection}, in this case, opening
    the specified file for reading on stdin.

    If you've done everything correctly, you should be able to run

    {@text[
    dune exec lab3 Email < example.tsv
    ]}

    which should print a list of emails extracted from the file
    [example.tsv].

 *)
