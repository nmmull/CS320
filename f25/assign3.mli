(** This assignment is due on {b Thursday 9/26 by 8:00PM}.
    You should put all of your programming solutions in a file called [assign3/lib/assign3.ml].
    See the file [test/test_assign3.ml] for example behavior of each function.
    You should put all your written solutions in a single pdf file.

    {b Note.} All implementations in this assignment must be tail-recursive.
    We may have large test-cases which will fail for non-tail-recursive implementations.

 *)

(** {1 Programming (50%)} *)

(* (\** {2 Reverse Append} *\) *)

(* val rev_append : 'a list -> 'a list -> 'a list *)
(* (\** *)
(*    Implement the function [rev_append] so that [rev_append l r] is equivalent to [(List.rev l) @ r], but does not use the function [List.rev]. *)
(* *\) *)

(* (\** {2 Separate on Whitespace} *\) *)

(* val sep_on_whitespace : string -> string list *)
(* (\** *)
(*    Implement the function [sep_on_whitespace] so that [sep_on_whitespace s] is the result of removing all whitespace in [s] and combining the contiugous non-whitespace characters in [s]. *)
(*    You should use the your [split_on_char] function as a template, along with the function [is_whitespace] included in the starter code. *)
(*    {b Hint:} First write a function [split_on_whitespace] in analogy with [split_on_char] and then remove any unnecessary elements from its output. *)
(*  *\) *)

(** {2 Unlimited Register Machines} *)

type registers = (int * int) list

(**
   An unlimited register machine (URM) is an abstract representation of an infinite collection of registers indexed by integers.

   {[
   REGISTERS ... | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | ...
     INDICES      -4  -3  -2  -1   0   1   2   3   4
   ]}

   Programmatically, we can represent a URM [rs] as an {i association list}, i.e., a list of type [(int * int) list] where the pair [(i, j)] indicates [rs] has the value [j] in register [i].
   Furthermore, we assume that if [(i, j)] is not a member of [rs] for any value [j], then register [i] has the value [0].
   For example, the list [[(-1, 5); (0, 4); (3, 1)]] represents the URM:

   {[
   REGISTERS ... | 0 | 0 | 0 | 5 | 4 | 0 | 0 | 1 | 0 | ...
     INDICES      -4  -3  -2  -1   0   1   2   3   4
   ]}

   where all registers not pictured have the value [0].
   In this part of the assignment, you'll be filling in an interface for working with URMs.
   In each of the following functions you must maintain the following invariants:
   - the pairs in a URM representaion must be given in order of increasing index, e.g., [[(0, 1); (-5, 1)]] is an invalid URM representation, and instead should be [[(-5, 1); (0, 1)]];
   - there is no pair of the form [(i, 0)], e.g, [[(0, 5); (1, 0)]] is an invalid URM representation and instead should be [[(0, 5)]];
   - an index should be mapped to at most one value, e.g., [[(0, 5), (0, 6)]] is an invalid URM representation.

   {b Hint:} There are quite a few functions, but many of them have the same logic.
   Try to avoiding rewriting this logic.
   Write a general function and reuse it.
 *)

val load : int list -> registers
(**
   Implement the function [load] so that [load l] is the URM in which the [i]th element of [l] (using 0-indexing) is put in register [i].
 *)

val lookup : registers -> int -> int
(**
   Implement the function [lookup] so that [lookup rs i] is the value in register [i] of [rs].
 *)

val incr : registers -> int -> registers
(**
   Implement the function [incr] so that [incr rs i] is the result of adding [1] to the value in register [i] of [rs].
 *)

val zero : registers -> int -> registers
(**
   Implement the function [zero] so that [zero rs i] is the result of setting the value in register [i] of [rs] to [0].
 *)

val transfer : registers -> int -> int -> registers
(**
   Implement the function [transfer] so that [transfer rs i j] is the result of setting the value in register [j] of [rs] to be the value in register [i] of [rs] (the value in register [i] remains unchanged).
 *)

(** {2 URM Programs} *)

(**
   A URM program is a sequence of instructions of the following form:
   - [Z i] (zero the value in register [i])
   - [I i] (increment the value in register [i])
   - [T i j] (transfer the value in register [i] to register [j])
   - [J i j k] (jump to the [k]th instruction if the value in register [i] is equal to the value in register [j])

   A URM program is whitespace agnostic, so you can write each instruction on its own line, or you can write multiple instructions on the same line.
   All instructions codes and indices must be separated by whitespace.
   Here is an example program:
   {@text[
   J  1   2   100
      I 0     I 2
      J 0 0 0
   ]}

   We will represent a URM program as a [int list list] with the following correspondence for instructions:
   - [[0;i]] stands for [Z i];
   - [[1;i]] stands for [I i];
   - [[2;i;j]] stands for [T i j];
   - [[3;i;j;k]] stands for [J i j k].

   The above program is represented by the following list:
   {[
   [[3; 1; 2; 100]; [1; 0]; [1; 2]; [3; 0; 0; 0]]
   ]}

   As with any programming language, we can give a formal semantics for the evaluation of URM programs.
   In this setting, a {i configuration} is of the form
   {math
   \langle \ P \ , R \ , \ n \ \rangle
   }
   where {m P} is a URM program, {m R} is a URM, and {m n} is a {i program counter}.
   We use the program to keep track of what instruction to run.
   An evaluation judgment is of the form
   {math
   \langle \ P \ , R \ , \ n \ \rangle
   \longrightarrow
   \langle \ P' \ , R' \ , \ n' \ \rangle
   }
   and expression that the configuration on the left-hand side {i evaluates to} the configuration on the right-hand side.

   {math
   \frac
   {\textcolor{green}{P[n] = \texttt{Z i}}}
   {
   \langle \ P \ , \ R \ , \ n \ \rangle
   \longrightarrow
   \langle \ P \ , \ \texttt{zero} \ R \ \texttt{i}  \ , \ n + 1 \ \rangle
   }
   \text{(zero)}
   }


   {math
   \frac
   {\textcolor{green}{P[n] = \texttt{I i}}}
   {
   \langle \ P \ , \ R \ , \ n \ \rangle
   \longrightarrow
   \langle \ P \ , \ \texttt{incr} \ R \ \texttt{i}  \ , \ n + 1 \ \rangle
   }
   \text{(incr)}
   }

   {math
   \frac
   {\textcolor{green}{P[n] = \texttt{T i j}}}
   {
   \langle \ P \ , \ R \ , \ n \ \rangle
   \longrightarrow
   \langle \ P \ , \ \texttt{transfer} \ R \ \texttt{i} \ \texttt{j}  \ , \ n + 1 \ \rangle
   }
   \text{(transfer)}
   }

   {math
   \frac
   {
   \textcolor{green}{P[n] = \texttt{J i j k}}
   \qquad
   \textcolor{green}{\texttt{lookup} \ R \ \texttt{i} = \texttt{lookup} \ R \ \texttt{j}}
   }
   {
   \langle \ P \ , \ R \ , \ n \ \rangle
   \longrightarrow
   \langle \ P \ , \ \texttt{transfer} \ R \ \texttt{i} \ \texttt{j}  \ , \ \texttt{k} \ \rangle
   }
   \text{(jump-eq)}
   }

   {math
   \frac
   {
   \textcolor{green}{P[n] = \texttt{J i j k}}
   \qquad
   \textcolor{green}{\texttt{lookup} \ R \ \texttt{i} \neq \texttt{lookup} \ R \ \texttt{j}}
   }
   {
   \langle \ P \ , \ R \ , \ n \ \rangle
   \longrightarrow
   \langle \ P \ , \ \texttt{transfer} \ R \ \texttt{i} \ \texttt{j}  \ , \ n + 1 \ \rangle
   }
   \text{(jump-neq)}
   }

   Evaluating a program {m P} on registers {m R} means starting in the configuration {m \langle \ P \ , \ R \ , \ 0 \ \rangle} applying the above rules until the program counter becomes as least the number of instructions in {m P}.

   For example, the above program has the following evaluation behavior on the URM [load [5;3]]:
   {@text[
   ⟨   [J 1 2 100] I 0  I 2  J 0 0 0    ,   [(0, 5); (1, 3)]           ,   0   ⟩   ───► (jump-neq)
   ⟨    J 1 2 100 [I 0] I 2  J 0 0 0    ,   [(0, 5); (1, 3)]           ,   1   ⟩   ───► (incr)
   ⟨    J 1 2 100  I 0 [I 2] J 0 0 0    ,   [(0, 6); (1, 3)]           ,   2   ⟩   ───► (incr)
   ⟨    J 1 2 100  I 0  I 2 [J 0 0 0]   ,   [(0, 6); (1, 3); (2, 1)]   ,   3   ⟩   ───► (jump-eq)
   ⟨   [J 1 2 100] I 0  I 2  J 0 0 0    ,   [(0, 6); (1, 3); (2, 1)]   ,   0   ⟩   ───► (jump-neq)
   ⟨    J 1 2 100 [I 0] I 2  J 0 0 0    ,   [(0, 6); (1, 3); (2, 1)]   ,   1   ⟩   ───► (incr)
   ⟨    J 1 2 100  I 0 [I 2] J 0 0 0    ,   [(0, 7); (1, 3); (2, 1)]   ,   2   ⟩   ───► (incr)
   ⟨    J 1 2 100  I 0  I 2 [J 0 0 0]   ,   [(0, 7); (1, 3); (2, 2)]   ,   3   ⟩   ───► (jump-eq)
   ⟨   [J 1 2 100] I 0  I 2  J 0 0 0    ,   [(0, 7); (1, 3); (2, 2)]   ,   0   ⟩   ───► (jump-neq)
   ⟨    J 1 2 100 [I 0] I 2  J 0 0 0    ,   [(0, 7); (1, 3); (2, 2)]   ,   1   ⟩   ───► (incr)
   ⟨    J 1 2 100  I 0 [I 2] J 0 0 0    ,   [(0, 8); (1, 3); (2, 2)]   ,   2   ⟩   ───► (incr)
   ⟨    J 1 2 100  I 0  I 2 [J 0 0 0]   ,   [(0, 8); (1, 3); (2, 3)]   ,   3   ⟩   ───► (jump-eq)
   ⟨   [J 1 2 100] I 0  I 2  J 0 0 0    ,   [(0, 8); (1, 3); (2, 3)]   ,   0   ⟩   ───► (jump-eq)
   ⟨    J 1 2 100  I 0  I 2  J 0 0 0    ,   [(0, 8); (1, 3); (2, 3)]   ,   100 ⟩
   ]}

   We've put square brackets in the program around the instruction that the program counter points to in order to make the behavior more clear.
   In this problem, you'll be implementing an interpreter for URM programs.
 *)

val sep_on_whitespace : string -> string list
(**
   [sep_on_whitespace s] is the result of removing all whitespace in [s] and combining the contiugous non-whitespace characters in [s]. {i This function is implemented for you.}
 *)

val parse_urm : string list -> int list list
(**
   Implement the function [parse_urm] so that [parse_urm l] maps the ouput of [sep_on_whitespace s] to a URM program representation, given that [s] is a valid URM program.
   In particular, the behavior of the function is undefined if [s] does not represent a valid URM program.
 *)

val eval_urm : int list list -> registers -> registers
(**
   Implement the function [eval_urm] so that [eval_urm prog rs] is the result of evaluating the program [prog] on the URM [rs] according to the above rules.
 *)

(**
   {i All remaining functions are implemented for you.}
 *)

val interp_urm : string -> int list -> int
(**
   [interp_urm s args] is the result of interpreting the URM program [s] on the URM after loading the values in [args].
 *)

val max_urm : int -> int -> int
(**
   [max_urm i j] is the maximum of [i] and [j], implemented by intpreting a URM program.
 *)

val fibonacci_urm : int -> int
(**
   [fibonacci_urm i] is [i]th Fibonacci number, implemented by intpreting a URM program.
 *)

(** {1 Written (50%)} *)

(** {2 Typing Derivation (I)} *)

(**
    {math
    \def\code#1{\textcolor{purple}{\texttt{#1}}}
    \def\side#1{\textcolor{green}{#1}}
    \{ \code{x} : \code{int} , \code{y} : \code{bool} \} \vdash \code{if y then [] else x :: []} : \code{int list}
    }

    Give a typing derivation of the above typing judgment using the following rules.
    Do not use any shorthands in your derivation.

    {math
    \def\code#1{\textcolor{purple}{\texttt{#1}}}
    \def\side#1{\textcolor{green}{#1}}
    \frac
    {\side{(x : t) \in \Gamma}}
    {\Gamma \vdash x : t}
    \text{(var)}
    \qquad
    \frac
    {
    \Gamma \vdash e_1 : \code{bool}
    \quad
    \Gamma \vdash e_2 : \tau
    \qquad
    \Gamma \vdash e_3 : \tau
    }
    {\Gamma \vdash \code{if } e_1 \code{ then } e_2 \code{ else } e_3 : \tau}
    \text{(if)}
    \qquad
    \frac{}
    {\Gamma \vdash \code{[]} : \tau \code{ list}}
    \text{(nil)}
    \qquad
    \frac
    {
    \Gamma \vdash e_1 : \tau
    \quad
    \Gamma \vdash e_2 : \tau \code{ list}
    }
    {\Gamma \vdash e_1 \code{ :: } e_2 : \tau \code{ list}}
    \text{(cons)}
    }
 *)

(** {2 Typing Derivation (II)} *)

(**
   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \Gamma \vdash \code{g (f (g 0))} : \tau
   }

   Determine a minimal context {m \Gamma} and type {m \tau} such that the above typing judgment is derivable, and then given a derivation.
   Minimal here means that if you remove a variable declaration from {m \Gamma}, then the judgment is no longer derivable.
   You may use the following rules.
   Do not use any shorthands in your derivation.

   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}
   \def\side#1{\textcolor{green}{#1}}
   \frac
   {\side{(x : t) \in \Gamma}}
   {\Gamma \vdash x : t}
   \text{(var)}
   \qquad
   \frac
   {\side{n {\text{ is an int literal}}}}
   {\Gamma \vdash n : \code{int}}
   \text{(int)}
   \qquad
   \frac
   {
   \Gamma \vdash e_1 : \tau_2 \code{ -> } \tau
   \quad
   \Gamma \vdash e_2 : \tau_2
   }
   {
   \Gamma \vdash e_1 \ e_2 : \tau
   }
   \text{(app)}
   }

   Can there be more than one choice of {m \Gamma} and {m \tau}?
   Justify your answer and explain the relationship between {m \Gamma} and {m \tau}.
 *)

(** {2 URM programs} *)

(**
   The way that we formalize the semantic behavior of things like URM programs is by using the {i transitive closure} of the evaluation judgment we defined above.
   This can be formalized by introduing the following two rules which over a new judgment of the form {m C_1 \longrightarrow^\star C_2} where {m C_1} and {m C_2} are configurations as defined above:
   {math
   \frac{}
   {C \longrightarrow^\star C}
   \text{(refl)}
   \qquad
   \frac{
   C_1 \longrightarrow C_2
   \quad
   C_2 \longrightarrow^\star C_3
   }
   {
   C_1 \longrightarrow^\star C_3
   }
   \text{(trans)}
   }

   Write a derivation of the following judgment using these rules and the rules from the above section on URM programs.

   {math
   \def\code#1{\textcolor{purple}{\texttt{#1}}}

   \langle \ \code{J 0 1 2 I 0 I 1} \ , \ \code{[(0, 5); (1, 5)]} \ , \ 0 \ \rangle \longrightarrow^\star
   \langle \ \code{J 0 1 2 I 0 I 1} \ , \ \code{[(0, 5); (1, 6)]} \ , \ 3 \ \rangle
   }

   You can use {m P} has shorthand for the program {m \texttt{J 0 1 2 I 0 I 1}} but otherwise do not use any shorthands in your derivation.
 *)
