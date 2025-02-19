(**
   Higher-order functions are particularly useful when we have to do a
   lot of list processing, e.g., for many applications in scheduling.
   In this lab, we'll be building a toy-version of
   {{:https://www.when2meet.com}when2meet}, an online tool for
   determining a meeting time against a collection of potentially
   conflicting schedules.

   Pull down the course repository to get the starter code for this
   lab in the directory [labs/lab5].

 *)

(** {1 Intervals and Schedules} *)

(**
   Suppose that we're trying to schedule a meeting for a group of
   people.  Each person has windows of time at which they are
   available to meet.  Our job is to collect those times and
   determine during which windows everyone in the group is available.

   We'll call a window of time an {i interval}.  An interval consists
   of a {i day} of the week and a {i starting time} and {i ending
   time}.  Times will be written in 24-hour notation, and we'll only
   allow the minute-part of a time to be [:00], [:15], [:30], or [:45]
   (this is consistent with when2meet).  We will also assume that an
   interval does not span multiple days.

   A {i schedule} is then defined to be a list of intervals.  As a
   simplifying assumption, we'll assume that {i all intervals in a
   schedule are disjoint} (you can try to remove this assumption by
   preprocessing schedules to eliminate overlaps).

   All the types we've just described are given below.

 *)

type day = Mo | Tu | We | Th | Fr | Sa | Su

type hour = int

type minute =
  | M00
  | M15
  | M30
  | M45

type time =
  {
    hour : hour;
    minute : minute;
  }

type interval =
  {
    day : day;
    start_time : time;
    end_time : time;
  }

type schedule = interval list

(** {1 Prelude: Comparison} *)

(** Before getting into the bulk of this lab, we need to take some time
    to look at {e comparison} functions.

    It would be nice if we could order the schedule output by our
    program by day and starting time.  If you take a look at
    [bin/main.ml], you'll see that this is one step in the process.
    But the [sort] function in the OCaml standard library doesn't take
    an boolean ordering function, it takes a more general {e
    comparison} function.

    A comparison function [compare] on ['a] is a function of type ['a -> 'a -> int]
    such that [compare x y] is:

    - positive if [x > y]
    - [0] if [x = y]
    - negative if [x < y]

    The benefit of such a function is that we don't need to remember
    which boolean ordering function we need to pass into [sort] (e.g.,
    is it [(<)] or [(<=)] or maybe even [(>)]).  Other functions can
    be parametrized by comparison functions, e.g., the [min] and [max]
    functions provided in the starter code.

    We'll start by getting some practice with this.

 *)

val is_empty : interval -> bool
(** Implement the function [is_empty] so that [is_empty i] is [true]
    if [i] the starting time of [i] is greater than or equal to the
    end time of [i] and [false] otherwise.  {i Hint:} Use
    [compare_time].

 *)

val compare_interval: interval -> interval -> int
(** Implement the function [compare_interval] so that it is a
    comparison function for intervals as described above {e which
    orders intervals by day and start_time.} For example, any interval
    on Wednesday should appear before any interval on Friday, but
    given two intervals on Wednesday, the one with earlier starting
    time should appear first, or they should be considered equal if
    they have the same starting time.

 *)

(** Make sure to look at the code given to you. In particular, see how
    comparison functions are defined for other types like [day] and
    [minute] and [time].  Also note that Monday is the {e first} day
    in our ordering of days.

 *)

(** {1 Intersecting Schedules} *)

(**
   The primary objective of this lab is determine the intersection of
   multiple schedules.  It will be easiest to solve a sequence of
   sub-problems:

   - intersect an interval with an interval
   - intersect an interval with a schedule
   - intersect two schedules
   - intersect a list of schedules

   Many of these problems have natural implementations using
   higher-order functions, and we recommend trying to implement them
   in this way.

 *)

val intersect_i_i : interval -> interval -> interval option
(** Implement the function [intersect_i_i] so that [intersect_i_i i1 i2]
    is [None] if [i1] and [i2] have empty intersection, and
    otherwise is [Some i] where [i] is the intersection of [i1] and
    [i2].  {i Note:} Two intervals can have
    nonempty intersection only if they are on the same day.

 *)

val intersect_s_i : schedule -> interval -> schedule
(** Implement the function [intersect_s_i] so that [intersect_s_i s i]
    is the schedule of intervals in [s] intersected with [i].  {i
    Hint:} Use [List.filter] and [List.map] (or [List.filter_map]).

 *)

val intersect_s_s : schedule -> schedule -> schedule
(** Implement the function [intersect_s_s] so that [intersect_s_s s1 s2]
    is a schedule of intervals consisting of the intersections of [s1]
    and [s2] (it may help to draw a picture). {i Hint:} Use
    [List.fold_left].
 *)


val intersect_schedules : schedule list -> schedule
(** Implement the function [intersect_schedules] so that
    [intersect_schedules scheds] is the intersection of all schedules
    in [scheds].  Given the empty list, it should produce a schedule
    containing all available time. {i Hint.} Use [List.fold_left].

 *)

(** Again, this is the core functionality of our program. What follows
    will allow us to read in and print out schedules so that we can
    interact with this core functionality.

 *)

(** {1 Parsing Intervals} *)

(** Our program will work by reading schedules from files given at the
    command line. We can imagine having every person in the group
    sending us a file consisting of intervals during which they're
    available, and we can then run our program on all the files we
    receive.

    For exapmle, if [Kiki] and [Tombo] send us their schedules in the
    files [kiki.sch] and [tombo.sch], respectively, then we'll be able
    to run:

    {@text[
    dune exec lab5 -- kiki.sch tombo.sch
    ]}

    in order to determine when they are both available to meet.

    A schedule file will consist of several lines, each of which
    contains an interval.  There will be one interval per line. For
    example:

    {@text[
    Mo  08:00--12:00
    Th  10:00--13:00
    Mo  14:00--18:00
    Fr  15:00--17:00
    ]}

    The time-part of an interval will be written as two times
    separated by [--], with no whitespace.  We will require that a
    time is written with 5 characters, so 8:00AM must be written as
    [08:00].  The day-part will be written first, and should consist
    of the two first letters of the day with the first letter
    capitalized.  Besides these restrictions, a schedule file is
    whitespace agnostic, e.g., this would also be a valid schedule:

    {@text[
                 Mo           08:00--12:00
      Th        10:00--13:00
    Mo  14:00--18:00
        Fr       15:00--17:00
    ]}

    Note that it is possible to have multiple intervals for a single
    day, and intervals are not required to be in order.

    For this lab, we've handled reading the lines of each file for you
    (if you're interested, take a look at [bin/main.ml]).  Your task
    is to parse a single line into an [interval].
 *)

val interval_of_string_opt : string -> interval option
(** Implement the function [interval_of_string_opt] so that
    [interval_of_string_opt s] is [Some i] where [i] is the [interval]
    represented by [s] according to the above rules, and is [None]
    otherwise.  You should assume that [s] is trimmed, i.e., there is
    no whitespace at the start or end of the [s].

 *)

(**
   Again, make sure to look at the functions already defined for you in
   [lib/lab5.ml].  A number of them will be helpful in solving this problem.

 *)

(**
   {1 Putting Everything Together}
 *)

(**
   Once we've implemented everything above, we should be able to pass
   in multiple file names to our program as command line arguments and
   check when everyone is available to meet:

   {@text[
   dune exec lab5 -- sched1.sch sched2.sch sched3.sch
   ]}

   We've included a couple small example schedules, but you can try it
   out yourself on larger schedules.

 *)

(* END*)
