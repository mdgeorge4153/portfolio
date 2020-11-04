
(** This is documentation for a subset of the [Async] library that should be
    sufficient for completing problem set 5.  We have omitted some modules,
    functions, and optional arguments.  The full [Async] documentation can be
    found {{:https://ocaml.janestreet.com/ocaml-core/111.03.00/doc/async/#Std} here}.  *)

(** The common definitions for [Async].  Async programs typically only
    [open Async.Std], the modules within [Async.Std] are typically referred to
    explicitly. *)
module Std : sig

(******************************************************************************)
(** {2 Blocking functions}                                                    *)
(******************************************************************************)

module Deferred : sig
  type 'a t

  val both : 'a t -> 'b t -> ('a * 'b) t
  (** [both t1 t2] becomes determined after both t1 and t2 become determined. *)

  val all : 'a t list -> 'a list t
  (** [all ts] returns a deferred that becomes determined when every t in ts is
      determined. The output is in the same order as the input. *)

  val any : 'a t list -> 'a t
  (** [any ts] returns a deferred that is fulfilled when any of the underlying
      deferreds is fulfilled *)

  module List : sig
    (** Provides blocking variants of the standard List module functions *)

    val map    : 'a list -> ('a -> 'b t) -> 'b list t
    val iter   : 'a list -> ('a -> unit t) -> unit t
    val fold   : 'a list -> 'b -> ('b -> 'a -> 'b t) -> 'b t
    val filter : 'a list -> ('a -> bool t) -> 'a list t
    val find   : 'a list -> ('a -> bool t) -> 'a option t
  end

end (* of Deferred *)

(** ['a Deferred.t] is the most important type in the [Async] library.  An
    ['a Deferred.t] represents a value that will become determined
    when an asynchronous computation completes.

    A deferred can be "undetermined" or "determined". A deferred that is
    undetermined may at some point become determined with value v, and will
    henceforth always be determined with value v.

    Any Async function that needs to wait for something to happen (i.e.
    "block") will return a [Deferred.t].  The function itself will return
    immediately, allowing the calling program to continue to do other tasks.
    Thus blocking functions should have the type ['a -> 'b Deferred.t].  *)

val (>>=)  : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t
(** [>>=] is used to chain blocking functions together.  [x >>= g] will cause
    [g] to be run after [x] is determined, with the value of [x] as input.  The
    deferred returned by [(>>=)] will become determined when [g] completes.

    There are two common stylistic idioms used when using [>>=].  The first is
    that many functions can be joined together with [>>=] without parentheses.
    For example, if one wants to wait for [x] to be determined, and then run
    [f] on the result, and then run the blocking function [g] on the output of
    [f], and then run the blocking function [h] on the result of [f], one would
    write the following:
    {[
    x >>= f >>= g >>= h
    ]}
    When splitting these invocations on multiple lines, one typically writes
    {[
    x
    >>= f
    >>= g
    >>= h
    ]}

    The second idiom is that anonymous function definitions are typically
    written inline and without parentheses.  For example, in the above sequence,
    if we wanted to print the value that is passed from f to g, we would write:
    {[
    x >>= f >>= fun y ->
    print_endline y;
    g y >>= h
    ]}
    This is the same as
    {[ x >>= (f >>= (fun y -> print_endline y; (g y >>= h))) ]}
    but the way it is written looks more like imperative code: first run [f] on
    [x] and call the result [y].  Then print [y].  Then run [g] on [y] and pass
    the result to h.

    It is helpful to think of the pattern
    {[
    f x >>= fun y ->
    ]}
    as being analogous to
    {[
    let y = f x in
    ]}
    This is why [(>>=)] is also called "bind".
    *)

val (>>|) : 'a Deferred.t -> ('a -> 'b) -> 'b Deferred.t
(** [>>|] is similar to [>>=], but it is used for non-blocking functions.  It
    can be mixed to chains of functions joined by [>>=].  For example, the
    provided function [WordCount.App.Make.main] is implemented as follows:
    {[
    Deferred.List.map Reader.file_lines filename
    >>| List.flatten
    >>= map_reduce
    >>= output
    ]}

    filenames is a list of file names: [["foo.txt"; "bar.txt"]].
    {!Deferred.List.map} is like List.map except that it works with blocking
    functions like {!Reader.file_lines}.

    The output of this line is a list containing a list of lines of ["foo.txt"]
    and a list of lines of ["bar.txt"].  We want to combine these into a single
    list, so we pass them through the [List.flatten] function.
    Since [List.flatten] is non-blocking, we use [>>|].  We then
    pass the combined list of lines to the [map_reduce] function
    (using [>>=] since [map_reduce] is a blocking function), and
    finally pass the results of [map_reduce] to the (blocking)
    [output] function.
    
    Be aware that [>>|] does not play as nicely with anonymous functions as
    [>>=] does.  The following code does {b not} work:
    {[
    f x >>| fun y ->
    print_endline y;
    g y >>= h
    ]}
    A good test of your understanding of the [>>=] and [>>|] syntax would be to
    explain why. *)

val return : 'a -> 'a Deferred.t
(** [return] is used to create a {!Deferred.t} that is immediately determined
    with the provided value.  It is often used to designate the return value of
    a function implemented using a chain of [>>=] expressions.  For example:
    {[
    let run_fgh x =
      f x >>= fun y ->
      g y >>= fun z ->
      h z >>= fun _ ->
      return (y,z)
    ]}
    will first run f on x; when that completes it will run g, and then h.  The
    result of the deferred created by [run_fgh] will be the pair [(y,z)].  *)

(******************************************************************************)
(** {2 Combining [Deferred.t]s}                                               *)
(******************************************************************************)

val don't_wait_for : unit Deferred.t -> unit
(** a convenience function for ignoring a deferred value *)

val never : unit -> 'a Deferred.t
(** a deferred that is never determined *)

(** See also {!Deferred.both}, {!Deferred.any} and {!Deferred.all}. *)

(******************************************************************************)
(** {2 Deferred data structures and communication}                            *)
(******************************************************************************)

(** Ivars are like write-once [ref]s.  They are initially undetermined, but you
    can fill them.  They are useful for creating Deferreds that you can
    determine whenever you want. *)
module Ivar : sig
  type 'a t

  val create : unit -> 'a t
  (** Create a new unfilled [Ivar.t] *)

  val read : 'a t -> 'a Deferred.t
  (** [read iv] returns the [Deferred.t] associated with [iv].  This deferred
      becomes determined when the Ivar is filled. *)

  val fill   : 'a t -> 'a -> unit
  (** Fill the given [Ivar.t].  Raises an exception if the Ivar is already full. *)

  val fill_if_empty : 'a t -> 'a -> unit
  (** Fill the given [Ivar.t].  Does nothing if the Ivar is already full. *)

  val is_full : 'a t -> bool
  (** Return true if the [Ivar.t] is full. *)

  val is_empty : 'a t -> bool
  (** Return true if the [Ivar.t] is empty. *)
end

(** A Pipe is a first-in first-out communication channel.  A pipe has a
    "writer" end and a "reader" end.  The writer feeds values into
    the pipe and the reader can subsequently read them back out. *)
module Pipe : sig

  module Reader : sig
    type 'a t
  end

  module Writer : sig
    type 'a t
  end

  val create : unit -> 'a Reader.t * 'a Writer.t
  (** Creates a new pipe, and returns the reader and writer ends. *)

  val write  : 'a Writer.t -> 'a -> unit Deferred.t
  (** Writes data into the writer end of a pipe. *)

  val read   : 'a Reader.t -> [`Eof | `Ok of 'a] Deferred.t
  (** Reads data from the reader end of the pipe.  Returns [`Eof] if the pipe
      has been closed (we have not documented the [close] function here). *)
end

(** The {!Deferred.List} contains blocking versions of many familiar List
    module functions. *)

(******************************************************************************)
(** {2 Time and timeouts }                                                    *)
(******************************************************************************)

(** Note: Use the function [Core.Std.sec] to create {!Core.Std.Time.Span.t}s.
    It takes in a [float] [t] and produces a [Time.Span.t] representing [t]
    seconds. *)

val after : Core.Std.Time.Span.t -> unit Deferred.t
(** a deferred that becomes determined after the given amount of time *)

val with_timeout : Core.Std.Time.Span.t -> 'a Deferred.t
                -> [`Result of 'a | `Timeout] Deferred.t
(** [with_timeout t x] will become determined with [`Result v] if [x] becomes
    determined with [v] within the timespan [t].  Otherwise, the value will be
    [`Timeout]. *)

val every : ?start : unit Deferred.t -> ?stop : unit Deferred.t
         -> Core.Std.Time.Span.t
         -> (unit -> unit)
         -> unit
(** [every t f] schedules [f] to be executed every [t] seconds.  If the
    arguments [?start] or [?finish] are supplied, [f] will not be scheduled
    before [start] becomes determined or after [finish] does. *)


(******************************************************************************)
(** {2 Input and output in Async}                                             *)
(******************************************************************************)

val printf : ('a,unit,string,unit) format4 -> 'a
(** This function is an Async-friendly version of
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html} [Printf.printf]},
    which is very useful for debugging. *)

(** The Reader module (not to be confused with {!Pipe.Reader}) is for doing
    network and file input. *)
module Reader : sig
  type t
  (** A [Reader.t] is a handle to a file or network stream. *)

  val file_lines : string -> string list Deferred.t
  (** [file_lines filename] returns a list of the lines in the named file. The
      lines do not contain the trailing newline. *)

  val file_contents : string -> string Deferred.t
  (** [file_contents filename] returns the string with the full contents of the
      file *)

  val read_line : t -> [`Eof | `Ok of string] Deferred.t
  (** reads a single line from the given input stream.  Returns `Eof if the
      stream was closed before the line could be read. *)

end

(** The Writer module (not to be confused with {!Pipe.Writer}) is for doing
    network and file output. *)
module Writer : sig
  type t
  (** A [Writer.t] is a handle to a file or network stream. *)

  val write_line : t -> string -> unit
  (** Writes a line of text to the given output stream.  May raise an
      exception if there is a network error. *)

end

(** The socket module contains types necessary for working with the [Tcp]
    module. *)
module Socket : sig
  type 'a t

  (** Close one or both ends of a socket *)
  val shutdown : 'a t -> [`Both | `Receive | `Send] -> unit

  module Address : sig
    type t
  end
end


(** Functions for connecting to remote hosts over the network. *)
module Tcp : sig
  type 'a where_to_connect
  (** Represents an address that can be connected to. *)

  val to_host_and_port : string -> int -> Socket.Address.t where_to_connect
  (** Interprets a string and int as a network host and port that can be
      connected to. *)

  val connect : 'a where_to_connect
             -> ('a Socket.t * Reader.t * Writer.t) Deferred.t
  (** Create a connection to the given address.  Returns a [Reader.t] and a
      [Writer.t] that can be used to communicate with the remote endpoint.

      Raises an exception if it cannot connect to the worker *)

  (** Note: you can close the connection by calling {!Socket.shutdown `Both} on the
      Socket *)
end


(******************************************************************************)
(** {2 Error handling}                                                        *)
(******************************************************************************)

val try_with : (unit -> 'a Deferred.t) -> ('a,exn) Core.Std.Result.t Deferred.t
(** [try_with f] runs the blocking function [f], and returns [Core.Std.Ok x] if
    [f ()] returns [x].  If [f ()] raises the exception [e], then [try_with f]
    returns [Core.Std.Error e]. *)

end

