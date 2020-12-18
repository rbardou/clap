(**********************************************************************************)
(* MIT License                                                                    *)
(*                                                                                *)
(* Copyright (c) 2020 Romain Bardou                                               *)
(*                                                                                *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy   *)
(* of this software and associated documentation files (the "Software"), to deal  *)
(* in the Software without restriction, including without limitation the rights   *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *)
(* copies of the Software, and to permit persons to whom the Software is          *)
(* furnished to do so, subject to the following conditions:                       *)
(*                                                                                *)
(* The above copyright notice and this permission notice shall be included in all *)
(* copies or substantial portions of the Software.                                *)
(*                                                                                *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *)
(* SOFTWARE.                                                                      *)
(**********************************************************************************)

(** Simple command-line argument parsing. *)

(** {2 Argument Kinds} *)

(** On the command-line, there are several kinds of arguments:

    - unnamed arguments, such as [depth] or [\--depth] or [\\depth],
      which do not start with a dash, or are exactly a single dash,
      and if they start with a backslash,
      this backslash is removed (so [\--depth] means [--depth],
      which is unnamed even though without the backslash it starts
      with a dash);

    - long-named arguments, such as [--depth], which start with two dashes;

    - short-named arguments, such as [-d], which start with a dash
      and which can be grouped together ([-abc] is the same as [-a -b -c]).

    The functions below allow to specify argument names with [long] and [short].
    If neither [long] nor [short] is given, the argument is unnamed.
    If [long] is given, the argument can be given on the command-line
    using the long name ["--" ^ long].
    If [short] is given, the argument can be given on the command-line
    using the short name (a dash followed by [short], possibly among others).

    At the beginning of the execution of your program, [Sys.argv] is put
    in an internal list, called the list of remaining arguments.
    The functions below will consume arguments from this list.
    Removed arguments will not be read by future consumers, as they
    no longer exist.

    You must consume unnamed arguments *after* the named arguments which
    can appear before them.
    For instance, in [--depth 42 filename.txt], is the next unnamed argument
    [42] or [filename.txt]? Until Clap knows whether [--depth] is the
    name of an argument or a flag, it doesn't know.
    But if you read [--depth] first, there is no issue.

    To prevent confusion, if you try to consume an unnamed argument,
    it must be the first in the list of remaining arguments.
    If this list is [--depth 42 filename.txt] for instance, trying to
    read an unnamed argument will cause an error saying that [--depth]
    is an invalid value for this argument (since it is not unnamed).

    A consequence of that is that is if you are trying to make subcommands
    (a la Git), you must read all global options first, then the subcommand
    as an unnamed argument, and then you may read the remaining options.
    Contrary to Git though, global options will be allowed after the
    subcommand name. *)

(** {2 Description} *)

(** Set the contents of the DESCRIPTION section of [--help].

    If you use [subcommand]s, this description is used when no [subcommand]
    has been selected. Otherwise, the description of the selected [case] is used. *)
val description: string -> unit

(** {2 Custom Types} *)

(** Custom type descriptions. *)
type 'a typ

(** Make a custom type.

    The [dummy] value is returned for missing mandatory arguments,
    or for arguments which are given invalid values.

    The [parse] function is used to parse arguments.
    It shall return [None] if parsing fails, i.e. if the argument is invalid.

    The [show] function is used to display default values in the help.

    The [name] is used in errors if [parse] returns [None]. *)
val typ:
  name: string ->
  dummy: 'a ->
  parse: (string -> 'a option) ->
  show: ('a -> string) ->
  'a typ

(** Make an enum type.

    Usage: [enum name cases]

    Same as [typ ~name ~default ~parse ~show] where:
    - the [default] value is the first value of [cases];
    - the [parse] and [show] functions find the first matching [cases]
      and return the associated value.

    Do not use this to make subcommands; use [subcommand] instead.
    It produces a better output for [--help].

    Function [compare] is used by [show] to find the name of the value to show.
    It defaults to [Stdlib.compare].

    @raise Invalid_argument if [cases] is empty. *)
val enum: ?compare: ('a -> 'a -> int) -> string -> (string * 'a) list -> 'a typ

(** The following types are built-in types that you probably do not need to use directly.
    Instead of writing [Clap.(mandatory string)] or [Clap.mandatory Clap.string]
    for instance you can just write [Clap.mandatory_string]. *)

(** String arguments (no parsing). *)
val string: string typ

(** Integer arguments (parsed with [int_of_string_opt]). *)
val int: int typ

(** Float arguments (parsed with [float_of_string_opt]). *)
val float: float typ

(** {2 Mandatory Arguments} *)

(** The following functions consume a mandatory argument.

    If [last] is [false], only consume the first occurrence.
    Other occurrences are preserved.

    If [last] is [true], consume all occurrences and return the last.
    All occurrences are removed and will not be read again.
    All occurrences must be valid.
    For unnamed arguments, this stops at the first argument which starts
    with a dash. For instance, reading an unnamed arguments with [last]
    set to [true] in [a b c --plop d e f] returns [c] and leaves [--plop d e f]
    in the list of remaining arguments.
    This is why one normally parses unnamed arguments after named ones,
    so that named arguments have all been consumed already.

    Default value for [last] is [false] for unnamed arguments
    and [true] for other arguments. This allows to override values of
    named arguments by specifying the argument again.

    Arguments can be given a [placeholder], which defaults to ["VALUE"].
    It is used in the generated help and some error messages.
    It is usually in uppercase.

    Arguments can also be given a [description], used in the generated help.

    If the argument is missing or its value is invalid, these functions
    return a dummy value. This is so that your program execution can still
    reach [close] to display help for all arguments. *)

(** Read a mandatory string argument. *)
val mandatory_string:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> string

(** Read a mandatory integer argument. *)
val mandatory_int:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> int

(** Read a mandatory float argument. *)
val mandatory_float:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> float

(** Read a mandatory argument of a custom type. *)
val mandatory:
  'a typ ->
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> 'a

(** {2 Optional Arguments} *)

(** The following functions are similar to mandatory arguments above,
    but they return [None] if the argument is missing instead of
    causing an error. *)

(** Read an optional string argument. *)
val optional_string:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> string option

(** Read an optional integer argument. *)
val optional_int:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> int option

(** Read an optional float argument. *)
val optional_float:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> float option

(** Read an optional argument of a custom type. *)
val optional:
  'a typ ->
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> 'a option

(** {2 Optional Arguments With Default Values} *)

(** The following functions are similar to optional arguments above,
    but they return the given default value if the argument is missing. *)

(** Read an optional string argument with a default value. *)
val default_string:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  string -> string

(** Read an optional integer argument with a default value. *)
val default_int:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  int -> int

(** Read an optional float argument with a default value. *)
val default_float:
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  float -> float

(** Read an optional argument of a custom type with a default value. *)
val default:
  'a typ ->
  ?last: bool ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  'a -> 'a

(** {2 Lists of Arguments} *)

(** The following functions read all occurrences of an argument.

    For unnamed arguments, this stops at the first argument which starts
    with a dash. For instance, reading the list of unnamed arguments in
    [a b c --plop d e f] returns [a b c] and leaves [--plop d e f] in the
    list of remaining arguments.
    This is why one normally parses unnamed arguments after named ones,
    so that named arguments have all been consumed already. *)

(** Read repeated string arguments. *)
val list_string:
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> string list

(** Read repeated integer arguments. *)
val list_int:
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> int list

(** Read repeated float arguments. *)
val list_float:
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> float list

(** Read repeated arguments of a custom type. *)
val list:
  'a typ ->
  ?long: string ->
  ?short: char ->
  ?description: string ->
  ?placeholder: string ->
  unit -> 'a list

(** {2 Flags} *)

(** Read a boolean value.

    Usage: [flag default]

    If [last] is [false], find the first occurrence of [set_long], [set_short],
    [unset_long] or [unset_short] and consume it.
    Return [true] if [set_long] or [set_short] was found,
    [false] if [unset_long] or [unset_short] was found.

    If [last] is [true] (which is the default), find all occurrences of
    [set_long], [set_short], [unset_long] or [unset_short] and consume then.
    Return [true] if [set_long] or [set_short] is the last occurrence,
    [false] if [unset_long] or [unset_short] is the last occurrence.

    If no occurrence is found, return [default]. *)
val flag:
  ?last: bool ->
  ?set_long: string ->
  ?set_short: char ->
  ?unset_long: string ->
  ?unset_short: char ->
  ?description: string ->
  bool -> bool

(** {2 Subcommands} *)

(** Subcommand cases. *)
type 'a case

(** Make a subcommand case.

    Usage: [case command f]

    If the value of the subcommand argument is [command], trigger [f]. *)
val case: ?description: string -> string -> (unit -> 'a) -> 'a case

(** Make a default subcommand case.

    Used if the subcommand argument was not specified by the user.
    If you don't provide a default case and the user does not specify
    a command, [subcommand] causes an error.

    The default case is not used if the user gave an invalid subcommand
    argument. An error is emitted instead. The default case is for
    the absence of an argument, not for an invalid argument. *)
val default_case: ?description: string -> (unit -> 'a) -> 'a case

(** An uninhabited type denoting that a function never returns.

    Used by [subcommand] callbacks to make sure that they exit
    or raise an exception, since there is no way to build a value
    of type [never_returns]. *)
type never_returns

(** Read a subcommand.

    Subcommands are unnamed string arguments.
    They are mandatory unless there is a [default_case].
    They are similar to [enum]s except that they usually allow
    additional parameters which are specific to each subcommand.
    Each subcommand case causes a specific function to be called,
    which can read those specific arguments.

    The first matching case in the list is used.
    If another case matches, it is ignored.

    You can call [close] in each case function, or after the call to
    [subcommand]. In the first case, this means that case functions
    can use argument values themselves. In the second case, this means
    that case functions will probably just return a variant describing the
    command, that you will pattern-match outside of [subcommand].

    Note that if the user did not specify a valid command,
    [subcommand] automatically calls [close] with [on_help] and [on_error].
    Indeed, [Arg] assumes that the remaining arguments are command-specific,
    and that without a valid command, there won't be any.
    The default values for [on_help] and [on_error] are the same as the default
    values of [close] itself. The type is slightly different though:
    [subcommand] requires those callbacks to exit or raise an exception.

    Default placeholder is ["COMMAND"]. *)
val subcommand:
  ?on_help: (unit -> never_returns) ->
  ?on_error: (string -> never_returns) ->
  ?placeholder: string ->
  'a case list -> 'a

(** {2 End of Parsing} *)

(** Check that the list of remaining arguments is empty and that no error occurred.

    If not, and unless [--help] is one of the remaining arguments,
    call [on_error]. The default is to print help, print an error,
    and exit with code [1].

    If [--help] is one of the remaining arguments, call [on_help].
    The default is to print help and exit with code [0].

    You must call this function before using the values of your
    arguments. Otherwise they may contain dummy values. For instance,
    in a mandatory string argument is missing, it will contain the
    empty string; and if an integer argument contains an invalid
    integer, it will contain [0] instead. Calling [close] would cause
    your program to exit instead. It also prevents the program from
    running in case the user added an unused argument by mistake,
    for instance if he mispelled one of them.

    The reason errors do not cause the program to exit before [close]
    is to be able to output the full help even if the user does not
    give valid arguments.

    There are exceptions to this rule: arguments which cannot trigger errors
    can be used before [close]. This means arguments which are not mandatory
    and for which the parser cannot fail.
    In practice, this is limited to [optional], [default], [list],
    and only for the [string] type (i.e. [optional_string], [default_string]
    and [list_string]) or custom types for which the parsing function always
    accepts its input. *)
val close:
  ?on_help: (unit -> unit) ->
  ?on_error: (string -> unit) ->
  unit -> unit

(** Emit an error to be displayed by [close].

    Only the first error is kept. Other errors are ignored.

    You may perform some checks on arguments before calling [close]
    and emit an error if those checks fail. Remember though that if
    an argument is already invalid (or missing), the value you get
    is a dummy value, so your check is probably meaningless.
    That being said, if this happens [error] will already have been
    called and your error will be ignored.

    If possible, it is usually better to use [mandatory], [optional]
    or [default] with a custom conversion function.
    Indeed, if the argument is specified multiple times, this will
    allow you to emit an error if any of the specified values is invalid.
    Otherwise, and especially with setting [last] to [true], some invalid
    values could be ignored. *)
val error: string -> unit

(** {2 Initialization} *)

(** Initialize command-line argument parsing.

    Usage: [init arguments]

    Reset parsing, starting from [arguments] as the list of remaining arguments.
    The description, current error and the set of arguments (used to produce
    the [--help]) is also reset.

    This is automatically called with [Sys.argv], converted to a list and without
    the first element (which is not an argument but the executable name).
    If you just want to parse actual command-line options, you thus do not need
    to call [init]. Call [init] yourself to override the list of arguments,
    or if you want to parse several lists of arguments, possibly in a different way. *)
val init: string list -> unit

(** {2 Help} *)

(** Output help as if [--help] was given.

    Only call this after all arguments have been, i.e. at the same place you
    would call [close]. Note that [close] automatically calls [help] if
    one of the remaining arguments is [--help].

    Output using [out], which is [prerr_string] by default.

    If [style] is [true], use ANSI escape codes.
    Default value is [true] if it appears that the output handles such codes. *)
val help: ?out: (string -> unit) -> ?style: bool -> unit -> unit
