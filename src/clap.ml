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

module Char_map = Map.Make (Char)
module String_map = Map.Make (String)

let list_iter x f = List.iter f x
let list_iteri x f = List.iteri f x

type argument =
  | Unnamed of string
  | Long of string
  | Short of char

let show_argument = function
  | Unnamed value -> value
  | Long name -> "--" ^ name
  | Short char -> "-" ^ String.make 1 char

let remaining: argument list ref = ref []

let global_description: string option ref = ref None

let description x = global_description := Some x

let first_error: string option ref = ref None

let error message =
  match !first_error with
    | None ->
        first_error := Some message
    | Some _ ->
        ()

type 'a typ =
  {
    type_name: string;
    dummy: 'a;
    parse: (string -> 'a option);
    show: ('a -> string);
  }

let typ ~name ~dummy ~parse ~show =
  { type_name = name; dummy; parse; show }

let enum (type a) ?(compare: a -> a -> int = Stdlib.compare) name = function
  | [] ->
      invalid_arg "Clap.enum: empty list of values"
  | ((_, (dummy: a)) :: _) as cases ->
      let by_key = ref String_map.empty in
      let module Value = struct type t = a let compare = compare end in
      let module Value_map = Map.Make (Value) in
      let by_value = ref Value_map.empty in
      (
        list_iter cases @@ fun (key, value) ->
        by_key := String_map.add key value !by_key;
        by_value := Value_map.add value key !by_value;
      );
      let parse (x: string) = String_map.find_opt x !by_key in
      let show (x: a) =
        match Value_map.find_opt x !by_value with
          | None ->
              "(unknown " ^ name ^ " value)"
          | Some key ->
              key
      in
      typ ~name ~dummy ~parse ~show

let string =
  {
    type_name = "string";
    dummy = "";
    parse = (fun x -> Some x);
    show = (fun x -> x);
  }

let int =
  {
    type_name = "int";
    dummy = 0;
    parse = int_of_string_opt;
    show = string_of_int;
  }

let float =
  {
    type_name = "float";
    dummy = 0.;
    parse = float_of_string_opt;
    show = string_of_float;
  }

type 'a parameter =
  {
    long: string list;
    short: char list;
    placeholder: string;
    description: string option;
    typ: 'a typ;
  }

type 'a case =
  {
    case_description: string option;
    case_command: string option;
    case_handler: (unit -> 'a);
  }

type spec =
  | Optional: { last: bool; parameter: 'a parameter } -> spec
  | Mandatory: { last: bool; parameter: 'a parameter } -> spec
  | Default: { last: bool; parameter: 'a parameter; default: 'a } -> spec
  | List: { parameter: 'a parameter } -> spec
  | Flag: {
      last: bool;
      set_long: string list;
      set_short: char list;
      unset_long: string list;
      unset_short: char list;
      description: string option;
      default: bool;
    } -> spec
  | Flag_enum: {
      last: bool;
      compare: 'a -> 'a -> int;
      description: string option;
      cases: (string list * char list * 'a) list;
      default: 'a;
    } -> spec
  | Subcommand: {
      cases: 'a case list;
      placeholder: string;
      value: string option;
    } -> spec

type section =
  {
    section_name: string;
    section_description: string;
    mutable section_specs_rev: spec list;
  }

let sections_rev: section list ref = ref []

let section ?(description = "") section_name =
  let section =
    {
      section_name;
      section_description = description;
      section_specs_rev = [];
    }
  in
  sections_rev := section :: !sections_rev;
  section

let options_section = section "OPTIONS"

let init args =
  let parse arg =
    let len = String.length arg in
    if len <= 0 then
      [ Unnamed "" ]
    else match arg.[0] with
      | '-' ->
          if len = 1 then
            [ Unnamed arg ]
          else (
            match arg.[1] with
              | '-' ->
                  [ Long (String.sub arg 2 (len - 2)) ]
              | '0'..'9' ->
                  [ Unnamed arg ]
              | _ ->
                  let rec make acc i =
                    if i < 1 then
                      acc
                    else
                      make (Short arg.[i] :: acc) (i - 1)
                  in
                  make [] (len - 1)
          )
      | '\\' ->
          [ Unnamed (String.sub arg 1 (len - 1)) ]
      | _ ->
          [ Unnamed arg ]
  in
  remaining := List.flatten (List.map parse args);
  options_section.section_specs_rev <- [];
  sections_rev := [ options_section ];
  first_error := None

let () =
  Sys.argv
  |> Array.to_list
  |> (function [] -> [] | _ :: tail -> tail)
  |> init

type action =
  | Continue (* Don't consume current argument, continue. *)
  | Take (* Consume current argument, add it to list, continue. *)
  | Take_next (* Consume current and next arguments, add the second one, continue. *)
  | Stop (* Don't consume current argument, stop. *)

type consume_result =
  {
    remainder: argument list;
    result: argument list;
  }

let rec consume (test: argument -> action) ~acc ~previous ~arguments =
  match arguments with
    | [] ->
        {
          remainder = List.rev previous;
          result = List.rev acc;
        }
    | head :: tail ->
        match test head with
          | Continue ->
              consume test ~acc ~previous: (head :: previous) ~arguments: tail
          | Take ->
              consume test ~acc: (head :: acc) ~previous ~arguments: tail
          | Take_next ->
              (
                match tail with
                  | [] ->
                      error ("missing value for " ^ show_argument head);
                      {
                        remainder = List.rev previous;
                        result = List.rev acc;
                      }
                  | next :: tail ->
                      consume test ~acc: (next :: acc) ~previous ~arguments: tail
              )
          | Stop ->
              {
                remainder = List.rev_append previous arguments;
                result = List.rev acc;
              }

let consume_in_place test =
  let consume_result = consume test ~acc: [] ~previous: [] ~arguments: !remaining in
  remaining := consume_result.remainder;
  consume_result.result

let consume_and_parse (type a) test (typ: a typ): a list =
  let parse arg: a =
    let arg = show_argument arg in
    match typ.parse arg with
      | None ->
          error ("invalid " ^ typ.type_name ^ ": " ^ arg);
          typ.dummy
      | Some value ->
          value
  in
  List.map parse (consume_in_place test)

let consume_and_parse_parameter all (type a) (parameter: a parameter): a list =
  let stop = ref false in
  let test arg =
    if !stop then Stop else
      match arg with
        | Unnamed _ when parameter.long = [] && parameter.short = [] ->
            if not all then stop := true;
            Take
        | Long name when List.mem name parameter.long ->
            if not all then stop := true;
            Take_next
        | Long _ when parameter.long = [] && parameter.short = [] ->
            Stop
        | Short name when List.mem name parameter.short ->
            if not all then stop := true;
            Take_next
        | Short _ when parameter.long = [] && parameter.short = [] ->
            Stop
        | _ ->
            Continue
  in
  consume_and_parse test parameter.typ

let get_last last parameter =
  match last with
    | Some last ->
        last
    | None ->
        match parameter.long, parameter.short with
          | [], [] ->
              false
          | _ ->
              true

let full_name parameter =
  match parameter.long, parameter.short with
    | [], [] ->
        parameter.placeholder
    | long :: _, _ ->
        "--" ^ long ^ " " ^ parameter.placeholder
    | [], short :: _ ->
        "-" ^ String.make 1 short ^ " " ^ parameter.placeholder

let make_parameter ?long ?(long_synonyms = []) ?short ?(short_synonyms = [])
    ?(placeholder = "VALUE") ?description typ =
  let long = match long with None -> long_synonyms | Some x -> x :: long_synonyms in
  let short = match short with None -> short_synonyms | Some x -> x :: short_synonyms in
  { long; short; placeholder; description; typ }

let optional (type a) (typ: a typ) ?(section = options_section) ?last
    ?long ?long_synonyms ?short ?short_synonyms
    ?placeholder ?description (): a option =
  let parameter =
    make_parameter ?long ?long_synonyms ?short ?short_synonyms ?placeholder ?description typ
  in
  let last = get_last last parameter in
  section.section_specs_rev <- Optional { last; parameter } :: section.section_specs_rev;
  match List.rev (consume_and_parse_parameter last parameter) with
    | [] ->
        None
    | head :: _ ->
        Some head

let optional_string = optional string
let optional_int = optional int
let optional_float = optional float

let mandatory (type a) (typ: a typ) ?(section = options_section) ?last
    ?long ?long_synonyms ?short ?short_synonyms
    ?placeholder ?description (): a =
  let parameter =
    make_parameter ?long ?long_synonyms ?short ?short_synonyms ?placeholder ?description typ
  in
  let last = get_last last parameter in
  section.section_specs_rev <- Mandatory { last; parameter } :: section.section_specs_rev;
  match List.rev (consume_and_parse_parameter last parameter) with
    | [] ->
        error ("missing argument " ^ full_name parameter);
        parameter.typ.dummy
    | head :: _ ->
        head

let mandatory_string = mandatory string
let mandatory_int = mandatory int
let mandatory_float = mandatory float

let default (type a) (typ: a typ) ?(section = options_section) ?last
    ?long ?long_synonyms ?short ?short_synonyms
    ?placeholder ?description default: a =
  let parameter =
    make_parameter ?long ?long_synonyms ?short ?short_synonyms ?placeholder ?description typ
  in
  let last = get_last last parameter in
  section.section_specs_rev <-
    Default { last; parameter; default } :: section.section_specs_rev;
  match List.rev (consume_and_parse_parameter last parameter) with
    | [] ->
        default
    | head :: _ ->
        head

let default_string = default string
let default_int = default int
let default_float = default float

let list (type a) (typ: a typ) ?(section = options_section)
    ?long ?long_synonyms ?short ?short_synonyms
    ?placeholder ?description (): a list =
  let parameter =
    make_parameter ?long ?long_synonyms ?short ?short_synonyms ?placeholder ?description typ
  in
  section.section_specs_rev <- List { parameter } :: section.section_specs_rev;
  consume_and_parse_parameter true parameter

let list_string = list string
let list_int = list int
let list_float = list float

let flag ?(section = options_section) ?(last = true)
    ?set_long ?(set_long_synonyms = []) ?set_short ?(set_short_synonyms = [])
    ?unset_long ?(unset_long_synonyms = []) ?unset_short ?(unset_short_synonyms = [])
    ?description default =
  let set_long =
    match set_long with None -> set_long_synonyms | Some x -> x :: set_long_synonyms
  in
  let set_short =
    match set_short with None -> set_short_synonyms | Some x -> x :: set_short_synonyms
  in
  let unset_long =
    match unset_long with None -> unset_long_synonyms | Some x -> x :: unset_long_synonyms
  in
  let unset_short =
    match unset_short with None -> unset_short_synonyms | Some x -> x :: unset_short_synonyms
  in
  section.section_specs_rev <-
    Flag { last; set_long; set_short; unset_long; unset_short; description; default }
    :: section.section_specs_rev;
  let stop = ref false in
  let test arg =
    if !stop then Stop else
      match arg with
        | Long name when List.mem name set_long || List.mem name unset_long ->
            if not last then stop := true;
            Take
        | Short name when List.mem name set_short || List.mem name unset_short ->
            if not last then stop := true;
            Take
        | _ ->
            Continue
  in
  match List.rev (consume_in_place test) with
    | [] ->
        default
    | Unnamed _ :: _ ->
        assert false (* bug in [test] or [consume_in_place] *)
    | Long name :: _ ->
        if List.mem name set_long then
          true
        else if List.mem name unset_long then
          false
        else
          assert false (* bug in [test] or [consume_in_place] *)
    | Short name :: _ ->
        if List.mem name set_short then
          true
        else if List.mem name unset_short then
          false
        else
          assert false (* bug in [test] or [consume_in_place] *)

let flag_enum ?(section = options_section) ?(last = true)
    ?(compare = Stdlib.compare) ?description cases default =
  section.section_specs_rev <-
    Flag_enum { last; compare; description; cases; default }
    :: section.section_specs_rev;
  let longs =
    let add acc (longs, _, value) =
      List.fold_left (fun acc long -> String_map.add long value acc) acc longs
    in
    List.fold_left add String_map.empty cases
  in
  let shorts =
    let add acc (_, shorts, value) =
      List.fold_left (fun acc short -> Char_map.add short value acc) acc shorts
    in
    List.fold_left add Char_map.empty cases
  in
  let stop = ref false in
  let test arg =
    if !stop then Stop else
      match arg with
        | Long name when String_map.mem name longs ->
            if not last then stop := true;
            Take
        | Short name when Char_map.mem name shorts ->
            if not last then stop := true;
            Take
        | _ ->
            Continue
  in
  match List.rev (consume_in_place test) with
    | [] ->
        default
    | Unnamed _ :: _ ->
        assert false (* bug in [test] or [consume_in_place] *)
    | Long name :: _ -> (
        match String_map.find_opt name longs with
          | None ->
              assert false (* bug in [test] or [consume_in_place] *)
          | Some value ->
              value
      )
    | Short name :: _ -> (
        match Char_map.find_opt name shorts with
          | None ->
              assert false (* bug in [test] or [consume_in_place] *)
          | Some value ->
              value
      )

let case ?description command handler =
  {
    case_description = description;
    case_command = Some command;
    case_handler = handler;
  }

let default_case ?description handler =
  {
    case_description = description;
    case_command = None;
    case_handler = handler;
  }

module type TEXT_OUTPUT =
sig
  val out: string -> unit
  type style
  val style: style -> unit
  val reset_style: style -> unit
end

module Text (X: TEXT_OUTPUT):
sig
  (** Functions to output indented text.
      Made into a functor in order to be thread-safe. *)

  (** Call [indent], then a function, then [dedent]. *)
  val with_indentation: int -> (unit -> unit) -> unit

  (** Output a new line character.

      But only if needed (no new line after a new line). *)
  val new_line: unit -> unit

  (** Output two new line characters.

      But only if needed (no new line after we already output two
      new lines, and only one new line after a single consecutive new line;
      and wait to be sure there is actually a word to output before outputing
      the second new line). *)
  val paragraph: unit -> unit

  (** Output a word, separated from other words by a breakable space. *)
  val word: ?style: X.style -> string -> unit
end =
struct

  let max_line_length = 80

  type position =
    | Beginning_of_text
    | Beginning_of_line
    | Beginning_of_line_with_pending_paragraph
    | After_word of int (* int = length of current line *)

  type state =
    {
      mutable indentation_level: int;
      mutable position: position;
    }

  let state =
    {
      indentation_level = 0;
      position = Beginning_of_text;
    }

  let indent n =
    state.indentation_level <- state.indentation_level + n

  let dedent n =
    indent (- n)

  let with_indentation n f =
    indent n;
    f ();
    dedent n

  let new_line () =
    match state.position with
      | Beginning_of_text | Beginning_of_line | Beginning_of_line_with_pending_paragraph ->
          ()
      | After_word _ ->
          (* Indentation will be printed by [word] since we don't want to output
             spaces in empty lines. *)
          X.out "\n";
          state.position <- Beginning_of_line

  let paragraph () =
    match state.position with
      | Beginning_of_text | Beginning_of_line_with_pending_paragraph ->
          ()
      | Beginning_of_line ->
          state.position <- Beginning_of_line_with_pending_paragraph
      | After_word _ ->
          X.out "\n";
          state.position <- Beginning_of_line_with_pending_paragraph

  let output_style = function
    | None -> ()
    | Some style -> X.style style

  let reset_style = function
    | None -> ()
    | Some style -> X.reset_style style

  let output_word_assuming_beginning_of_line style s =
    let level = max 0 state.indentation_level in
    X.out (String.make level ' ');
    output_style style;
    X.out s;
    reset_style style;
    state.position <- After_word (level + String.length s)

  let word ?style s =
    let slen = String.length s in
    if slen > 0 then
      match state.position with
        | Beginning_of_text | Beginning_of_line ->
            output_word_assuming_beginning_of_line style s
        | Beginning_of_line_with_pending_paragraph ->
            X.out "\n";
            output_word_assuming_beginning_of_line style s
        | After_word position ->
            let new_position = position + 1 + slen in
            if new_position > max_line_length then
              (
                X.out "\n";
                output_word_assuming_beginning_of_line style s;
              )
            else
              (
                X.out " ";
                output_style style;
                X.out s;
                reset_style style;
                state.position <- After_word new_position;
              )

end

let help ?(out = prerr_string) ?(style = Sys.getenv_opt "TERM" <> Some "dumb") () =
  let spec_is_optional = function
    | Optional _ | Default _ | List _ | Flag _ | Flag_enum _ -> true
    | Mandatory _ -> false
    | Subcommand { cases; _ } ->
        List.exists (function { case_command = None; _ } -> true | _ -> false) cases
  in
  let spec_is_list = function
    | List _ -> true
    | Optional _ | Default _ | Flag _ | Flag_enum _ | Mandatory _ | Subcommand _ -> false
  in
  let name_list long short =
    List.map (fun x -> "--" ^ x) long @
    List.map (fun x -> "-" ^ String.make 1 x) short
  in
  let names_of_parameter { long; short; _ } = name_list long short in
  let names_of_spec = function
    | Optional { parameter; _ } -> names_of_parameter parameter
    | Mandatory { parameter; _ } -> names_of_parameter parameter
    | Default { parameter; _ } -> names_of_parameter parameter
    | List { parameter } -> names_of_parameter parameter
    | Flag { set_long; set_short; unset_long; unset_short; _ } ->
        name_list set_long set_short @ name_list unset_long unset_short
    | Flag_enum { cases; _ } ->
        List.flatten (List.map (fun (longs, shorts, _) -> name_list longs shorts) cases)
    | Subcommand _ -> []
  in
  let placeholder_of_spec = function
    | Optional { parameter; _ } -> Some parameter.placeholder
    | Mandatory { parameter; _ } -> Some parameter.placeholder
    | Default { parameter; _ } -> Some parameter.placeholder
    | List { parameter } -> Some parameter.placeholder
    | Flag _ | Flag_enum _ -> None
    | Subcommand { placeholder; _ } -> Some placeholder
  in
  let description_of_spec = function
    | Optional { parameter; _ } -> parameter.description
    | Mandatory { parameter; _ } -> parameter.description
    | Default { parameter; _ } -> parameter.description
    | List { parameter } -> parameter.description
    | Flag { description; _ } -> description
    | Flag_enum { description; _ } -> description
    | Subcommand _ -> None
  in
  let sections = List.rev !sections_rev in
  let all_specs =
    List.map (fun sec -> List.rev sec.section_specs_rev) sections
    |> List.flatten
  in
  let bold = "\027[1m" in
  let module Output =
  struct
    let out = out
    type style = string
    let reset_style _ = if style then out "\027[0m"
    let style s = if style then out s else ()
  end
  in
  let module Text = Text (Output) in
  let section name f =
    Text.paragraph ();
    Text.word ~style: bold name;
    Text.paragraph ();
    Text.with_indentation 4 f;
    Text.paragraph ();
  in
  let rich_text s =
    list_iter (String.split_on_char '\n' s) @@ fun line ->
    if line = "" then
      Text.paragraph ()
    else (
      Text.new_line ();
      String.split_on_char ' ' line |> List.filter ((<>) "") |> List.iter Text.word
    )
  in
  (
    section "SYNOPSIS" @@ fun () ->
    Text.word (Filename.basename Sys.executable_name);
    Text.with_indentation 4 @@ fun () ->
    list_iter all_specs @@ fun spec ->
    match spec with
      | Subcommand { value = Some name; _ } ->
          (* Commands which are already specified are replaced by their value. *)
          Text.word name
      | _ ->
          let optional = spec_is_optional spec in
          let names = names_of_spec spec in
          Text.word @@ String.concat "" @@ [
            if optional then "[" else "";
            (
              (* There are several cases to avoid useless parentheses and spaces. *)
              match names, placeholder_of_spec spec with
                | [], None -> "???"
                | [ name ], None -> name
                | names, None ->
                    (if optional then "" else "[") ^
                    String.concat "|" names ^
                    (if optional then "" else "]")
                | [], Some name -> "<" ^ name ^ ">"
                | [ name ], Some pname -> name ^ " <" ^ pname ^ ">"
                | names, Some pname -> "(" ^ String.concat "|" names ^ ") <" ^ pname ^ ">"
            );
            if spec_is_list spec then "..." else "";
            if optional then "]" else "";
          ]
  );
  (
    (* "DESCRIPTION" section, shown if:
       - a subcommand has been selected and it has a description;
       - no subcommand has been selected and there is a global description.
         If there are several subcommands, we consider the last one. *)
    let description =
      let rec find_subcommand_with_value acc = function
        | [] ->
            acc
        | Subcommand { value = (Some _ as value); cases; _ } :: tail ->
            (
              match List.find_opt (fun case -> case.case_command = value) cases with
                | None ->
                    find_subcommand_with_value acc tail
                | Some { case_description; _ } ->
                    find_subcommand_with_value case_description tail
            )
        | _ :: tail ->
            find_subcommand_with_value acc tail
      in
      match find_subcommand_with_value None all_specs with
        | Some _ as x ->
            x
        | None ->
            match !global_description with
              | None ->
                  None
              | Some _ as x ->
                  x
    in
    match description with
      | None ->
          ()
      | Some description ->
          section "DESCRIPTION" @@ fun () ->
          rich_text description
  );
  (
    (* Used for sorting only. *)
    let full_name_of_spec = function
      | Optional x ->
          full_name x.parameter
      | Mandatory x ->
          full_name x.parameter
      | Default x ->
          full_name x.parameter
      | List x ->
          full_name x.parameter
      | Flag x -> (
          match x.set_long, x.set_short, x.unset_long, x.unset_short with
            | [], [], [], [] ->
                "???"
            | name :: _, _, _, _ ->
                "--" ^ name
            | [], name :: _, _, _ ->
                "-" ^ String.make 1 name
            | [], _, name :: _, _ ->
                "--" ^ name
            | [], [], [], name :: _ ->
                "-" ^ String.make 1 name
        )
      | Flag_enum x -> (
          match x.cases with
            | [] ->
                "???"
            | ([], [], _) :: _ ->
                "???"
            | (name :: _, _, _) :: _ ->
                "--" ^ name
            | (_, name :: _, _) :: _ ->
                "-" ^ String.make 1 name
        )
      | Subcommand x ->
          (* Unused since we put subcommands in their own section. *)
          x.placeholder
    in
    let by_full_name a b = String.compare (full_name_of_spec a) (full_name_of_spec b) in
    list_iter sections @@ fun sec ->
    if sec.section_description <> "" || sec.section_specs_rev <> [] then
      section sec.section_name @@ fun () ->
      rich_text sec.section_description;
      let section_specs = List.rev sec.section_specs_rev in
      list_iter (List.sort by_full_name section_specs) @@ fun spec ->
      match spec with
        | Subcommand _ ->
            (* Subcommands are described in their own section. *)
            ()
        | _ ->
            let names = names_of_spec spec in
            let placeholder = placeholder_of_spec spec in
            let name_count = List.length names in
            Text.paragraph ();
            (
              match names with
                | [] ->
                    (* Unnamed argument. *)
                    Text.word (
                      match placeholder with
                        | None -> "???"
                        | Some name -> name
                    )
                | _ ->
                    (
                      list_iteri names @@ fun i name ->
                      Text.word (
                        name ^
                        (
                          match placeholder with
                            | None -> ""
                            | Some pname -> " " ^ pname
                        ) ^
                        (if i < name_count - 1 then "," else "")
                      )
                    )
            );
            (
              (* Show default value, if any. *)
              match spec with
                | Optional _ | Mandatory _ | List _ | Subcommand _ ->
                    ()
                | Default { parameter; default; _ } ->
                    Text.word ("(default: " ^ parameter.typ.show default ^ ")")
                | Flag { set_long; set_short; unset_long; unset_short; default; _ } -> (
                    let long, short =
                      if default then set_long, set_short else unset_long, unset_short
                    in
                    match long, short with
                      | [], [] ->
                          ()
                      | name :: _, _ ->
                          Text.word ("(default: --" ^ name ^ ")")
                      | [], name :: _ ->
                          Text.word ("(default: -" ^ String.make 1 name ^ ")")
                  )
                | Flag_enum { compare = cmp; cases; default; _ } ->
                    let rec find_default = function
                      | [] ->
                          ()
                      | (longs, shorts, value) :: tail ->
                          if cmp value default = 0 then
                            match longs, shorts with
                              | [], [] ->
                                  ()
                              | name :: _, _ ->
                                  Text.word ("(default: --" ^ name ^ ")")
                              | _, name :: _ ->
                                  Text.word ("(default: -" ^ String.make 1 name ^ ")")
                          else
                            find_default tail
                    in
                    find_default cases
            );
            (
              match description_of_spec spec with
                | None ->
                    ()
                | Some description ->
                    Text.new_line ();
                    Text.with_indentation 4 @@ fun () ->
                    rich_text description
            );
            Text.paragraph ()
  );
  (
    let rec find_subcommand_without_value = function
      | [] ->
          ()
      | Subcommand { value = None; cases; placeholder } :: _ ->
          (
            section placeholder @@ fun () ->
            list_iter cases @@ fun { case_description; case_command; _ } ->
            Text.paragraph ();
            let command =
              match case_command with
                | None -> "(default)"
                | Some command -> command
            in
            Text.word command;
            Text.new_line ();
            Text.with_indentation 4 @@ fun () ->
            match case_description with
              | None -> ()
              | Some description -> rich_text description
          )
      | _ :: tail ->
          find_subcommand_without_value tail
    in
    find_subcommand_without_value all_specs
  );
  ()

let close
    ?(on_help = fun () -> help (); exit 0)
    ?(on_error = fun message ->
        help ();
        prerr_newline ();
        prerr_string "Error: ";
        prerr_endline message;
        exit 1)
    () =
  if List.exists (function Long "help" -> true | _ -> false) !remaining then on_help ();
  (
    match !remaining with
      | [] ->
          ()
      | head :: _ ->
          error ("don't know what to do with: " ^ show_argument head)
  );
  match !first_error with
    | None ->
        ()
    | Some message ->
        on_error message

(* [never_returns] is inhabited, but only from the point of view of outside the module.
   Here, we have it equal to [unit] so that [subcommand] can call [close]. *)
type never_returns = unit

let subcommand (type a) ?on_help ?on_error ?(placeholder = "COMMAND")
    (cases: a case list): a =
  let parameter =
    { long = []; short = []; placeholder; description = None; typ = string }
  in
  match List.rev (consume_and_parse_parameter false parameter) with
    | [] ->
        options_section.section_specs_rev <-
          Subcommand { cases; placeholder; value = None }
          :: options_section.section_specs_rev;
        (
          match
            List.find_opt
              (fun case -> match case.case_command with None -> true | Some _ -> false)
              cases
          with
            | None ->
                error ("missing argument " ^ placeholder);
                close ?on_help ?on_error ();
                assert false (* since there is an error, [close] does not return *)
            | Some case ->
                case.case_handler ()
        )
    | head :: _ ->
        match
          List.find_opt
            (fun case -> match case.case_command with None -> false | Some c -> c = head)
            cases
        with
          | None ->
              options_section.section_specs_rev <-
                Subcommand { cases; placeholder; value = None }
                :: options_section.section_specs_rev;
              error ("invalid command: " ^ head);
              close ?on_help ?on_error ();
              assert false (* since there is an error, [close] does not return *)
          | Some case ->
              options_section.section_specs_rev <-
                Subcommand { cases; placeholder; value = Some head }
                :: options_section.section_specs_rev;
              case.case_handler ()
