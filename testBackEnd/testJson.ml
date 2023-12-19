(* #load "str.cma";;

let iterate_through_words line =
  let words = Str.split (Str.regexp "\\b") line in
  List.iter (fun word -> Printf.printf "%s\n" word) words

let iterate_through_lines s =
  let lines = Str.split (Str.regexp "\n") s in
  List.iter iterate_through_words lines

let () =
  let input_string = "let _ =\n let x = 1 in\n let y = 0 in\n let z = add x y" in
  iterate_through_lines input_string *)

#use "topfind";;
#require "yojson";;

open Yojson.Basic

let convert_asml_to_json asml_string =
  `Assoc [("asml", `String asml_string)]

let extract_asml_from_json json =
  match json with
  | `Assoc fields ->
    begin
      match List.assoc_opt "asml" fields with
      | Some (`String raw_string) -> Some raw_string
      | _ -> None
    end
  | _ -> None

let () =
  let my_raw_string = "let _ =\n let x = 1 in\n let y = 0 in\n let z = add x y" in
  let my_json = convert_asml_to_json my_raw_string in
  Printf.printf "Original JSON:\n%s\n" (Yojson.Basic.pretty_to_string my_json);

  match extract_asml_from_json my_json with
  | Some raw_string -> Printf.printf "\nExtracted Raw String: %s\n" raw_string
  | None -> Printf.printf "\nNo raw string found in the JSON.\n"

