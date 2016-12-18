open Str

type day_part =
  | Part1 | Part2;;

let rec repeat_string to_repeat times ?(result="") count =
  match times with
    | n when n < count -> result
    | _ ->
        let new_result = String.concat "" [result; to_repeat] in
        repeat_string to_repeat times ~result:new_result (count + 1)
;;

let rec decompress original part =
  let rec decompress_internal length original =
    match String.contains original '(' with
      | false -> length + String.length original
      | true ->
          let marker_begin = (String.index original '(') + 1 in
          let marker_end = (String.index_from original marker_begin ')') - 1 in
          let repeat_ins = String.sub original marker_begin (marker_end - marker_begin + 1) in
          let split_repeat = Str.split (Str.regexp "x") repeat_ins in
          match split_repeat with
            | num :: times :: [] ->
                let to_repeat = String.sub original (marker_end + 2) (int_of_string num) in
                let sub = repeat_string to_repeat (int_of_string times) 1 in
                let res = String.concat "" [(String.sub original 0 (marker_begin - 1)); sub] in
                let len = (String.length original) - marker_end - (int_of_string num) in
                let rest = String.sub original (marker_end + (int_of_string num) + 2) (len - 2) in
                (match part with
                  | Part1 -> decompress_internal (length + (String.length res)) rest
                  | Part2 ->
                      let all_rest = String.concat "" [res; rest] in
                      (match String.contains all_rest '(' with
                        | false -> length + (String.length all_rest)
                        | true ->
                            let next_repeat = String.index all_rest '(' in
                            let before = String.sub all_rest 0 next_repeat in
                            let after = String.sub all_rest next_repeat ((String.length all_rest) - next_repeat) in
                            decompress_internal (length + (String.length before)) after
                      )
                )
            | _ -> raise Not_found
  in decompress_internal 0 original
;;

let input = read_line () in
let output = decompress input in
Printf.printf "\nPart 1: %d\n" (output Part1);
(*Part 2 implementation is SLOW*)
Printf.printf "\nPart 2: %d\n" (output Part2);
