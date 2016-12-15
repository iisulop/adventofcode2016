open Str

let rec repeat_string to_repeat times ?(result="") count =
  match times with
    | n when n < count ->
      result
    | _ ->
      let new_result = String.concat "" [result; to_repeat] in
        repeat_string to_repeat times ~result:new_result (count + 1)
    ;;

let rec decompress ?(decompressed="") original =
  try
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
                        decompress ~decompressed:(String.concat "" [decompressed; res]) rest
              | _ -> raise Not_found
  with
    (* String.index will raise a Not_found if it cannot find '(' or ')' *)
    Not_found -> String.concat "" [decompressed; original];;

let input = read_line () in
  Printf.printf "%s\n" (decompress input)
