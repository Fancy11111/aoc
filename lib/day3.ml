open Core;;

(* type pos = (int * int) *)
(* type part = (char * pos) *)
(* part_number (y, x, len, val) *)
(* type part_number = (int * int * int * int);; *)

let is_dot = function
    | '.' -> true
    | _ -> false

let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

let get_parts lines = 
    List.foldi lines ~init:[] ~f:(fun x _ps l -> 
        String.foldi l ~init:_ps ~f:(fun y ps v ->
            if (not (is_dot v) && not (is_digit v)) then List.append ps [(v, (x,y))]
            else ps
        )
    )

let get_numbers _y line =
    let nums = String.foldi ~init:([], (_y, -1, -1, 0)) ~f:(fun i (ns, c) cur ->
        if not @@ is_digit @@ cur then 
            match c with
            | (_, -1,_,_) -> (ns, c)
            | _ -> (c :: ns, (_y, -1,-1,0))
        else
            let (y, x, len, v) = c in
            let x = if x = -1 then i else x in
            (ns, (y, x, len+1, v*10 + int_of_string (String.make 1 cur)))
    ) line in
    match nums with 
    | (ns, (_,-1,-1,_)) -> ns
    | (ns, num) -> num :: ns

let get_all_numbers lines = 
    List.foldi ~init:[] ~f:(fun i ns line -> 
        List.append ns (get_numbers i line)
    ) lines

(* let group_parts ps =  *)
(*     let parts : char list = List.map ps ~f:(fun (p, _) -> p) in *)
(*     Set.to_map (Char.Set.of_list parts) ~f:(fun p -> List.fold ps ~init:[] ~f:(fun acc c -> *)
(*         match c with  *)
(*         | (_p, pos) when Char.equal _p p  -> pos :: acc *)
(*         | _ -> acc *)
(*     )) *)

let number_touches px x_start x_end =
    let (px_start, px_end) = (px-1, px+1) in
    (x_start <= px_start && x_end >= px_start) || (x_start <= px_end && x_end >= px_end) || (x_start >= px_start && x_end <= px_end) || (x_start <= px_start && x_end >= px_end)
    (* (x_start >= px_start && x_end <= px_end) || (x_start <= px_start && x_end >= px_end) *)

(* let part_values ps ns =  *)
(*     Map.map ps ~f:(fun pos_list ->  *)
(*         List.fold ~init:0 pos_list ~f:(fun acc (py,px) ->  *)
(*             let (py_start, py_end) = (py-1, py+1) in *)
(*             let touches_p = number_touches px in *)
(*             acc + List.fold ns ~init:0 ~f:(fun acc (y,x,len,v) -> *)
(*                 let touches = py_start <= y && py_end >= y && touches_p (x) (x+len) in *)
(*                 (* let _ = Format.printf "num: x,y,len: %d %d %d %b\n" x y len (touches) in *) *)
(*                 if (touches) then acc + v *)
(*                 else acc *)
(*             ) *)
(*         ) *)
(*     ) *)

let group_parts_numbers ps ns =
    List.map ps ~f:(fun p -> 
        let (_, (py,px)) = p in
        let (py_start, py_end) = (py-1, py+1) in
        let touches_p = number_touches px in
        (p, List.fold ns ~init:[] ~f:(fun acc (y,x,len,v) -> 
            let touches = py_start <= y && py_end >= y && touches_p (x) (x+len) in
            if touches then v :: acc
            else acc
        ))
    )

(* let v1 lines =  *)
(*     let parts = group_parts @@ (get_parts lines) in  *)
(*     let numbers = get_all_numbers lines in *)
(*     (* let _ = List.iter parts ~f:(fun (p, (x, y)) -> Format.printf "%c %d %d\n" p x y) in *) *)
(*     let _ = List.iter numbers ~f:(fun (y,x, len, v) -> Format.printf "%d %d %d %d\n" x y len v) in *)
(*     let part_vals = part_values parts numbers in  *)
(*     let sum = Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc+data) part_vals in *)
(*     Format.sprintf "%d\n" sum *)

let v1 lines =
    let parts = get_parts lines in 
    let numbers = get_all_numbers lines in 
    let parts_numbers = group_parts_numbers parts numbers in 
    let sum = List.fold parts_numbers ~init:0 ~f:(fun acc (_, ns) -> 
        List.fold ns ~init:acc ~f:(fun _acc _v -> _acc+_v)
    ) in
    Format.sprintf "%d\n" sum


let v2 lines =
    let parts = get_parts lines in 
    let numbers = get_all_numbers lines in 
    let parts_numbers = group_parts_numbers parts numbers in 
    let sum = List.fold parts_numbers ~init:0 ~f:(fun acc (_, ns) -> 
        if List.length ns = 2 then acc + List.fold ns ~init:1 ~f:(fun _acc _v -> _acc*_v)
        else acc
    ) in
    Format.sprintf "%d\n" sum
