module type my_board = sig
  type t = int array array

  val score : int ref
  val make_board : int -> int -> t
  val set_spawnable_values : int list -> unit
  val get_score : unit -> int
  val to_list : t -> int list list
  val get_value : int -> int -> t -> int
  val set_value : int -> int -> int -> t -> unit
  val empty_cells : t -> (int * int) list
  val pick_1_empty : t -> int * int
  val pick_2_empty : t -> (int * int) * (int * int)
  val generate_tile : t -> t
  val check_move : t -> char -> bool
  val check_lost : t -> bool
  val check_won : t -> bool
  val do_move : t -> char -> t
end

module Board_2048 : my_board = struct
  type t = int array array

  let score = ref 0
  let spawnable_values = ref []
  let make_board n elem = Array.make_matrix n n elem
  let set_spawnable_values lst = spawnable_values := lst
  let get_score () = !score

  let rec to_list_helper board lst =
    match board with
    | [] -> lst
    | h :: t ->
        let new_lst = lst @ [ Array.to_list h ] in
        to_list_helper t new_lst

  let to_list board =
    let board_list = Array.to_list board in
    to_list_helper board_list []

  let get_value x y board = board.(x).(y)
  let set_value x y value board = board.(x).(y) <- value

  (*Adapted from
    https://chat.openai.com/share/26b93a4c-91d0-48e8-9e78-91c2fb213294, accessed
    3/27/2024*)
  let empty_cells matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let zero_indices = ref [] in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if matrix.(i).(j) = 0 then zero_indices := (i, j) :: !zero_indices
      done
    done;
    !zero_indices

  let pick_1_empty matrix =
    let empty_lst = empty_cells matrix in
    let () = Random.self_init () in
    let generate_num () = 0 + Random.int (List.length empty_lst) in
    let index = generate_num () in
    let choice = List.nth empty_lst index in
    choice

  let rec pick_2_empty matrix =
    let choice_1 = pick_1_empty matrix in
    let choice_2 = pick_1_empty matrix in
    if choice_1 = choice_2 then pick_2_empty matrix else (choice_1, choice_2)

  let generate_tile matrix =
    let empty_cells = empty_cells matrix in
    match empty_cells with
    | [] -> matrix (* No empty cell available, return the matrix unchanged *)
    | _ ->
        let () = Random.self_init () in
        let generate_num () = Random.int 3 in
        let value =
          if generate_num () < 2 then List.nth !spawnable_values 0
          else List.nth !spawnable_values 1
        in
        let row, col =
          List.nth empty_cells (Random.int (List.length empty_cells))
        in
        set_value row col value matrix;
        matrix

  (**[left_valid] returns true if it is valid to move left, false otherwise
     @param matrix is the board
     @return a boolean*)
  let left_valid matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let valid = ref false in
    for r = 0 to rows - 1 do
      for c = cols - 1 downto 1 do
        let current = matrix.(r).(c) in
        let next_one = matrix.(r).(c - 1) in
        if current = next_one || next_one = 0 then valid := true
      done
    done;

    !valid

  (**[right_valid] returns true if it is valid to move right, false otherwise
     @param matrix is the board
     @return a boolean*)
  let right_valid matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let valid = ref false in
    for r = 0 to rows - 1 do
      for c = 0 to cols - 2 do
        let current = matrix.(r).(c) in
        let next_one = matrix.(r).(c + 1) in
        if current = next_one || next_one = 0 then valid := true
      done
    done;

    !valid

  (**[up_valid] returns true if it is valid to move up, false otherwise
     @param matrix is the board
     @return a boolean*)
  let up_valid matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let valid = ref false in
    for c = 0 to cols - 1 do
      for r = rows - 1 downto 1 do
        let current = matrix.(r).(c) in
        let next_one = matrix.(r - 1).(c) in
        if current = next_one || next_one = 0 then valid := true
      done
    done;

    !valid

  (**[down_valid] returns true if it is valid to move down, false otherwise
     @param matrix is the board
     @return a boolean*)
  let down_valid matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let valid = ref false in
    for c = 0 to cols - 1 do
      for r = 0 to rows - 2 do
        let current = matrix.(r).(c) in
        let next_one = matrix.(r + 1).(c) in
        if current = next_one || next_one = 0 then valid := true
      done
    done;

    !valid

  let check_move matrix move =
    match Char.lowercase_ascii move with
    | 'a' -> left_valid matrix
    | 'd' -> right_valid matrix
    | 'w' -> up_valid matrix
    | 's' -> down_valid matrix
    | _ -> false

  let check_lost matrix =
    if
      (not (check_move matrix 'w'))
      && (not (check_move matrix 'a'))
      && (not (check_move matrix 's'))
      && not (check_move matrix 'd')
    then true
    else false

  let check_won matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let won = ref false in
    for c = 0 to cols - 1 do
      for r = 0 to rows - 1 do
        if matrix.(r).(c) = List.nth !spawnable_values 2 then won := true
      done
    done;
    !won

  let move_right_fst_shift matrix rows cols new_matrix =
    for r = 0 to rows - 1 do
      let idx = ref (cols - 1) in
      for c = cols - 1 downto 0 do
        if matrix.(r).(c) <> 0 then begin
          new_matrix.(r).(!idx) <- matrix.(r).(c);
          idx := !idx - 1
        end
      done
    done

  let move_right_merge rows cols new_matrix =
    for r = 0 to rows - 1 do
      for c = cols - 1 downto 1 do
        if new_matrix.(r).(c) = new_matrix.(r).(c - 1) then begin
          score := !score + new_matrix.(r).(c);
          new_matrix.(r).(c) <- new_matrix.(r).(c) * 2;
          new_matrix.(r).(c - 1) <- 0
        end
      done
    done

  let move_right_snd_shift rows cols new_matrix =
    for r = 0 to rows - 1 do
      let idx = ref (cols - 1) in
      for c = cols - 1 downto 0 do
        if new_matrix.(r).(c) <> 0 then begin
          new_matrix.(r).(!idx) <- new_matrix.(r).(c);
          if !idx <> c then new_matrix.(r).(c) <- 0;
          (* Clear the original position if shifted *)
          idx := !idx - 1
        end
      done
    done

  let move_right matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let new_matrix = Array.make_matrix rows cols 0 in

    (* Shift non-zero elements to the right *)
    move_right_fst_shift matrix rows cols new_matrix;

    (* Merge adjacent equal elements *)
    move_right_merge rows cols new_matrix;

    (* Shift non-zero elements to the right again *)
    move_right_snd_shift rows cols new_matrix;
    new_matrix

  let move_left_fst_shift matrix rows cols new_matrix =
    for r = 0 to rows - 1 do
      let idx = ref 0 in
      for c = 0 to cols - 1 do
        if matrix.(r).(c) <> 0 then begin
          new_matrix.(r).(!idx) <- matrix.(r).(c);
          idx := !idx + 1
        end
      done
    done

  let move_left_merge rows cols new_matrix =
    for r = 0 to rows - 1 do
      for c = 0 to cols - 2 do
        if new_matrix.(r).(c) = new_matrix.(r).(c + 1) then begin
          score := !score + new_matrix.(r).(c);
          new_matrix.(r).(c) <- new_matrix.(r).(c) * 2;
          new_matrix.(r).(c + 1) <- 0
        end
      done
    done

  let move_left_snd_shift rows cols new_matrix =
    for r = 0 to rows - 1 do
      let idx = ref 0 in
      for c = 0 to cols - 1 do
        if new_matrix.(r).(c) <> 0 then begin
          new_matrix.(r).(!idx) <- new_matrix.(r).(c);
          if !idx <> c then new_matrix.(r).(c) <- 0;
          idx := !idx + 1
        end
      done
    done

  let move_left matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let new_matrix = Array.make_matrix rows cols 0 in

    (* Shift non-zero elements to the left *)
    move_left_fst_shift matrix rows cols new_matrix;

    (* Merge adjacent equal elements *)
    move_left_merge rows cols new_matrix;

    (* Shift non-zero elements to the left again *)
    move_left_snd_shift rows cols new_matrix;
    new_matrix

  let move_up_fst_shift matrix rows cols new_matrix =
    for c = 0 to cols - 1 do
      let idx = ref 0 in
      for r = 0 to rows - 1 do
        if matrix.(r).(c) <> 0 then begin
          new_matrix.(!idx).(c) <- matrix.(r).(c);
          idx := !idx + 1
        end
      done
    done

  let move_up_merge rows cols new_matrix =
    for c = 0 to cols - 1 do
      for r = 0 to rows - 2 do
        if new_matrix.(r).(c) = new_matrix.(r + 1).(c) then begin
          score := !score + new_matrix.(r).(c);
          new_matrix.(r).(c) <- new_matrix.(r).(c) * 2;
          new_matrix.(r + 1).(c) <- 0
        end
      done
    done

  let move_up_snd_shift rows cols new_matrix =
    for c = 0 to cols - 1 do
      let idx = ref 0 in
      for r = 0 to rows - 1 do
        if new_matrix.(r).(c) <> 0 then begin
          new_matrix.(!idx).(c) <- new_matrix.(r).(c);
          if !idx <> r then new_matrix.(r).(c) <- 0;
          idx := !idx + 1
        end
      done
    done

  let move_up matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let new_matrix = Array.make_matrix rows cols 0 in

    (* Shift non-zero elements upwards *)
    move_up_fst_shift matrix rows cols new_matrix;

    (* Merge adjacent equal elements *)
    move_up_merge rows cols new_matrix;

    (* Shift non-zero elements upwards again *)
    move_up_snd_shift rows cols new_matrix;
    new_matrix

  let move_down_fst_shift matrix rows cols new_matrix =
    for c = 0 to cols - 1 do
      let idx = ref (rows - 1) in
      for r = rows - 1 downto 0 do
        if matrix.(r).(c) <> 0 then begin
          new_matrix.(!idx).(c) <- matrix.(r).(c);
          idx := !idx - 1
        end
      done
    done

  let move_down_merge rows cols new_matrix =
    for c = 0 to cols - 1 do
      for r = rows - 1 downto 1 do
        if new_matrix.(r).(c) = new_matrix.(r - 1).(c) then begin
          score := !score + new_matrix.(r).(c);
          new_matrix.(r).(c) <- new_matrix.(r).(c) * 2;
          new_matrix.(r - 1).(c) <- 0
        end
      done
    done

  let move_down_snd_shift rows cols new_matrix =
    for c = 0 to cols - 1 do
      let idx = ref (rows - 1) in
      for r = rows - 1 downto 0 do
        if new_matrix.(r).(c) <> 0 then begin
          new_matrix.(!idx).(c) <- new_matrix.(r).(c);
          if !idx <> r then new_matrix.(r).(c) <- 0;
          idx := !idx - 1
        end
      done
    done

  let move_down matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let new_matrix = Array.make_matrix rows cols 0 in

    (* Shift non-zero elements downwards *)
    move_down_fst_shift matrix rows cols new_matrix;

    (* Merge adjacent equal elements *)
    move_down_merge rows cols new_matrix;

    (* Shift non-zero elements downwards again *)
    move_down_snd_shift rows cols new_matrix;

    new_matrix

  let do_move matrix move =
    match Char.lowercase_ascii move with
    | 'a' -> move_left matrix
    | 'd' -> move_right matrix
    | 'w' -> move_up matrix
    | 's' -> move_down matrix
    | _ -> matrix
end
