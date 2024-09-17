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

module type my_game = sig
  type t = int array array

  val initialize_game : int -> int -> t
  val check_lost : t -> bool
end

module Play_Game (Game_Board : my_board) : my_game with type t = Game_Board.t =
struct
  type t = Game_Board.t

  let game_types = [ [ 2; 4; 2048 ]; [ 1; 2; 1024 ]; [ 2; 4; 65536 ] ]

  (**[generate_cell] generates a random 2 or 4

     @return a 2 or a 4*)
  let generate_cell game_type =
    let spawnable_nums = List.nth game_types game_type in
    let () = Random.self_init () in
    let generate_num () = 0 + Random.int 9 in
    let rand_num = generate_num () in
    if rand_num = 4 then List.nth spawnable_nums 0
    else List.nth spawnable_nums 1

  let initialize_game game_type board_size =
    let board = Game_Board.make_board board_size 0 in
    let (cell_1a, cell_1b), (cell_2a, cell_2b) =
      Game_Board.pick_2_empty board
    in
    let () =
      Game_Board.set_value cell_1a cell_1b (generate_cell game_type) board
    in
    let () =
      Game_Board.set_value cell_2a cell_2b (generate_cell game_type) board
    in
    let () = Game_Board.set_spawnable_values (List.nth game_types game_type) in
    board

  let check_lost board = Game_Board.check_lost board
end
