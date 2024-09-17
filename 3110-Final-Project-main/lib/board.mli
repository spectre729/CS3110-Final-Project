module type my_board = sig
  type t = int array array
  (** [t] is the int array array matrix that is the 2048 board. *)

  val score : int ref
  (** [score] represents the current score. *)

  val make_board : int -> int -> t
  (** [make_board n elem] creates the board with dimension [n] by [n] filled
      with element [elem]. *)

  val set_spawnable_values : int list -> unit
  (** [set_spawnable_values lst] takes list [lst] in the form \[spawnable val;
      spawnable value; goal value\] which contains the 2 values that can
      randomly spawn the value the player needs to win. *)

  val get_score : unit -> int
  (** [get_score ()] returns the current score. *)

  val to_list : t -> int list list
  (** [to_list board] converts the [board] from an using arrays to using lists. *)

  val get_value : int -> int -> t -> int
  (**[get_value x y board] returns the value at the coordinates [x] [y] in
     [board]. *)

  val set_value : int -> int -> int -> t -> unit
  (**[set_value x y value board] sets the value at coordinates [x] [y] in
     [board] to [value]. *)

  val empty_cells : t -> (int * int) list
  (**[empty_cells board] returns a list of tuples containing the indices of all
     empty cells (elements equal to 0) in [board]. *)

  val pick_1_empty : t -> int * int
  (**[pick_1_empty board] picks a random empty cell from all possible empty
     cells in [board]. *)

  val pick_2_empty : t -> (int * int) * (int * int)
  (**[pick_2_empty board] picks 2 random empty cells from all possible empty
     cells in [board]. *)

  val generate_tile : t -> t
  (**[generate_tile board] picks a empty tile in [board] and sets it to 2 or 4
     and returns the new board. If there are no empty tiles, the function
     returns [board]. *)

  val check_move : t -> char -> bool
  (**[check_move board move] is true if the player's move [move] is possible in
     [board]. *)

  val check_lost : t -> bool
  (** [check_lost board] is true if there are no possible moves left in [board]. *)

  val check_won : t -> bool
  (** [check_won board] is true if the player has won the game (reached the goal
      number). *)

  val do_move : t -> char -> t
  (** [do_move board move] performs player's move [move] on [board] and returns
      the new board. *)
end

module Board_2048 : my_board
