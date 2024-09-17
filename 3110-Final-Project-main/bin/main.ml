(**@authors Dennis Chen dc777
            Anand Bannerji ab2585
            Jonathan Miller jdm454*)

open Batteries
open Final_project.Board
open Final_project.Game
module PlayBoard : my_board = Board_2048
module PlayGame : my_game = Play_Game (PlayBoard)

let won = ref false

let get_input () =
  let () = print_string "> " in
  let the_input = read_line () in
  the_input

open Unix

let set_non_canonical () =
  let termios = tcgetattr stdin in
  let new_termios = { termios with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSANOW new_termios

let set_canonical () =
  BatIO.flush_all ();
  let termios = tcgetattr stdin in
  let new_termios = { termios with c_icanon = true; c_echo = true } in
  tcsetattr stdin TCSANOW new_termios

let get_input_char () =
  let buffer = Bytes.create 1 in
  let _ = read Unix.stdin buffer 0 1 in
  Bytes.get buffer 0

let print_clr clr str =
  match clr with
  | "b" -> print_string ("\027[38;5;21m" ^ str)
  | "0" -> print_string ("\027[38;5;0m" ^ str)
  | "1" -> print_string ("\027[38;5;251m" ^ str)
  | "2" -> print_string ("\027[38;5;251m" ^ str)
  | "4" -> print_string ("\027[38;5;223m" ^ str)
  | "8" -> print_string ("\027[38;5;173m" ^ str)
  | "16" -> print_string ("\027[38;5;208m" ^ str)
  | "32" -> print_string ("\027[38;5;202m" ^ str)
  | "64" -> print_string ("\027[38;5;166m" ^ str)
  | "128" -> print_string ("\027[38;5;220m" ^ str)
  | "256" -> print_string ("\027[38;5;227m" ^ str)
  | "512" -> print_string ("\027[38;5;214m" ^ str)
  | "1024" -> print_string ("\027[38;5;220m" ^ str)
  | "2048" -> print_string ("\027[38;5;220m" ^ str)
  | "4096" -> print_string ("\027[38;5;233m" ^ str)
  | _ -> print_string ("\027[38;5;21m" ^ str)

let rec print_list list =
  match list with
  | [] -> ()
  | h :: t ->
      let str_h = string_of_int h in
      let padding = String.make (6 - String.length str_h) ' ' in
      let () =
        if h > 4096 then
          let () = print_clr "4096" (str_h ^ padding) in
          print_clr "b" "|"
        else
          let () = print_clr str_h (str_h ^ padding) in
          print_clr "b" "|"
      in
      print_list t

let rec print_board board len =
  match board with
  | [] ->
      let () =
        if len = 4 then print_clr "b" (String.make 29 '-')
        else print_clr "b" (String.make 36 '-')
      in
      print_endline "\n"
  | h :: t ->
      let () =
        if len = 4 then print_clr "b" (String.make 29 '-')
        else print_clr "b" (String.make 36 '-')
      in
      let () = print_endline "" in
      let () = print_clr "b" "|" in
      let () = print_list h in
      let () = print_endline "" in
      print_board t len

let read_leaderboard game_type =
  BatList.of_enum
    (BatFile.lines_of ("data/leaderboard" ^ string_of_int game_type ^ ".txt"))

let write_leaderboard game_type lst =
  BatFile.write_lines
    ("data/leaderboard" ^ string_of_int game_type ^ ".txt")
    (BatList.enum lst)

let print_leaderboard game_type =
  let lst = read_leaderboard game_type in
  let t =
    match game_type with
    | 0 -> "2048"
    | 1 -> "1024"
    | 2 -> "65536"
    | _ -> "Invalid Game Type"
  in
  let () = print_clr "b" "\n" in
  let () = print_clr "b" (t ^ " Leaderboard:\n") in
  let format_leaderboard name num = print_clr "b" (name ^ " - " ^ num ^ "\n") in
  for i = 0 to 2 do
    format_leaderboard (List.nth lst (i * 2)) (List.nth lst ((i * 2) + 1))
  done;
  print_clr "b" "\n"

let first_n_elements lst n =
  let return_lst = ref [] in
  for i = n - 1 downto 0 do
    return_lst := List.nth lst i :: !return_lst
  done;
  !return_lst

let update_leaderboard_pos lst name score =
  let int_score = int_of_string score in
  if int_score > int_of_string (List.nth lst 1) then
    first_n_elements (name :: score :: lst) 6
  else if int_score > int_of_string (List.nth lst 3) then
    first_n_elements
      (List.nth lst 0 :: List.nth lst 1 :: name :: score :: lst)
      6
  else if int_score > int_of_string (List.nth lst 5) then
    [
      List.nth lst 0;
      List.nth lst 1;
      List.nth lst 2;
      List.nth lst 3;
      name;
      score;
    ]
  else lst

let update_leaderboard game_type name score =
  let updated_lst =
    update_leaderboard_pos (read_leaderboard game_type) name score
  in
  write_leaderboard game_type updated_lst

let reset_leaderboard () =
  for i = 0 to 2 do
    write_leaderboard i [ "AAA"; "0"; "AAA"; "0"; "AAA"; "0" ]
  done

let print_score is_final game_type =
  let _ = game_type in
  let () =
    if is_final then print_clr "b" "Your final score was: "
    else print_clr "b" "Score: "
  in
  let score = string_of_int (PlayBoard.get_score ()) in
  let () = print_clr "b" score in
  if is_final then
    let () = BatIO.flush_all () in
    let () = print_clr "b" "\n" in
    let () = print_clr "b" "Please enter a name for the leaderboard: \n" in
    let name = String.uppercase_ascii (get_input ()) in
    let () = update_leaderboard game_type name score in
    print_leaderboard game_type
  else print_clr "b" "\n"

let game_won_msgs game_type =
  let () = won := true in
  let () = print_clr "b" "Congratulations! You won!\n" in
  let () = print_score true game_type in
  let () = set_canonical () in
  print_clr "b" "Enter Q to quit or anything else to continue"

let game_lost_msgs game_type =
  let () = print_score true game_type in
  let () = print_clr "b" "Game Over! You lost.\n" in
  let () = set_canonical () in
  print_clr "b" "Thank you for playing.\n"

let board_move board ch =
  if PlayBoard.check_move board ch then
    let new_board = PlayBoard.do_move board ch in
    if new_board <> board then
      let updated_board = PlayBoard.generate_tile new_board in
      updated_board
    else begin
      print_clr "b" "Invalid Move/Entry\n";
      board
    end
  else begin
    print_clr "b" "Invalid Move/Entry\n";
    board
  end

let get_move () =
  let () =
    print_clr "b"
      "Please enter a move. Enter W, S, A, or D to move or Q to exit the game\n"
  in
  let _ = set_non_canonical () in
  Char.lowercase_ascii (get_input_char ())

let quit_game is_final game_type =
  BatIO.flush_all ();
  let () = print_clr "b" "Thank you for playing.\n" in
  let () = if is_final then print_score true game_type else () in
  set_canonical ();
  exit 0

let rec play_game board game_type =
  let () = print_score false game_type in
  let () = print_board (PlayBoard.to_list board) (Array.length board) in
  if PlayBoard.check_won board && !won = false then
    let () = game_won_msgs game_type in
    match Char.lowercase_ascii (get_input_char ()) with
    | 'q' -> quit_game false game_type
    | _ -> play_game board game_type
  else if PlayGame.check_lost board then begin
    BatIO.flush_all ();
    let () = game_lost_msgs game_type in
    set_canonical ();
    exit 0
  end
  else
    let ch = get_move () in
    match ch with
    | 'q' -> quit_game true game_type
    | _ -> play_game (board_move board ch) game_type

let mode_2048 () =
  let () = print_clr "b" "Welcome to 2048!\n" in
  let () = print_leaderboard 0 in
  let board = PlayGame.initialize_game 0 4 in
  let () =
    print_clr "b"
      "Please enter a move. Enter W, S, A, or D to move or Q to exit the game\n"
  in
  play_game board 0

let mode_1024 () =
  let () = print_clr "b" "Welcome to 1024!\n" in
  let () = print_leaderboard 0 in
  let board = PlayGame.initialize_game 1 4 in
  let () =
    print_clr "b"
      "Please enter a move. Enter W, S, A, or D to move or Q to exit the game\n"
  in
  play_game board 1

let mode_65536 () =
  let () = print_clr "b" "Welcome to 65536\n" in
  let () = print_leaderboard 0 in
  let board = PlayGame.initialize_game 2 5 in
  let () =
    print_clr "b"
      "Please enter a move. Enter W, S, A, or D to move or Q to exit the game\n"
  in
  play_game board 2

let print_start_msgs () =
  let () = print_clr "b" "Welcome!\n\n" in
  let () =
    print_clr "b"
      "There are 3 game modes available to play. The main mode is 2048 with \
       alternative modes being 1024 and 65536.\n"
  in
  let () =
    print_clr "b"
      "The goal of the games is to merge tiles on the board to create:\n\
       the 2048 tile (for the 2048 mode), the 1024 tile (for the 1024 mode), \
       or the 65536 tile (for the 65536 mode).\n\n"
  in
  let () =
    print_clr "b"
      "Please input the name of one of the following game modes to start \
       playing:\n"
  in
  print_clr "b" "2048, 1024, 65536\n"

let start_multiple_modes () =
  let () = print_start_msgs () in
  let mode = String.lowercase_ascii (get_input ()) in
  if mode = "2048" then mode_2048 ()
  else if mode = "1024" then mode_1024 ()
  else if mode = "65536" then mode_65536 ()
  else exit 0

let () =
  if Array.length Sys.argv = 2 then reset_leaderboard ()
  else start_multiple_modes ()
