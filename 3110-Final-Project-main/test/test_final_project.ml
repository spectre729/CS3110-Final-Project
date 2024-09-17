open OUnit2
open Final_project.Board
open Final_project.Game
module Test_board : my_board = Board_2048
module Test_game : my_game = Play_Game (Test_board)

(**[compare_matrix] checks if two game_boards are equal
   @param matrix_1 is game_board 1
   @param matrix_2 is game_board 2
   @return a boolean*)
let rec compare_matrix matrix_1 matrix_2 =
  match matrix_1 with
  | [] -> true
  | _ :: _ ->
      if List.hd matrix_1 = List.hd matrix_2 then
        compare_matrix (List.tl matrix_1) (List.tl matrix_2)
      else false

let tests =
  [
    ( "Empty board test" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (Test_board.make_board 4 0 |> Test_board.to_list) );
    ( "Simple get test" >:: fun _ ->
      assert_equal 0 (Test_board.make_board 4 0 |> Test_board.get_value 2 3) );
    ( "Simple set test" >:: fun _ ->
      assert_equal 4
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 2 3 4 board in
         let a = Test_board.get_value 2 3 board in
         a) );
    ( "Simple get score test" >:: fun _ ->
      assert_equal 0 (Test_board.get_score ()) );
    ( "Get score test 2" >:: fun _ ->
      assert_equal 2
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.do_move board 'a' in
         Test_board.get_score ()) );
    ( "Set and print" >:: fun _ ->
      assert_equal
        [ [ 0; 2; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 1 2 board in
         let board_list = Test_board.to_list board in
         board_list) );
    ( "Empty cell test 1" >:: fun _ ->
      assert_equal []
        (let board = Test_board.make_board 4 2 in
         let lst = Test_board.empty_cells board in
         lst) );
    ( "Empty cell test 2" >:: fun _ ->
      assert_equal
        [ (0, 1) ]
        (let board = Test_board.make_board 4 2 in
         let _ = Test_board.set_value 0 1 0 board in
         let lst = Test_board.empty_cells board in
         lst) );
    ( "Check lost test" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let has_lost = Test_board.check_lost board in
         has_lost) );
    ( "Check lost test 2" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 1 1 4 board in
         let _ = Test_board.set_value 2 1 8 board in
         let _ = Test_board.set_value 0 2 16 board in
         let _ = Test_board.set_value 3 2 32 board in
         let has_lost = Test_board.check_lost board in
         has_lost) );
    ( "Generate tile test 1" >:: fun _ ->
      assert_equal true
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 2 3 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 2 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         let _ = Test_board.generate_tile board in
         let board_list = Test_board.to_list board in
         let equal =
           compare_matrix board_list
             [ [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ] ]
         in
         equal) );
    ( "Generate tile test 2" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 3 4 board in
         let _ = Test_board.generate_tile board in
         let board_list = Test_board.to_list board in
         let equal =
           compare_matrix board_list
             [ [ 0; 0; 0; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
         in
         equal) );
    ( "Generate tile test 3" >:: fun _ ->
      assert_equal true
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 2 3 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 2 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.generate_tile board in
         let _ = Test_board.generate_tile board in
         let board_list = Test_board.to_list board in
         let equal =
           compare_matrix board_list
             [ [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ] ]
           || compare_matrix board_list
                [
                  [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ]; [ 2; 2; 2; 2 ]; [ 2; 2; 2; 4 ];
                ]
         in
         equal) );
    ( "Check game doesn't end at start" >:: fun _ ->
      assert_equal false
        (let game = Test_game.initialize_game 0 4 in
         let has_lost = Test_game.check_lost game in
         has_lost) );
    ( "Move right test 1" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move right test 2" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 4; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move right test 3" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 4; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 4 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move right test 4" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 2 ]; [ 0; 0; 0; 2 ]; [ 0; 0; 0; 2 ]; [ 0; 0; 0; 2 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move right test 5" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move right test 6" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 2; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move right test 7" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 4 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let new_board = Test_board.do_move board 'D' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move left test 1" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let new_board = Test_board.do_move board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move left test 2" >:: fun _ ->
      assert_equal
        [ [ 2; 0; 0; 0 ]; [ 2; 0; 0; 0 ]; [ 2; 0; 0; 0 ]; [ 2; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         let new_board = Test_board.do_move board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move left test 3" >:: fun _ ->
      assert_equal
        [ [ 4; 2; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let new_board = Test_board.do_move board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move left test 4" >:: fun _ ->
      assert_equal
        [ [ 4; 4; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let new_board = Test_board.do_move board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move left test 5" >:: fun _ ->
      assert_equal
        [ [ 4; 4; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let new_board = Test_board.do_move board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move left test 6" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let new_board = Test_board.do_move board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move up test 1" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let new_board = Test_board.do_move board 'W' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move up test 2" >:: fun _ ->
      assert_equal
        [ [ 2; 2; 2; 2 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         let new_board = Test_board.do_move board 'W' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move up test 3" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 2; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let new_board = Test_board.do_move board 'W' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move up test 4" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 4; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let new_board = Test_board.do_move board 'W' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move up test 5" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 4; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let new_board = Test_board.do_move board 'W' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move up test 6" >:: fun _ ->
      assert_equal
        [ [ 4; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let new_board = Test_board.do_move board 'W' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move down test 1" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 4; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let new_board = Test_board.do_move board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move down test 2" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 2; 2; 2; 2 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         let new_board = Test_board.do_move board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move down test 3" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 2; 0; 0; 0 ]; [ 4; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let new_board = Test_board.do_move board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move down test 4" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 4; 0; 0; 0 ]; [ 4; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let new_board = Test_board.do_move board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move down test 5" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 4; 0; 0; 0 ]; [ 4; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let new_board = Test_board.do_move board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Move down test 6" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 4; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let new_board = Test_board.do_move board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Multi move test 1" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 4; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let new_board = Test_board.do_move board 'D' in
         let new_board = Test_board.do_move new_board 'S' in
         let new_board = Test_board.do_move new_board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Multi move test 2" >:: fun _ ->
      assert_equal
        [ [ 8; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let new_board = Test_board.do_move board 'A' in
         let new_board = Test_board.do_move new_board 'A' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Multi move test 3" >:: fun _ ->
      assert_equal
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 32 ] ]
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 2 3 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 2 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         let new_board = Test_board.do_move board 'A' in
         let new_board = Test_board.do_move new_board 'D' in
         let new_board = Test_board.do_move new_board 'W' in
         let new_board = Test_board.do_move new_board 'S' in
         let board_list = Test_board.to_list new_board in
         board_list) );
    ( "Win test 1" >:: fun _ ->
      assert_equal true
        (let board = Test_game.initialize_game 0 4 in
         let _ = Test_board.set_value 1 0 2048 board in
         let won = Test_board.check_won board in
         won) );
    ( "Alt Win test 1" >:: fun _ ->
      assert_equal true
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 1 0 2048 board in
         let won = Test_board.check_won board in
         won) );
    ( "Win test 2" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 2 3 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 2 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         Test_board.check_won board) );
    ( "Win test 3" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 0 1 8 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 16 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 4 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 8 board in
         let _ = Test_board.set_value 2 0 32 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 4 board in
         let _ = Test_board.set_value 2 3 512 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 1024 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 128 board in
         Test_board.check_won board) );
    ( "Win test 4" >:: fun _ ->
      assert_equal true
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 0 1 8 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 16 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 4 board in
         let _ = Test_board.set_value 1 2 2048 board in
         let _ = Test_board.set_value 1 3 8 board in
         let _ = Test_board.set_value 2 0 32 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 4 board in
         let _ = Test_board.set_value 2 3 512 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 1024 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 128 board in
         Test_board.check_won board) );
    ( "Lose test 1" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 4 board in
         Test_board.check_lost board) );
    ( "Lose test 2" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 2048 board in
         Test_board.check_lost board) );
    ( "Lose test 3" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 2 board in
         let _ = Test_board.set_value 0 1 2 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 2 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 2 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 2 board in
         let _ = Test_board.set_value 2 0 2 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 2 board in
         let _ = Test_board.set_value 2 3 2 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 2 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 2 board in
         Test_board.check_lost board) );
    ( "Lose test 4" >:: fun _ ->
      assert_equal false
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 0 1 8 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 16 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 4 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 8 board in
         let _ = Test_board.set_value 2 0 32 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 4 board in
         let _ = Test_board.set_value 2 3 512 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 1024 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 128 board in
         Test_board.check_lost board) );
    ( "Lose test 5" >:: fun _ ->
      assert_equal true
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 0 1 8 board in
         let _ = Test_board.set_value 0 2 4 board in
         let _ = Test_board.set_value 0 3 16 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 4 board in
         let _ = Test_board.set_value 1 2 2 board in
         let _ = Test_board.set_value 1 3 8 board in
         let _ = Test_board.set_value 2 0 32 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 4 board in
         let _ = Test_board.set_value 2 3 512 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 1024 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 128 board in
         Test_board.check_lost board) );
    ( "Lose test 6" >:: fun _ ->
      assert_equal true
        (let board = Test_board.make_board 4 0 in
         let _ = Test_board.set_spawnable_values [ 2; 4; 2048 ] in
         let _ = Test_board.set_value 0 0 4 board in
         let _ = Test_board.set_value 0 1 8 board in
         let _ = Test_board.set_value 0 2 2 board in
         let _ = Test_board.set_value 0 3 16 board in
         let _ = Test_board.set_value 1 0 2 board in
         let _ = Test_board.set_value 1 1 4 board in
         let _ = Test_board.set_value 1 2 2048 board in
         let _ = Test_board.set_value 1 3 8 board in
         let _ = Test_board.set_value 2 0 32 board in
         let _ = Test_board.set_value 2 1 2 board in
         let _ = Test_board.set_value 2 2 4 board in
         let _ = Test_board.set_value 2 3 512 board in
         let _ = Test_board.set_value 3 0 2 board in
         let _ = Test_board.set_value 3 1 1024 board in
         let _ = Test_board.set_value 3 2 2 board in
         let _ = Test_board.set_value 3 3 128 board in
         Test_board.check_lost board) );
  ]

let test_suite = "Final Project test suite" >::: tests
let _ = run_test_tt_main test_suite
