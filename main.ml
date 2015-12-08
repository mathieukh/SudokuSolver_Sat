open Sudoku

let file = "puzzles.sdk"

let sudoku = 
  Sudoku.open_sudoku_game file

let diabo =
  [|[|3;0;0;0;6;0;9;0;0|];
   [|0;2;0;0;3;0;0;0;0|];
   [|9;0;1;0;0;0;0;5;0|];
   [|0;0;0;6;0;3;0;0;0|];
   [|0;4;0;0;2;0;0;1;0|];
   [|0;0;0;5;0;8;0;0;0|];
   [|0;6;0;0;0;0;3;0;5|];
   [|0;0;0;0;5;0;0;7;0|];
   [|0;0;2;0;7;0;0;0;8|]|]

let () = 
  Sudoku.print_sudoku_game sudoku;

  let sudoku_solved = Sudoku.solve sudoku in
  Sudoku.print_sudoku_game sudoku_solved;

  print_newline()
