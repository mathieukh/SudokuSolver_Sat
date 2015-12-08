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

  let t = Sys.time() in
  let sudoku_solved = Sudoku.solve sudoku in
  Format.eprintf "Execution time Minisat: %fs@." (Sys.time() -. t);
  Sudoku.print_sudoku_game sudoku_solved;

  let t = Sys.time() in
  let sudoku_solved = Sudoku.solve_2 sudoku in
  Format.eprintf "Execution time DPLL Naif: %fs@." (Sys.time() -. t);
  Sudoku.print_sudoku_game sudoku_solved;

  let t = Sys.time() in
  let sudoku_solved = Sudoku.solve_3 sudoku in
  Format.eprintf "Execution time DPLL BackJumping: %fs@." (Sys.time() -. t);
  Sudoku.print_sudoku_game sudoku_solved;

  let t = Sys.time() in
  let sudoku_solved = Sudoku.solve_4 sudoku in
  Format.eprintf "Execution time DPLL BackJump & Learn: %fs@." (Sys.time() -. t);
  Sudoku.print_sudoku_game sudoku_solved;

  print_newline()
