open Satconvert
open Clauses

type sudoku = int array array

let grille = "grille.cnf"
let solution = "solution.cnf"

(* Convertir un char en entier *)

let int_from_char c = 
 match c with
 | '0' -> 0
 | '1' -> 1
 | '2' -> 2
 | '3' -> 3
 | '4' -> 4
 | '5' -> 5
 | '6' -> 6
 | '7' -> 7
 | '8' -> 8
 | '9' -> 9
 | _ -> failwith "NaN"
 
 (* Convertir une ligne String en int array array *)

let get_sudoku_from_line file_line =
  Array.init 9 (fun i -> 
    Array.init 9 (fun j -> int_from_char (String.get file_line (i*9+j) ) ) )
    
(* Ouvre un sudoku en le piochant au hasard dans le fichier *)

let open_sudoku_game file =
  Random.self_init();
  let sudokus_lines  = Filepers.lines_from_file file in
  let random = Random.int (List.length sudokus_lines) in
  get_sudoku_from_line (List.nth sudokus_lines random)
  
  
(* Fonction permettant l'affichage d'un sudoku *)

let print_sudoku_game sudoku =
  Array.iteri (fun i x ->
    print_newline();
    if(i mod 3 = 0) then
      print_endline "-------------";
    Array.iteri (fun j y ->	
      if(j mod 3 = 0) then
	print_string "|";
      print_int y;
      if(j = 8) then
	print_string "|"
    ) x ) sudoku;
  print_newline();
  print_endline "-------------"
  
(* Résolution par minisat *)
    
let solve sudoku = 
  let clauses = Clauses.get_clauses_for sudoku in
  let lines = Satconvert.convert_to_dimacs clauses in
  Filepers.file_from_lines lines grille;
  ignore (Satconvert.generate_solution grille solution);
  Clauses.convert_clauses_to_sudoku (Satconvert.convert_from_dimacs solution)

(* Résolution par DPLL Naif *)

let solve_2 sudoku = 
  let clauses = Clauses.get_clauses_for sudoku in
  let satclauses = Satconvert.convert_to_satclauses clauses in
  let response = Satconvert.convert_from_satclauses (Satsolver.resolve satclauses) in
  Clauses.convert_clauses_to_sudoku response
  
(* Résolution par DPLL Backtrack *)

let solve_3 sudoku = 
  let clauses = Clauses.get_clauses_for sudoku in
  let satclauses = Satconvert.convert_to_satclausesb clauses in
  let response = Satconvert.convert_from_satclausesb (Satsolverback.resolve satclauses) in
  Clauses.convert_clauses_to_sudoku response
  
(* Résolution par DPLL Backtrack & Learn *)

let solve_4 sudoku = 
  let clauses = Clauses.get_clauses_for sudoku in
  let satclauses = Satconvert.convert_to_satclausesl clauses in
  let response = Satconvert.convert_from_satclausesl (Satsolverlearn.resolve satclauses) in
  Clauses.convert_clauses_to_sudoku response
