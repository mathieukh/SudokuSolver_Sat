type cellule = { i : int; j : int }
type atome = { cellule : cellule; da : int; signe : bool }
type clause = atome list
type fnc = clause list

let valid var = 
  let clauses = ref [] in
  let lgt = (List.length var - 1)  in
  if(lgt <> 0) then
    begin
      for x=0 to lgt do
	for y=0 to lgt do
	  if (x < y) then
	    begin
	      for k=1 to 9 do
		let cel1 = (List.nth var x) in
		let atome1 = { cellule=cel1; da=k; signe=false} in
		let cel2 = (List.nth var y) in
		let atome2 = { cellule=cel2; da=k; signe=false} in
		let clause = [atome1;atome2] in
		clauses := clause :: !clauses
	      done;
	    end;
	done;
      done;
    end;
  !clauses
   
let get_clauses =
  let clauses = ref[] in
  
  (* Ensure each cell *)
  for i=1 to 9 do
    for j=1 to 9 do
      let atomes_each = ref[] in
      for d=1 to 9 do
	atomes_each := {cellule={i=i;j=j};da=d;signe=true} :: !atomes_each
      done;
      clauses := !atomes_each :: !clauses
    done;
  done;
  
 (* Denotes one of the 9 digits *)
  for i=1 to 9 do
    for j=1 to 9 do
      let atomes_one = ref[] in
      for d=1 to 9 do
	for d'=(d+1) to 9 do
	  if d < d' then
	    begin
	      atomes_one := {cellule={i=i;j=j};da=d;signe=false} :: !atomes_one;
	      atomes_one := {cellule={i=i;j=j};da=d';signe=false} :: !atomes_one;
	    end;
	done;
      done;
      clauses := !atomes_one :: !clauses;
    done;
  done;
  
  (* Row & column constraint *)
  for i=1 to 9 do
    let cellules_row = ref[] in
    let cellules_column = ref[] in
    for j=1 to 9 do
      cellules_row := {i=i;j=j} :: !cellules_row;
      cellules_column := {i=j;j=i} :: !cellules_column
    done;
    clauses := (valid !cellules_row) @ !clauses ;
    clauses := (valid !cellules_column) @ !clauses
  done;

  (* Square constraint *)
  List.iter
    (fun i ->    
      List.iter
	(fun j ->
	  let cellules_square = ref [] in
	  for d=0 to 8 do
	    cellules_square := {i=(i+(d mod 3));j=(j+(d/3))} :: !cellules_square
	  done;
	  clauses := (valid !cellules_square) @ !clauses
	) [1;4;7];   
    ) [1;4;7];
  !clauses

let convert_clauses_to_sudoku clauses =
  let clauses = List.filter (fun x -> (x.signe)) clauses in
  Array.init 9 
    (fun i ->
      Array.init 9
	(fun j ->
	  (List.find (fun a -> (a.cellule.i = i+1 && a.cellule.j = j+1)) clauses).da
	  (*(List.nth clauses ((i*9)+j)).d*)
	))
    
let convert_sudoku_to_clauses sudoku = 
  let clauses = ref [] in
  Array.iteri (fun i x ->
    Array.iteri (fun j y ->
      if (y <> 0) then
	clauses := [{cellule={i=(i+1);j=(j+1)};da=y;signe=true}] :: !clauses
    ) x ) sudoku;
  !clauses

let get_clauses_for sudoku =
  let clauses = get_clauses in
  clauses @ (convert_sudoku_to_clauses sudoku)
