open Clauses
open Satsolver
open Satsolverback
open Satsolverlearn
open Str
open Filepers

let generate_solution grille solution =
  let shell_string = "minisat -verb=0 "^grille^" "^solution in
  Sys.command shell_string

let convert_atome_to_litteral atome =
  let number = ((atome.cellule.i-1) * 81 + (atome.cellule.j-1) * 9 + atome.da) in
  {d=number;value=atome.signe}

let convert_litteral_to_atome litteral =
  let x = litteral.d in
  let i = if (x mod 81 = 0) then (x/81) else (x/81) + 1 in
  let j = 
    if(((x mod 81)mod 9) = 0) then 
      if(((x mod 81)/9)=0) then 9 
      else ((x mod 81)/9) 
    else ((x mod 81)/9)+1 in
  let d = ((x mod 81) mod 9) in
  let d= if(d = 0) then 9 else d in
  {cellule={i=i;j=j};da=d;signe=litteral.value}

let convert_atome_to_litteralb atome =
  let number = ((atome.cellule.i-1) * 81 + (atome.cellule.j-1) * 9 + atome.da) in
  {db=number;valueb=atome.signe}

let convert_litteralb_to_atome litteral =
  let litteral = litteral.litteral in
  let signe = litteral.valueb in
  let x = litteral.db in
  let i = if (x mod 81 = 0) then (x/81) else (x/81) + 1 in
  let j = 
    if(((x mod 81)mod 9) = 0) then 
      if(((x mod 81)/9)=0) then 9 
      else ((x mod 81)/9) 
    else ((x mod 81)/9)+1 in
  let d = ((x mod 81) mod 9) in
  let d= if(d = 0) then 9 else d in
  {cellule={i=i;j=j};da=d;signe=signe}

let convert_atome_to_litterall atome =
  let number = ((atome.cellule.i-1) * 81 + (atome.cellule.j-1) * 9 + atome.da) in
  {dl=number;valuel=atome.signe}

let convert_litterall_to_atome litteral =
  let litteral = litteral.litterall in
  let signe = litteral.valuel in
  let x = litteral.dl in
  let i = if (x mod 81 = 0) then (x/81) else (x/81) + 1 in
  let j = 
    if(((x mod 81)mod 9) = 0) then 
      if(((x mod 81)/9)=0) then 9 
      else ((x mod 81)/9) 
    else ((x mod 81)/9)+1 in
  let d = ((x mod 81) mod 9) in
  let d= if(d = 0) then 9 else d in
  {cellule={i=i;j=j};da=d;signe=signe}

let convert_atome_to_number atome =
  let number = ((atome.cellule.i-1) * 81 + (atome.cellule.j-1) * 9 + atome.da) in
  if(atome.signe) then number else number * -1

let convert_number_to_atome number =
  let signe = (number > 0) in
  let x = abs number in
  let i = if (x mod 81 = 0) then (x/81) else (x/81) + 1 in
  let j = 
    if(((x mod 81)mod 9) = 0) then 
      if(((x mod 81)/9)=0) then 9 
      else ((x mod 81)/9) 
    else ((x mod 81)/9)+1 in
  let d = ((x mod 81) mod 9) in
  let d= if(d = 0) then 9 else d in
  {cellule={i=i;j=j};da=d;signe=signe}
      
let convert_from_dimacs file =
  let lines = Filepers.lines_from_file file in
  if(List.hd lines = "SAT") then
    let string_list = (Str.split (Str.regexp_string " ") (List.nth lines 1)) in
    let string_list = List.filter (fun s -> s <> "0") string_list in
    List.rev_map
      (fun x->	
	convert_number_to_atome (int_of_string x)) string_list
  else
    failwith "No solution"
    
let convert_to_dimacs clauses =
  let dimacs_file = ref[] in
  let max_variables = 729 in
  (*let max_variables = get_max_variables clauses in*)
  let nbr_clauses = List.length clauses in
  let init_string =  "p cnf "^(string_of_int max_variables)^" "^(string_of_int nbr_clauses) in
  dimacs_file := init_string :: !dimacs_file;
  List.iter 
    (fun x -> 
      let string_clause = ref "" in
      List.iter
	(fun y ->
	  let number = convert_atome_to_number y in
	  let string = (string_of_int number)^" " in
	  string_clause := !string_clause^string
	) x;
      string_clause := !string_clause^(string_of_int 0);
      dimacs_file := !string_clause :: !dimacs_file
    ) clauses;
  List.rev !dimacs_file

let convert_to_satclauses clauses =
  List.rev_map
    (fun c ->
      List.rev_map
	(fun a ->
	  convert_atome_to_litteral a
	) c
    ) clauses
    
let convert_from_satclauses atomes =
  List.rev_map
    (fun a ->
      convert_litteral_to_atome a
    ) atomes

let convert_to_satclausesb clauses =
  List.rev_map
    (fun c ->
      let litt = List.rev_map
	(fun a ->
	 convert_atome_to_litteralb a
	) c in
      {litterals=litt;causes=[]}
    ) clauses
    
let convert_from_satclausesb atomes =
  List.rev_map
    (fun a ->
      convert_litteralb_to_atome a
    ) atomes

let convert_to_satclausesl clauses =
  List.rev_map
    (fun c ->
      let litt = List.rev_map
	(fun a ->
	 convert_atome_to_litterall a
	) c in
      {litteralsl=litt;causesl=[]}
    ) clauses
    
let convert_from_satclausesl atomes =
  List.rev_map
    (fun a ->
      convert_litterall_to_atome a
    ) atomes
