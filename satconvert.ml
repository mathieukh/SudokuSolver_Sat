open Clauses
open Satsolver

let convert_atome_to_litteral atome =
  let number = ((atome.cellule.i-1) * 81 + (atome.cellule.j-1) * 9 + atome.da) in
  {d=number;value=atome.signe}
  
let convert_litteral_to_atome litteral =
  let litteral = litteral.litteral in
  let signe = litteral.value in
  let x = litteral.d in
  let i = if (x mod 81 = 0) then (x/81) else (x/81) + 1 in
  let j = 
    if(((x mod 81)mod 9) = 0) then 
      if(((x mod 81)/9)=0) then 9 
      else ((x mod 81)/9) 
    else ((x mod 81)/9)+1 in
  let d = ((x mod 81) mod 9) in
  let d= if(d = 0) then 9 else d in
  {cellule={i=i;j=j};da=d;signe=signe}
  
let convert_to_satclauses clauses =
  List.rev_map
    (fun c ->
      let litt = List.rev_map
	(fun a ->
	 convert_atome_to_litteral a
	) c in
      {litterals=litt;causes=[]}
    ) clauses
    
let convert_from_satclauses atomes =
  List.rev_map
    (fun a ->
      convert_litteral_to_atome a
    ) atomes

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
