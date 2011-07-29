let extract_pair_positions old_string = 
  let length = String.length old_string in 
  let positions = Hashtbl.create length in 
  for pos = length-2 downto 0 do
    let pair = Char.code old_string.[pos] * 256 + Char.code old_string.[pos+1] in
    Hashtbl.add positions pair pos
  done ;
  positions

let identify_pairs pair_positions new_string = 
  let length = String.length new_string in  
  Array.init (length-1) (fun i -> 
    let pair = (Char.code new_string.[i] * 256 + Char.code new_string.[i+1]) in
    Hashtbl.find_all pair_positions pair
  )

let merge_pair_positions pairs = 

  let length = Array.length pairs in   
  let previous_undecided = ref 0 in

  (* Find all decided pairs (those with only one possible source) and force them laterally to
     cover as many characters as possible *)
  for i = 0 to length - 2 do    
    match seq.(i) with [source] -> begin                 
      if List.mem (source+1) pairs.(i+1) then pairs.(i+1) <- [source+1] else pairs.(i+1) <- [] ;
      for j = !previous_undecided to i-1 do 
	if List.mem (source+j-i) pairs.(j) then pairs.(j) <- [source+j-i] else pairs.(j) <- [] ;
      done ;	
      previous_undecided := i
    end | [] -> previous_undecided := (i+1) | _ -> ()
  done ;

  (* Identify all remaining undecided pairs, pick the first source (arbitrarily) and expand it
     laterally to cover as many characters as possible *)
  for i = 0 to length - 2 do
    match pairs.(i) with [] -> () | source :: _ -> begin
      pairs.(i) <- [source] ;
      if List.mem (source+1) pairs.(i+1) then pairs.(i+1) <- [source+1] else pairs.(i+1) <- []
    end
  done ;

  (* Remove sequences of kept pairs shorter than [smallest_kept] *)
  let previous_empty = ref 0 in
  let smallest_kept = 8 in

  for i = 0 to length - 1 do 
    if pairs.(i) = [] then (
      if i - !previous_empty <= smallest_kept then
	for j = !previous_empty to i do pairs.(j) <- [] done ;
      previous_empty := i+1 ;
    ) 
  done 

type diff_change = FromOld of int * int | FromNew of int

type diff = {
  new_text : string ;
  changes  : diff_change list 
}

let build_diff merged_pair_positions new_string = 

  let buffer = Buffer.create (String.length new_string) in
  let length = Array.length merged_pair_positions in 
  
  let rec from_old position start i =     
    if i = length || merged_pair_positions.(i) = [] then  
      (FromOld (position - start, i - start + 1))
      :: (if i = length then [] else from_new (i+1) (i+1))
    else
      from_old position start (i+1)

  and from_new start i = 
    if i = length then 
      if i = start then [] else
	( Buffer.add_substring buffer new_string start (i-start+1) ; [FromNew (i-start+1)] ) 
    else
      match merged_pair_positions.(i) with [] -> from_new start (i+1) | position :: _ ->
	if i <= start + 1 then from_old position i (i+1) else
	  ( Buffer.add_substring buffer dest start (i-start) ;
	    FromNew (i-start) :: from_old position i (i+1) )
  in

  let changes  = from_new 0 0 in
  let new_text = Buffer.contents buffer in

  { new_text ; changes }

let diff old_string new_string = 
  let pairs = identify_pairs (extract_pair_positions old_string) new_string in 
  merge_pair_positions pairs ;
  build_diff pairs new_string
      
let apply diff old_text = 

  let buffer = Buffer.create (max (String.length old_text) 100) in
  let new_text_position = ref 0 in

  let () = 
    List.iter begin function 

      | FromOld (offset,length) -> 
	Buffer.add_substring buffer old_text (Buffer.length buffer + offset) length

      | FromNew length ->
	Buffer.add_substring buffer diff.new_text !new_text_position length ;
	new_text_position := !new_text_position + length
 
    end diff.changes
  in

  Buffer.contents buffer

