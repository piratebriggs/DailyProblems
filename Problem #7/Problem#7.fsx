open System

let charMap = 
  seq { for x in 1..26 -> (x, char (int 'a' + x-1))}
  |> Map.ofSeq

let encryptionKeys = charMap |> Map.toSeq |> Seq.map fst

let toString (inp:int list) = 
  List.fold (fun s v -> s + (string v)) "" inp

let rec problem goal (results:int list list) current = 
  let s = toString current
  if s = goal then
    current::results
  elif s.Length < goal.Length then
    let z = Seq.fold (fun s v -> problem goal s (v::current)) results encryptionKeys
    z
  else
    results

problem "111" [] [] 