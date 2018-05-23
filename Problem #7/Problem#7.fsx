open System

let charMap = 
  seq { for x in 1..26 -> (x, char (int 'a' + x-1))}
  |> Map.ofSeq

let encryptionKeys = charMap |> Map.toSeq |> Seq.map fst

let toString (inp:int list) = 
  List.fold (fun s v -> s + (string v)) "" inp

let rec solve goal (results:int list list) current = 
  let s = toString current
  if s = goal then
    current::results
  elif s.Length < goal.Length then
    let z = Seq.fold (fun s v -> solve goal s (v::current)) results encryptionKeys
    z
  else
    results


let decode cipher = 
  Seq.fold (fun s v -> s + string charMap.[v] ) "" cipher

let solutions goal = 
  let ciphers = solve goal [] []
  Seq.fold (fun s v -> (decode v)::s) [] ciphers

let problem goal = 
  List.length (solve goal [] [])

solutions "111"

problem "111"
