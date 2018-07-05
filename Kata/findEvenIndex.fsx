let findEvenIndex (inp:int array)  = 
    match inp with
        | [||] -> 0
        | _ ->  inp |> Array.toList 
                |> List.scan (fun (leftsum, focus, rightsum) v -> (leftsum + focus, v, rightsum-v ) ) (0,0,Array.sum inp)
                |> List.tail
                |> List.tryFindIndex (fun (leftsum, _, rightsum) -> leftsum = rightsum)
                |> Option.defaultValue -1

findEvenIndex [|1; 2; 3; 4; 3; 2; 1|]   // 3
findEvenIndex [|1; 100; 50; -51; 1; 1|] // 1
findEvenIndex [|20;10;-80;10;10;15;35|]    // 0
findEvenIndex [||]    // 0
findEvenIndex [|10|]    // 0
findEvenIndex [|-46; 15; 14; 17; 0|]    // 4 


