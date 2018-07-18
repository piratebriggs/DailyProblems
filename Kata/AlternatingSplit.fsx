let rec encrypt (str:string) (n:int) = 
    let rec encrypt' (leftList,rightList) = function
        | odd::even::rest -> encrypt' (odd::leftList,even::rightList) rest
        | odd::_ -> (odd::leftList,rightList)
        | _ -> leftList,rightList
    let str' inp = encrypt' ([],[]) (Seq.toList inp) |> (fun (fst,snd) -> (List.rev snd)@(List.rev fst)) |> Array.ofList |> (fun x -> new string(x))
    match str, n with 
    | null,_ -> null
    | "",_ -> ""
    | _,_ when n < 1 -> str
    | _,1 -> str' str
    | _,_ -> encrypt (str' str) (n-1)
    



let rec decrypt (str:string) (n:int) = 
    let rec zip = function
        | left::leftRest, right::rightRest -> right :: left :: (zip (leftRest, rightRest))
        | [],[right] -> [right]
        | _ -> []
    let str' (inp:string) = inp.Length/2 |> (fun x -> inp.[..x-1],inp.[x..]) |> (fun (l,r) -> Seq.toList l,Seq.toList r) |> zip |> Array.ofList |> (fun x -> new string(x))
    match str, n with 
    | null,_ -> null
    | "",_ -> ""
    | _,_ when n < 1 -> str
    | _,1 -> str' str
    | _,_ -> decrypt (str' str) (n-1)

encrypt null -1


encrypt "This is a test" -1

decrypt "hsi  etTi sats!" 1

decrypt "s eT ashi tist" 2