type Zipper = Zipper of (int list * int * int list) with
    static member create l =
        match l with
        | [] -> failwith "oops"
        | h :: t -> Zipper ([], h, t)

    member z.right () =
        match z with
        | Zipper(l, z, []) -> Zipper(l, z, [])
        | Zipper(l, z, h::rt) -> Zipper(z::l, h, rt)

let isEvenIndex zipper = 
    match zipper with
    | Zipper([], z, right) ->  0 = List.sum right
    | Zipper(left, z, []) ->  List.sum left = 0
    | Zipper(left, _, right) ->  List.sum left = List.sum right

let rec FindIndex zipper func = 
    if func zipper then 
        match zipper with
        | Zipper([], _, _) -> 0
        | Zipper(l, _, _) -> List.length l
    else 
        match zipper with
        | Zipper(_, _, []) -> -1
        | _ -> FindIndex (zipper.right()) func

let findEvenIndex (items : int array) = 
    match items with
    | [||] -> 0
    | [|_|] -> 0
    | _ -> FindIndex (items |> Array.toList |> Zipper.create) isEvenIndex


findEvenIndex [|1; 2; 3; 4; 3; 2; 1|]   // 3
findEvenIndex [|1; 100; 50; -51; 1; 1|] // 1
findEvenIndex [|20;10;-80;10;10;15;35|]    // 0
findEvenIndex [||]    // 0
findEvenIndex [|10|]    // 0
findEvenIndex [|-46; 15; 14; 17; 0|]    // 4 


