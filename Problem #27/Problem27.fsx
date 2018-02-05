

let matchClose list expected =
    match list with 
    | (head:char) :: tail when head = expected -> tail
    | [] -> failwith "Too many close brackets" 
    | (actualHead:char) :: _ -> failwith (sprintf "Expected %c, actual %c" expected actualHead)

let checkChar (c:char) acc= 
    match c with
    | '{' | '(' | '[' -> c :: acc
    | '}' -> matchClose acc '{'
    | ')' -> matchClose acc '('
    | ']' -> matchClose acc '[' 
    | _ -> acc

let balanced (s:string) = 
    let rec balancedRec list acc =
        match list with
        | head :: tail -> balancedRec tail (checkChar head acc)
        | [] -> acc
    try
        match balancedRec [for c in s -> c] [] with
        | [] -> true
        | _ -> failwith "Too few close brackets"
    with
        | Failure msg -> printfn "%s" msg; false



let input = "(wibble[])[a-z]({wobble})"
printfn "'%s' is balanced? %b" input (balanced input)
