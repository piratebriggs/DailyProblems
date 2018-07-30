(* let rot90CounterIndex max (x:int, y:int) =
    max-y,x


rot90CounterIndex 3 (1,2)

let rot90Counter (s: string) =
    let s' = s.Split '\n' 
             |> (fun s' -> Array2D.init (Array.length s') (Array.length s') (fun x y -> s'.[y].[x]))
    Array2D.mapi(fun x y v -> s') s'
*)

let rot90CounterIndex max (x:int, y:int) =
    [|y;max-x|]

let diag_2_sym max (x:int, y:int) =
    [|max-y;x|]
(*
let oper (fcn:int->(int*int)->int []) (s:string) = 
    s.Split '\n'
        |> (fun s' -> Array2D.init (s'.Length) (s'.Length) (fun x y -> s'.[x].[y] ))
        |> (fun matrix -> Array2D.init (Array2D.length1 matrix) (Array2D.length2 matrix) (fun x y -> matrix.GetValue (fcn ((Array2D.length1 matrix)-1) (x,y) )))
*)

    [|for i in 1..4 ->
        for j in 1..4 ->
            yield rot90CounterIndex 4 (i,j)|]

seq { for i in 0..4 ->  }

let rot90Counter (s: string) =
    s |> oper rot90CounterIndex
    
let diag2Sym (s: string) =
    s |> oper diag_2_sym
let selfieDiag2Counterclock (s: string) =
    (diag2Sym s) + (rot90Counter s)
    
rot90Counter "abcd\nefgh\nijkl\nmnop"
