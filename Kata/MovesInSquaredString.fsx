let rot90CounterIndex max (x:int, y:int) =
    max-y,x
let diag_2_symIndex max (x:int, y:int) =
    max-y,max-x

let getChar (arr:string[]) (x,y) =
    arr.[y].[x]

let oper' func (s: string) =
    let s' = s.Split '\n' 
    let dim = (Array.length s')-1
    seq { for y in 0..dim do 
            for x in 0..dim do
                yield getChar s' (func dim (x,y)) 
            yield '\n'
    }  |> Array.ofSeq |> System.String |> (fun s -> s.TrimEnd '\n')

let oper func s = 
    func s
let rot90Counter (s: string) =
    s |> oper' rot90CounterIndex
    
let diag2Sym (s: string) =
    s |> oper' diag_2_symIndex

let combineArray (a1:string[]) (a2:string[]) = 
    match a2 with
    | [||] -> a1
    | _ -> Array.mapi (fun i v -> v + "|" + a2.[i] ) a1

let combinecubes (cubes:string list) = 
    List.fold  (fun s (v:string) -> (v.Split '\n')::s) [] cubes
    |> List.fold  (fun s (v:string[]) -> combineArray v s) [||]
    |> String.concat "\n" 
    
let selfieDiag2Counterclock (s: string) =
    combinecubes [s; (diag2Sym s) ; (rot90Counter s)]


selfieDiag2Counterclock "NJVGhr\nMObsvw\ntPhCtl\nsoEnhi\nrtQRLK\nzjliWg"
let rot90Counter2 (s: string) =
    s.Split([|'\n'|]) |> Array.map (fun x -> x.ToCharArray() |> Seq.toList) |> Seq.toList |> transpos

rot90Counter2 "NJVGhr\nMObsvw\ntPhCtl\nsoEnhi\nrtQRLK\nzjliWg"

