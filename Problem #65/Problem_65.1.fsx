(*
This problem was asked by Amazon.
Given a N by M matrix of numbers, print out the matrix in a clockwise spiral.
For example, given the following matrix:
[[1,  2,  3,  4,  5],
 [6,  7,  8,  9,  10],
 [11, 12, 13, 14, 15],
 [16, 17, 18, 19, 20]]
You should print out the following:
1
2
3
4
5
10
15
20
19
18
17
16
11
6
7
8
9
14
13
12
*)


type Tree<'T> =
    | Node of Tree<'T> * 'T * Tree<'T>
    | Leaf

let rec depth = function
    | Node(l, _, r) -> 1 + max (depth l) (depth r)
    | Leaf -> 0


type Direction = 
    | Right
    | Down
    | Left
    | Up

type Coord = {
    x:int;
    y:int;
}

type State = {
    dir:Direction;
    pos:Coord;
}
        
let next state  = 
    let newPos = match state.dir with
        | Right -> { state.pos with x = state.pos.x+1} 
        | Down ->  { state.pos with y = state.pos.y+1} 
        | Left ->  { state.pos with x = state.pos.x-1} 
        | Up ->    { state.pos with y = state.pos.y-1} 
    {state with pos = newPos}

let changeDir state  = 
    match state.dir with
    | Right -> {state with dir = Down} 
    | Down ->  {state with dir = Left}
    | Left ->  {state with dir = Up}
    | Up ->  {state with dir = Right}


(*
need to store bounds of array in state
as each dim is completed, reduce bounds of array

12345
12345
12345
12345
12345


*)


let rec prob (dir:Direction) l t r b = seq {
        match dir with
        | Right -> for i in l..r do yield i; yield! prob Down l (t-1) r b; 
        | Down -> for i in t..b do yield i; yield! prob Left l t (r-1) b; 
        | Left -> for i in r..l do yield i; yield! prob Up l t r (b-1); 
        | Up -> for i in b..t do yield i; yield! prob Right (l-1) t r b; 
    }

prob Right 0 0 5 5 |> Seq.toList

