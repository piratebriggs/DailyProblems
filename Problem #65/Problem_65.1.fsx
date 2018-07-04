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


type Direction = 
    | Right
    | Down
    | Left
    | Up


let rec prob (dir:Direction) l t r b = seq {
        if r-l >= 0 && b-t >= 0 then
            match dir with
            | Right -> for i in l..r do yield i,t; 
            | Down -> for i in t..b do yield r,i; 
            | Left -> for i = r downto l do yield i,b; 
            | Up -> for i = b downto t do yield l,i; 
            match dir with
            | Right -> yield! prob Down l (t+1) r b; 
            | Down ->  yield! prob Left l t (r-1) b; 
            | Left ->  yield! prob Up l t r (b-1); 
            | Up ->  yield! prob Right (l+1) t r b; 
    }

let inp = array2D [|[|1;  2;  3;  4;  5|];
 [|6;  7;  8;  9;  10|];
 [|11; 12; 13; 14; 15|];
 [|16; 17; 18; 19; 20|]|]


Seq.foldBack (fun v s -> inp.[snd v,fst v]::s ) (prob Right 0 0 4 3) []

