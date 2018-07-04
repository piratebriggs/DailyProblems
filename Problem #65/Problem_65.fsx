type Direction = 
    | Right
    | Down
    | Left
    | Up

type Coord = {
    x:int;
    y:int;
}

let boundsCheck (data:int[,]) (pos:Coord) =
    pos.x >= 0 && pos.x < Array2D.length2 data && pos.y >= 0 && pos.y < Array2D.length1 data
        
let next (dir:Direction) (pos:Coord)  = 
    match dir with
    | Right -> {pos with x = pos.x+1;} 
    | Down -> {pos with y = pos.y+1}
    | Left -> {pos with x = pos.x-1;}
    | Up -> {pos with y = pos.y-1}

let nextDir (dir:Direction)  = 
    match dir with
    | Right -> Down
    | Down -> Left
    | Left -> Up
    | Up -> Right

let shrink (dir:Direction) (data:int[,]) = 
    match dir with
    | Right -> data.[1..,*], {x=(Array2D.length2 data-1); y=0}
    | Down -> data.[*,0..(Array2D.length2 data-2)], {x=(Array2D.length2 data-2); y=(Array2D.length1 data-1)}
    | Left -> data.[0..(Array2D.length1 data-2),*], {x=0; y=(Array2D.length1 data-2)}
    | Up -> data.[*,1..], {x=0; y=0}

let rec prob pos (dir:Direction) state (data:int[,]) =
    printfn "%i,%i,%s" pos.x pos.y (dir.ToString())
    printfn "%A" data
    if Array2D.length1 data = 0 || Array2D.length2 data = 0 then
        state
    else
        let newState = data.[pos.y,pos.x]::state
        let newPos = next dir pos
        if boundsCheck data newPos then
            prob newPos dir newState data
        else
            let newDir = nextDir dir
            let newData,newPos = shrink dir data
            prob newPos newDir newState newData
    
let inp = array2D [|[|1;  2;  3;  4;  5|];
    [|6;  7;  8;  9;  10|];
    [|11; 12; 13; 14; 15|];
    [|16; 17; 18; 19; 20|]|]
let inp2 = array2D [|[|1;   2;  3; 4|];
                     [|12; 13; 14; 5|];
                     [|11; 16; 15; 6|];
                     [|10;  9;  8; 7|]|]

let inp3 = array2D [|[|1;  2;  3;  4;  5|];
    [|6;  7;  8;  9;  10|];
    [|11; 12; 13; 14; 15|];
    [|16; 17; 18; 19; 20|];
    [|21; 22; 23; 24; 25|];
    [|26; 27; 28; 29; 30|]|]

prob {x=0;y=0} Right [] inp |> List.rev

