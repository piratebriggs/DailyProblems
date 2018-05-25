
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

let boundsCheck maxX maxY (pos:Coord) = 
    pos.x >= 0 && pos.x < maxX && pos.y >= 0 && pos.y < maxY
        
let next b (dir:Direction) (pos:Coord)  = 
    match dir with
    | Right -> b pos {x = pos.x+1; y = pos.y} Down
    | Down -> b pos {x = pos.x; y = pos.y+1} Left
    | Left -> b pos {x = pos.x-1; y = pos.y} Up
    | Up -> b pos {x = pos.x; y = pos.y-1} Right

let rec step b (curPos:Coord) (nextPos:Coord) dir =
    if b nextPos then
        nextPos
    else
        let step' = step b
        next step' dir curPos

let boundsCheck' = boundsCheck 5 4

let step' = step boundsCheck'

let res = next step' Right {x=0;y=0} 

res
