open System.Collections.Generic
open System

type OpenCell = {
    FScore: float; GScore: float
}

type UCell =
    | Unknown
    | Open of OpenCell
    | Closed

type Position = int*int

type Board = {
    Walls : bool[,]
    Cells : Map<Position,UCell>
    CameFrom : Map<Position,Position>
}

let MakeBoard (walls:bool[,]) =
    { Walls = walls;
        Cells = Map.ofList [for x in 1..Array2D.length1 walls do
                            for y in 1..Array2D.length2 walls do
                                yield (x,y),UCell.Unknown ];
        CameFrom = Map.empty
    }

let board =  MakeBoard (
                array2D [|    
                        [|false; false; false; false|];
                        [|true; true; true; false|];
                        [|false; false; false; false|];
                        [|false; false; false; false|]|])

let  GetNeighbors (x,y) (board:bool[,]) = 
    let makeList x =
        match x with
        | Some(v) -> [v]
        | None -> []

    let topNeighbor (x, y) =
        if(x>1) then Some (x-1, y) else None

    let leftNeighbor (x, y) = 
        if(y>1) then Some (x, y-1) else None

    let bottomNeighbor (x, y, maxX) =
        if(x<maxX) then Some (x+1, y) else None

    let rightNeighbor (x, y, maxY) = 
        if(y<maxY) then Some (x, y+1) else None
    
    makeList(topNeighbor(x,y)) 
        @ makeList(leftNeighbor(x,y)) 
        @ makeList(bottomNeighbor(x,y,Array2D.length1 board))
        @ makeList(rightNeighbor(x,y,Array2D.length2 board))

let DistanceTo (x,y) (goalX,goalY) = 
    let x' = goalX - x
    let y' = goalY - y
    Math.Sqrt(float(y'*y' + x'*x'))
    
let mapJoin = Map.fold (fun acc key value -> Map.add key value acc) 
let mapJoin2 = Map.fold (fun acc key value -> Map.add key value acc) 

let goal = (Array2D.length1 board.Walls,Array2D.length2 board.Walls)

let board' = {board with
                Cells = Map.add (1,1) (UCell.Open {FScore = (DistanceTo (1,1) goal); GScore=0.0}) board.Cells
                }

let rec reconstructPath current (camefrom: Map<Position,Position>) acc = 
    match Map.tryFind current camefrom with
        | Some current -> reconstructPath current camefrom (current::acc)
        | None -> acc

let rec AStar (goal:Position) (board:Board) =
    let openCells = Map.toList board.Cells |> List.choose (fun (key,value) -> match value with |Open cell -> Some (key,cell)|_ -> None)
    if List.isEmpty openCells then
        []
    else
        let current = List.minBy (fun value -> match value with _,value -> value.FScore ) openCells
        if (fst current) = goal then
            reconstructPath (fst current) board.CameFrom []
        else
            let neighborPositions = GetNeighbors (fst current) board.Walls
            let unkNeighbors = Map.filter (fun key value -> List.contains key neighborPositions &&  match value with |Unknown -> true |_ -> false) board.Cells
            let openNeighbors = Map.map (fun key _ -> UCell.Open {FScore = (DistanceTo key goal); GScore= (snd current).GScore+1.0}) unkNeighbors
            let cells = mapJoin board.Cells openNeighbors
            let cells = Map.add (fst current) UCell.Closed cells
            let cameFrom = mapJoin2 board.CameFrom (Map.map (fun key _ -> (fst current)) openNeighbors)
            AStar goal {board with 
                            Cells = cells
                            CameFrom = cameFrom
                        }

AStar goal board'
