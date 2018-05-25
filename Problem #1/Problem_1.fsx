(*
    Given an array of numbers, return whether any two sums to K.
    For example, given [10, 15, 3, 7] and K of 17, return true since 10 + 7 is 17.
*)

let inp = [10; 15; 3; 7]

let problem1 inp k =
    let s = Set.ofList inp
    let checkSum v =
        s.Remove v |> List.ofSeq |> List.tryFind (fun x -> v+x=k )
    inp |> Seq.ofList |> Seq.choose (fun x -> (checkSum x)) |> List.ofSeq

problem1 inp 17