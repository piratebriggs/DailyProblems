(*
    Given an array of numbers, return whether any two sums to K.
    For example, given [10, 15, 3, 7] and K of 17, return true since 10 + 7 is 17.
*)

let problem1_results inp k =
    let workingSet = Set.ofList inp
    let checkSum v =
        workingSet.Remove v |> List.ofSeq |> List.tryFind (fun x -> v+x=k )
    inp |> Seq.ofList |> Seq.choose (fun x -> (checkSum x)) |> List.ofSeq

let problem1 inp k = 
    not (List.isEmpty (problem1_results inp k))

let inp = [10; 15; 3; 7]
problem1 inp 17