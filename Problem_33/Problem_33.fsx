(*
Compute the running median of a sequence of numbers. That is, given a stream of numbers, print out the median of the list so far on each new element.
Recall that the median of an even-numbered list is the average of the two middle numbers.
For example, given the sequence [2, 1, 5, 7, 2, 0, 5], your algorithm should print out:
2
1.5
2
3.5
2
2
2
*)

let inp = [2; 1; 5; 7; 2; 0; 5]
open System.Data

7/2
inp.[3]


let odd a = 
    (a%2=1)

let median (l:int list) =
    let mid = l.Length/2
    if( odd l.Length ) then
        l.[mid]
    else
        (l.[mid-1] + l.[mid])/2

let mf (acc:int list) (x:int list) =
    let v = (median x)
    v::acc

let rec mr (l1:int list) (l2:int list)  =
    match l2 with 
    | [] -> l1
    | head::tail -> let nl= head::l1
                    mr (median nl) tail
       



