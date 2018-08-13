(*
Divisors of 42 are : 1, 2, 3, 6, 7, 14, 21, 42. These divisors squared are: 1, 4, 9, 36, 49, 196, 441, 1764. The sum of the squared divisors is 2500 which is 50 * 50, a square!

Given two integers m, n (1 <= m <= n) we want to find all integers between m and n whose sum of squared divisors is itself a square. 42 is such a number.

The result will be an array of arrays or of tuples (in C an array of Pair) or a string, each subarray having two elements, first the number whose squared divisors is a square and then the sum of the squared divisors.

#Examples:

list_squared(1, 250) --> [[1, 1], [42, 2500], [246, 84100]]
list_squared(42, 250) --> [[42, 2500], [246, 84100]]

*)

let square n =
    n * n

let divisors n = 
    seq [for i in 1..n/2 do if n%i = 0 then yield i
         yield n]
    |> Seq.toList
    
let sumofsquareddivisors n = 
    (divisors n)
    |> List.map square 
    |> List.sum

let issquare n =
    let n' = float n
    sqrt n' = floor (sqrt n')

let listSquared m n = 
  match (m, n) with
  | m, n when m >= 1 && n >= m ->
    seq {for i in m..n do yield i, sumofsquareddivisors i }
    |> Seq.filter (fun (_, sqi) -> issquare sqi )
    |> Seq.toList
  | _,_ -> []

listSquared 1 250
