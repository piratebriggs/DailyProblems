let folder n =
    n % 3 = 0 || n % 5 = 0

let solve n = 
    [1..n-1] |> List.filter folder |> List.sum

solve 10    // 23