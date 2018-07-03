let solve n = 
    let threeorfive inp = 
        inp % 3 = 0 || inp % 5 = 0

    let rec threeandfive (state:int list) inp =
        if inp = 0 then 
            state
        else
            let newstate = if (threeorfive inp) then (inp::state) else state
            threeandfive newstate (inp-1)

    List.sum (threeandfive [] (n-1))

solve 10