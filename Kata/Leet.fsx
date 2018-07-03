let encode c = 
    match c with
    | 'B' -> '8'
    | 'C' -> '('
    | 'E' -> '3'
    | 'G' -> '6'
    | 'H' -> '#'
    | 'I' -> '!'
    | 'L' -> '1'
    | 'O' -> '0'
    | 'S' -> '$'
    | 'T' -> '7'
    | 'Z' -> '2'
    | _ -> c

let toLeetSpeak s = 
    String.map encode s

toLeetSpeak "LEET"
