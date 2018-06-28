let cipher = dict['A' , '@';
  'B' , '8';
  'C' , '(';
  'D' , 'D';
  'E' , '3';
  'F' , 'F';
  'G' , '6';
  'H' , '#';
  'I' , '!';
  'J' , 'J';
  'K' , 'K';
  'L' , '1';
  'M' , 'M';
  'N' , 'N';
  'O' , '0';
  'P' , 'P';
  'Q' , 'Q';
  'R' , 'R';
  'S' , '$';
  'T' , '7';
  'U' , 'U';
  'V' , 'V';
  'W' , 'W';
  'X' , 'X';
  'Y' , 'Y';
  'Z' , '2';
  ' ' , ' ']

let toLeetSpeak s = 
  let l = Seq.toList s
  let lchars = List.map (fun x -> (cipher.Item(x))) l
  System.String(lchars |> Array.ofList)

toLeetSpeak "LEET"
(*
module Tests = begin
    open Fuchu
    
    let suite = 
        testList "Tests" [
            testCase "Basic tests" <| (fun _ -> 
                Assert.Equal("toLeetSpeak \"LEET\"", "1337", toLeetSpeak "LEET");
                Assert.Equal("toLeetSpeak \"CODEWARS\"", "(0D3W@R$", toLeetSpeak "CODEWARS");
                Assert.Equal("toLeetSpeak \"HELLO WORLD\"", "#3110 W0R1D", toLeetSpeak "HELLO WORLD");
                Assert.Equal("toLeetSpeak \"LOREM IPSUM DOLOR SIT AMET\"", "10R3M !P$UM D010R $!7 @M37", toLeetSpeak "LOREM IPSUM DOLOR SIT AMET");
                Assert.Equal("toLeetSpeak \"THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG\"", "7#3 QU!(K 8R0WN F0X JUMP$ 0V3R 7#3 1@2Y D06", toLeetSpeak "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG");                
            )           
        ]
end
*)