
let readings =
    System.IO.File.ReadAllLines("input")
    |> Seq.toList
    |> Seq.map int

let countIncreases (s: int seq) =
    s
    |> Seq.pairwise
    |> Seq.map (fun (prev, curr) -> if curr > prev then 1 else 0)
    |> Seq.sum


readings
|> countIncreases
|> printfn "Part 1: %d"

readings
|> Seq.windowed 3
|> Seq.map Array.sum
|> countIncreases
|> printfn "Part 2: %d"
