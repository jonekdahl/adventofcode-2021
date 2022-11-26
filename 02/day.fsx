
type Instruction =
    | Forward of int
    | Down of int
    | Up of int

let instructions =
    System.IO.File.ReadAllLines("input")
    |> Seq.map (fun s ->  s.Split " " |> List.ofSeq)
    |> Seq.map (fun xs -> xs[0], xs[1])
    |> Seq.map (fun (p1, p2) ->
        let i = int p2
        match p1 with
        | "forward" -> Forward i
        | "down" -> Down i
        | "up" -> Up i)

type Pos = {
    hpos: int
    depth: int
}

let initialPos = { hpos = 0; depth = 0 }

let folder1 (pos: Pos) (instr: Instruction): Pos =
    match instr with
    | Forward i -> { pos with hpos = pos.hpos + i }
    | Up i -> { pos with depth = pos.depth - i }
    | Down i -> { pos with depth = pos.depth + i }

instructions
|> Seq.fold folder1 initialPos 
|> fun p -> printfn $"Part 1: {p.hpos * p.depth}"

type Pos3 = {
    hpos: int
    depth: int
    aim: int
}

let initialPos3 = { hpos = 0; depth = 0; aim = 0 }

let folder2 (pos: Pos3) (instr: Instruction): Pos3 =
    match instr with
    | Forward i -> 
        { pos with 
            hpos = pos.hpos + i
            depth = pos.depth + pos.aim * i }
    | Up i -> { pos with aim = pos.aim - i }
    | Down i -> { pos with aim = pos.aim + i }

instructions
|> Seq.fold folder2 initialPos3 
|> fun p -> printfn $"Part 2: {p.hpos * p.depth}"
