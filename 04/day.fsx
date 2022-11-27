open System

let lines = 
    System.IO.File.ReadAllLines("input")

let numbers = lines[0].Split "," |> Array.map int


type Board = int[][]

let parseBoard (boardLines: string[]): Board =
    boardLines
    |> Array.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

let boards = 
    lines[1..]
    |> Seq.filter (fun s -> s <> "")
    |> Seq.chunkBySize 5
    |> Seq.map parseBoard
    |> Seq.toList

let boardHasBingo (isDrawn: int -> bool) (board: Board) =
    let rows =
        seq { for r in 0..4 do
                            yield board[r] }
    let columns =
        seq { for c in 0..4 do
                            yield [| board[0][c]; board[1][c] ; board[2][c] ; board[3][c] ; board[4][c] |] }
    let hasBingo (numbers: int[]) = numbers |> Seq.forall isDrawn

    Seq.append rows columns
    |> Seq.exists hasBingo


let isDrawn (numbersDrawn: int[]) (number: int) =
    let lookup = numbersDrawn |> Set
    Set.contains number lookup

let findWinningBoard =
    let mutable winningBoard: Board option = None
    let mutable drawIndex = 0
    let mutable numbersDrawn = [||]
    while winningBoard.IsNone do
        drawIndex <- drawIndex + 1
        numbersDrawn <- numbers[0..drawIndex]
        let isDrawnX = isDrawn numbersDrawn
        winningBoard <-
            boards
            |> Seq.tryFind (fun board -> boardHasBingo isDrawnX board)
    winningBoard.Value, numbersDrawn

let sumUnmarked (isDrawn: int -> bool) (board: Board): int =
    board
    |> Array.sumBy (fun row -> row |> Array.sumBy (fun num -> if isDrawn num then 0 else num))

let winningBoard, numbersDrawn = findWinningBoard
let score = (sumUnmarked (isDrawn numbersDrawn) winningBoard) * (Array.last numbersDrawn)
printfn $"Part 1: {score}"
