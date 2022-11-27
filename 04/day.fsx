open System

let lines = 
    System.IO.File.ReadAllLines("input")

let numbers = lines[0].Split "," |> Array.map int


type Board = int[,]

let parseBoard (boardLines: string[]): Board =
    boardLines
    |> Array.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
    |> array2D

let boards = 
    lines[1..]
    |> Seq.filter (fun s -> s <> "")
    |> Seq.chunkBySize 5
    |> Seq.map parseBoard
    |> Seq.toList

let boardHasBingo (isDrawn: int -> bool) (board: Board) =
    let rows =
        seq { for r in 0..4 do
                            yield board[r,*] }
    let columns =
        seq { for c in 0..4 do
                            yield board[*,c] }
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
    let mutable sum = 0
    Array2D.iter (fun num -> if not (isDrawn num) then sum <- sum + num) board
    sum

let winningBoard, numbersDrawn = findWinningBoard
let score = (sumUnmarked (isDrawn numbersDrawn) winningBoard) * (Array.last numbersDrawn)
printfn $"Part 1: {score}"


let findLastWinningBoard (allBoards: Board list) =
    let mutable drawIndex = 0
    let mutable numbersDrawn = [||]

    let rec _find (boards: Board list) =
        let isDrawnX = isDrawn numbersDrawn

        if boards.Length = 1 && (boardHasBingo isDrawnX boards.Head) then
            boards.Head
        else
            drawIndex <- drawIndex + 1
            numbersDrawn <- numbers[0..drawIndex]

            boards
            |> List.filter (fun board -> not (boardHasBingo isDrawnX board))
            |> _find
    
    let lastWinningBoard = _find allBoards
    lastWinningBoard, numbersDrawn
    
let lastWinningBoard, numbersDrawnLast = findLastWinningBoard boards
let scoreLast = (sumUnmarked (isDrawn numbersDrawnLast) lastWinningBoard) * (Array.last numbersDrawnLast)
printfn $"Part 2: {scoreLast}"
