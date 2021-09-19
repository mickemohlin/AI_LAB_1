namespace FSAI

open System
open Microsoft.FSharp.Collections


module Minimax =
    let Empty = byte 0
    let White = byte 1
    let Black = byte 2
    let Valid = byte 3
    let Tie = byte 4

    let rec minimaxAlphaBeta (boardF: byte[,], depthF: int, minValueF: int, maxValueF: int, tileF: byte, isMaxPlayerF: bool, evaluation, getValidMoves, makeMove, getWinner, otherTile) =
        if ((depthF = 0) || (getWinner(boardF) <> Empty)) then
            let evalRes = evaluation boardF 
            evalRes
        else 
            let mutable bestScore = 0

            if isMaxPlayerF then
                bestScore <- minValueF
            else
                bestScore <- maxValueF

            let validMoves: ResizeArray<Tuple<int, int>> = getValidMoves boardF tileF
           
            let isOverZeroMoves = (0 < validMoves.Count)

            match isOverZeroMoves with
            | true ->
                let mutable i: int = 0

                while i < validMoves.Count do
                    let childBoard: byte[,] = boardF // TODO: Shallow copy?

                    let move = validMoves.[i]
                    makeMove childBoard move tileF

                    let nodeScore = minimaxAlphaBeta(childBoard, depthF-1, minValueF, maxValueF, otherTile(tileF), (not isMaxPlayerF), evaluation, getValidMoves, makeMove, getWinner, otherTile)

                    match isMaxPlayerF with
                    | true ->
                        bestScore <- Math.Max(bestScore, nodeScore)
                        minValueF = Math.Max(bestScore, maxValueF)
                    | false ->
                        bestScore <- Math.Min(bestScore, nodeScore)
                        maxValueF = Math.Min(bestScore, maxValueF)
                    |> ignore
                       
                    match maxValueF <= minValueF with
                    | true ->
                        i <- validMoves.Count // Break out of for loop
                    | false ->
                        i <- i + 1
                    |> ignore

                bestScore
                
            | false ->
                minimaxAlphaBeta(boardF, depthF, minValueF, maxValueF, tileF, (not isMaxPlayerF), evaluation, getValidMoves, makeMove, getWinner, otherTile)
            |> ignore
                
            bestScore



    
    
    
    