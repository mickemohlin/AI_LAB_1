namespace FSAI

open System
open Microsoft.FSharp.Collections

module Minimax =
    let Empty = byte 0
    let White = byte 1
    let Black = byte 2
    let Valid = byte 3
    let Tie = byte 4

    // F# Evaluation
    let evaluate (board: byte[,], getValidMoves, getScore, countCorners) : int =
   
            let evaluation: int = 0
   
            let validMovesBlack: ResizeArray<Tuple<int, int>> = getValidMoves board Black
            let validMovesWhite: ResizeArray<Tuple<int, int>> = getValidMoves board White
   
            let blackMobility: int = validMovesBlack.Count;
            let whiteMobility: int = validMovesWhite.Count;
            let blackScore: int = getScore board Black;
            let whiteScore: int = getScore board White;
   
            printf "Black score: %i White score: %i" blackScore whiteScore
   
            if blackScore = 0 then
                -200000
            else if whiteScore = 0 then
                200000
            else if blackScore + whiteScore = 64 || blackMobility + whiteMobility = 0 then
                if blackScore < whiteScore then
                    -100000 - whiteScore + blackScore
                else if blackScore > whiteScore then
                    100000 + blackScore - whiteScore
                else 
                    0
            else
                let evaluation2 = evaluation + blackScore - whiteScore
   
                if blackScore + whiteScore > 55 then
                    blackScore - whiteScore
   
                else
                    let evaluation3 = evaluation2 + (blackMobility - whiteMobility) * 10
                    let evaluation4 = evaluation3 + ((countCorners board Black) - (countCorners board White)) * 100
   
                    evaluation4
           
    // F# MinimaxAlphaBeta
    let rec minimaxAlphaBeta (boardF: byte[,], depthF: int, minValueF: int, maxValueF: int, tileF: byte, isMaxPlayerF: bool, getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners) =
        if ((depthF = 0) || (getWinner(boardF) <> Empty)) then
            let evalRes = evaluate (boardF, getValidMoves, getScore, countCorners)
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
                    let childBoard: byte[,] = Array2D.copy boardF

                    let move = validMoves.[i]
                    makeMove childBoard move tileF

                    let nodeScore = minimaxAlphaBeta(childBoard, depthF-1, minValueF, maxValueF, otherTile(tileF), (not isMaxPlayerF), getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners)

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
                minimaxAlphaBeta(boardF, depthF, minValueF, maxValueF, tileF, (not isMaxPlayerF), getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners)
            |> ignore
                
            bestScore
   

        






        

         
    
    
    
    