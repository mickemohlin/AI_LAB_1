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
   
            // gets the current valid moves for Black and White
            let validMovesBlack: ResizeArray<Tuple<int, int>> = getValidMoves board Black
            let validMovesWhite: ResizeArray<Tuple<int, int>> = getValidMoves board White
            let blackMobility: int = validMovesBlack.Count;
            let whiteMobility: int = validMovesWhite.Count;

            // gets the current score of Black and White 
            let blackScore: int = getScore board Black;
            let whiteScore: int = getScore board White;
      
            // If leaf node leads to a black score of 0, chance of winning terrible
            if blackScore = 0 then
                -200000
            // else high chance of winning
            else if whiteScore = 0 then
                200000

            // If game is over ( max score = 64 or no more moves can be made )
            else if blackScore + whiteScore = 64 || blackMobility + whiteMobility = 0 then
                
                if blackScore < whiteScore then
                    -100000 - whiteScore + blackScore
                else if blackScore > whiteScore then
                    100000 + blackScore - whiteScore
                else 
                    0
            else
            // returns evaluation depending on the score
                let evaluation =  blackScore - whiteScore
   
                if blackScore + whiteScore > 55 then
                    blackScore - whiteScore
    
                else
                    // if black has fewer moves than white -> negative evaluation, worse evaluation
                    let evaluation2 = evaluation + (blackMobility - whiteMobility) * 10
                    let evaluation3 = evaluation2 + ((countCorners board Black) - (countCorners board White)) * 100
   
                    evaluation3
           
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
   

        






        

         
    
    
    
    