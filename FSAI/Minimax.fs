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
    let rec minimaxAlphaBeta (boardF: byte[,], depthF: int, alpha: int, beta: int, tileF: byte, isMaxPlayerF: bool, getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners) =
        if ((depthF = 0) || (getWinner(boardF) <> Empty)) then // If leaf node or game over --> Return value of leaf node.
            let evalRes = evaluate (boardF, getValidMoves, getScore, countCorners)
            evalRes
        else 
            let mutable bestScore = 0

            if isMaxPlayerF then
                bestScore <- alpha // bestScore = -infinity
            else
                bestScore <- beta // bestScore = +infinity

            let validMoves: ResizeArray<Tuple<int, int>> = getValidMoves boardF tileF // Get values from getValidMoves and convert into to a ResizeArray-type
            let isOverZeroMoves = (0 < validMoves.Count)

            match isOverZeroMoves with
            | true ->
                let mutable i: int = 0

                while i < validMoves.Count do
                    let childBoard: byte[,] = Array2D.copy boardF

                    // Fetch current move in the current iteration and call makeMove.
                    let move = validMoves.[i]
                    makeMove childBoard move tileF

                    // Fetch value from child.
                    let nodeScore = minimaxAlphaBeta(childBoard, depthF-1, alpha, beta, otherTile(tileF), (not isMaxPlayerF), getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners)

                    match isMaxPlayerF with
                    | true ->
                        bestScore <- Math.Max(bestScore, nodeScore)
                        alpha = Math.Max(bestScore, alpha)
                    | false ->
                        bestScore <- Math.Min(bestScore, nodeScore)
                        beta = Math.Min(bestScore, beta)
                    |> ignore
                       
                    match beta <= alpha with
                    | true ->
                        i <- validMoves.Count // Sets i to the size of valiMoves which will break the ongoing while loop.
                    | false ->
                        i <- i + 1 // Increment i with +1 to make the loop move forward to the next validmove.
                    |> ignore

                bestScore
                
            | false -> // If there are no valid moves --> recurse minimax with the opposite value of isMaxPlayer.
                minimaxAlphaBeta(boardF, depthF, alpha, beta, tileF, (not isMaxPlayerF), getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners)
            |> ignore
                
            bestScore     
   

        






        

         
    
    
    
    