namespace FSAI

open System

module Minimax =
    let Empty = byte 0
    let White = byte 1
    let Black = byte 2
    let Valid = byte 3
    let Tie = byte 4

    // F# Evaluation
    let evaluate (board: byte[,], getValidMoves, getScore, countCorners) : int =
        let mutable evaluation: int = 0
   
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

        else 
            // If game is over ( max score = 64 or no more moves can be made )
            if (blackScore + whiteScore) = 64 || (blackMobility + whiteMobility) = 0 then
                let black = int Black
                
                if black < whiteScore then
                    -100000 - whiteScore + blackScore
                else if blackScore > whiteScore then
                    100000 + blackScore - whiteScore
                else
                    0
            else
                // returns evaluation depending on the score
                evaluation <- evaluation + (blackScore - whiteScore)
   
                if blackScore + whiteScore > 55 then
                    (blackScore - whiteScore)
                else
                    // if black has fewer moves than white -> negative evaluation, worse evaluation
                    evaluation <- evaluation + ((blackMobility - whiteMobility) * 10)
                    evaluation <- evaluation + (((countCorners board Black) - (countCorners board White)) * 100)
   
                    evaluation
           


    // F# MinimaxAlphaBeta
    let rec minimaxAlphaBeta (boardF: byte[,], depthF: int, alpha: byref<int>, beta: byref<int>, tileF: byte, isMaxPlayerF: bool, getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners) =  
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

            let mutable result: int = 0

            match isOverZeroMoves with
            | true ->
                let mutable i: int = 0

                while i < validMoves.Count do
                    let childBoard: byte[,] = Array2D.copy boardF

                    // Fetch current move in the current iteration and call makeMove.
                    let move = validMoves.[i]
                    makeMove childBoard move tileF

                    // Fetch value from child.
                    let nodeScore = minimaxAlphaBeta(childBoard, (depthF-1), &alpha, &beta, otherTile(tileF), (not isMaxPlayerF), getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners)

                    match isMaxPlayerF with
                    | true ->
                        bestScore <- Math.Max(bestScore, nodeScore)
                        alpha <- Math.Max(bestScore, alpha)
                    | false ->
                        bestScore <- Math.Min(bestScore, nodeScore)
                        beta <- Math.Min(bestScore, beta)          
                       
                    match beta <= alpha with
                    | true ->
                        i <- validMoves.Count // Sets i to the size of valiMoves which will break the ongoing while loop.
                    | false ->
                        i <- i + 1 // Increment i with +1 to make the loop move forward to the next validmove.
                    |> ignore
                    
                result <- bestScore   
                
            | false -> // If there are no valid moves --> recurse minimax with the opposite value of isMaxPlayer.
                result <- minimaxAlphaBeta(boardF, depthF, &alpha, &beta, otherTile(tileF), (not isMaxPlayerF), getValidMoves, makeMove, getWinner, otherTile, getScore, countCorners)
                
            result



        

         
    
    
    
    