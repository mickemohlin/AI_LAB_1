namespace FSAI
open System
open Microsoft.FSharp.Collections

module Minimax =
    let Empty = byte 0
    let White = byte 1
    let Black = byte 2
    let Valid = byte 3
    let Tie = byte 4

    let rec minimaxAlphaBeta (boardF: byte[,], depthF: int, minValueF: int, maxValueF: int, tileF: int, isMaxPlayerF: bool, evaluation, getValidMoves, makeMove, getWinner, otherTile) =
        if ((depthF = 0) || (getWinner(boardF) <> Empty)) then
            let evalRes = evaluation boardF
            let evalResInt:int = evalRes
            evalResInt
        else 
            let mutable bestScore = 0

            if isMaxPlayerF then
                bestScore <- minValueF
            else
                bestScore <- maxValueF

            bestScore


    let evaluate (board: byte[,], getValidMoves) : int =

        let evaluation = 0
        let validMovesBlack: ResizeArray<Tuple<int, int>> = getValidMoves board Black
        let validMovesWhite: ResizeArray<Tuple<int, int>> = getValidMoves board Black
        let blackMobility = validMovesBlack.Count
        let whiteMobility = validMovesWhite.Count
        let blackScore = 0; //GetScore(board, Black);
        let whiteScore = 0; //GetScore(board, White);

        0






        


    
    
    
    