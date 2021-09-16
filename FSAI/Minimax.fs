namespace FSAI

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



    
    
    
    