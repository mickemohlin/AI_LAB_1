namespace FSAI

// TODO: public static int Evaluation(byte[,] board) - Mikael

// TODO: public static List<Tuple<int, int>> GetValidMoves(byte[,] board, byte tile) - Mikael

// TODO: public static void MakeMove(byte[,] board, Tuple<int, int> move, byte tile) - Filip

// TODO: public static byte GetWinner(byte[,] board) - Filip

// TODO: public static int MinimaxAlphaBeta(byte[,] board, int depth, int a, int b, byte tile, bool isMaxPlayer)

module Minimax =
    let minimaxAlphaBeta board depth a b tile isMaxPlayer =
        $"Test Minimax: {board}, {depth}, {a}, {b}, {tile}, {isMaxPlayer}"

    type Game() =
        member this.property = 5
        
        member this.testfunction =
            $"Number: {this.property}"

    
    
    
    