pragma solidity ^0.6.2;

contract SimpleChess {
    
    struct Position {
        uint color; // 0 = none, 1 = white player, 2 = black player
        uint piece; // 0 = empty, 1 = pawn, 2 = rook, 3 = knight, 4 = bishop, 5 = queen, 6 = king
    }
    
    Position[][] board; // numbering starts from bottom-left corner and extends up and to the right
    
    // TODO further attributes?
    
    
    /**
     * Configures the starting position of the game
     */
    constructor() public {
        board[7][0] = Position({color: 2, piece: 2}); board[7][1] = Position({color: 2, piece: 3}); board[7][2] = Position({color: 2, piece: 4}); board[7][3] = Position({color: 2, piece: 6}); 
        board[7][4] = Position({color: 2, piece: 5}); board[7][5] = Position({color: 2, piece: 4}); board[7][6] = Position({color: 2, piece: 3}); board[7][7] = Position({color: 2, piece: 2});
        board[6][0] = Position({color: 2, piece: 1}); board[6][1] = Position({color: 2, piece: 1}); board[6][2] = Position({color: 2, piece: 1}); board[6][3] = Position({color: 2, piece: 1}); 
        board[6][4] = Position({color: 2, piece: 1}); board[6][5] = Position({color: 2, piece: 1}); board[6][6] = Position({color: 2, piece: 1}); board[6][7] = Position({color: 2, piece: 1});
        board[1][0] = Position({color: 1, piece: 1}); board[1][1] = Position({color: 1, piece: 1}); board[1][2] = Position({color: 1, piece: 1}); board[1][3] = Position({color: 1, piece: 1}); 
        board[1][4] = Position({color: 1, piece: 1}); board[1][5] = Position({color: 1, piece: 1}); board[1][6] = Position({color: 1, piece: 1}); board[1][6] = Position({color: 1, piece: 1});
        board[0][0] = Position({color: 1, piece: 2}); board[0][1] = Position({color: 1, piece: 3}); board[0][2] = Position({color: 1, piece: 4}); board[0][3] = Position({color: 1, piece: 6}); 
        board[0][4] = Position({color: 1, piece: 5}); board[0][5] = Position({color: 1, piece: 4}); board[0][6] = Position({color: 1, piece: 3}); board[0][7] = Position({color: 1, piece: 2});
    }

    /**
     * Determine whether the resulting position is "sending check"
     */
    function verifyCheck(address movingPlayer) private view return (bool isCheck, isCheckMate) {
        // DO NOT NEED TO FILL IN
    }
    
    /**
     * Determine whether the resulting position represents a draw 
     */
    function verifyDraw(address movingPlayer) private view return (bool isDraw) {
        // DO NOT NEED TO FILL IN
    }

    /**
     * Performs a move on the board
     */
    function move(uint startRow, uint startColumn, uint endRow, uint endColumn) public {
        require(startRow <= 7 || startColumn <= 7 || endRow <= 7 || endColumn <= 7, "row or column out of bounds");
        // require that the move is valid DO NOT NEED TO FILL IN
        // require that the resulting position is not "receiving check" DO NOT NEED TO FILL IN
        // TODO any other requires?

        // moving the piece to the the end position
        board[endRow][endColumn] = Position({
            color: board[startRow][startColumn].color,
            piece: board[startRow][startColumn].piece
        });
        // setting the start position to empty
        board[startRow][startColumn] = Position({
            color: 0,
            piece: 0
        });

        // TODO anything else missing?

    }
    
    /**
     * Retrieve the winning pot or just his own stake in case of a draw
     */
    function withdraw() {
        // TODO
        //
        //
    }
    
    // TODO further functions?

}