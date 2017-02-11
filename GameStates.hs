module GameStates where
import DataTypes

playerOnePieces :: [(PieceType, (Int, Int))]
playerOnePieces = [(Rook, (0,0)),
       (Knight, (1,0)),
       (Bishop, (2,0)),
       (Queen, (3,0)),
       (King, (4,0)),
       (Bishop, (5,0)),
       (Knight, (6,0)),
       (Rook, (7,0)),
       (Pawn, (0,1)),
       (Pawn, (1,1)),
       (Pawn, (2,1)),
       (Pawn, (3,1)),
       (Pawn, (4,1)),
       (Pawn, (5,1)),
       (Pawn, (6,1)),
       (Pawn, (7,1))]
playerTwoPieces :: [(PieceType, (Int, Int))]
playerTwoPieces = [(Rook, (0,7)),
       (Knight, (1,7)),
       (Bishop, (2,7)),
       (Queen, (3,7)),
       (King, (4,7)),
       (Bishop, (5,7)),
       (Knight, (6,7)),
       (Rook, (7,7)),
       (Pawn, (0,6)),
       (Pawn, (1,6)),
       (Pawn, (2,6)),
       (Pawn, (3,6)),
       (Pawn, (4,6)),
       (Pawn, (5,6)),
       (Pawn, (6,6)),
       (Pawn, (7,6))]
initialBoard = Board playerOnePieces playerTwoPieces
initialGameState = GameState initialBoard PlayerOne 60

testOnePieces1 = [(Pawn, (0,1)),(King, (6,6))]
testTwoPieces1 = [(King, (1,2))]
testBoard1 = Board testOnePieces1 testTwoPieces1
testGameState1 = GameState testBoard1 PlayerTwo 2

testOnePieces2 = [(Pawn, (0,1)),(King, (6,6))]
testTwoPieces2 = [(King, (1,2)),(Bishop, (2,2))]
testBoard2 = Board testOnePieces2 testTwoPieces2
testGameState2 = GameState testBoard2 PlayerTwo 3

testOnePieces3 = [(Queen, (3,5)),(Rook, (2,4)),(King,(6,6))]
testTwoPieces3 = [(King, (0,5)),(Rook, (1,5))]
testBoard3 = Board testOnePieces3 testTwoPieces3
testGameState3 = GameState testBoard3 PlayerTwo 4

testOnePieces4 = [(Rook, (3,4)),(Rook, (2,3)),(King,(6,6))]
testTwoPieces4 = [(King, (0,5))]
testBoard4 = Board testOnePieces4 testTwoPieces4
testGameState4 = GameState testBoard4 PlayerTwo 4

testOnePieces5 = [(Bishop, (0,5)),(Queen, (3,6)),(Pawn, (2,5)),(Pawn, (2,2)),(Bishop, (2,0)),(Knight,(1,0)),(Rook,(0,0)),(Pawn, (0,1)),(Pawn, (1,1)),(King,(4,0)),(Knight, (6,0)),(Rook, (7,0)),(Pawn, (7,1)),(Pawn, (6,1)),(Pawn, (5,1))]
testTwoPieces5 = [(King, (4,7)),(Rook, (1,7)),(Pawn, (0,6)),(Pawn, (1,5)),(King, (4,7)),(Pawn, (4,6)),(Bishop, (5,7)),(Pawn, (5,4)),(Pawn, (6,5)),(Pawn, (7,4)),(Knight, (7,5)),(Rook, (7,7))]
testBoard5 = Board testOnePieces5 testTwoPieces5
testGameState5 = GameState testBoard5 PlayerTwo 30

testOnePieces6 = [(King,(0,0)),(Pawn,(0,6))]
testTwoPieces6 = [(King,(0,1))]
testBoard6 = Board testOnePieces6 testTwoPieces6
testGameState6 = GameState testBoard6 PlayerOne 30
