module DataTypes where

data Winner = Won Player | Tie deriving (Show, Eq)

type Location = (Int,Int)
type Move = (Location, Location)

data PieceType = King | Queen | Bishop | Rook | Knight | Pawn deriving (Read, Show, Eq)
type Piece = (PieceType, Location)

data Player = PlayerOne | PlayerTwo deriving (Read,Show, Eq)

type TurnCount = Int
data Board = Board [Piece] [Piece] deriving (Show, Eq)
data GameState = GameState Board Player TurnCount deriving (Show, Eq)

data Flag = FinalWinner | Help | Depth String | NextMove | Interactive | Verbose deriving (Eq, Show)
