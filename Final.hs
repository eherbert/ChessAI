import DataTypes
import PieceInformation
import Solver
import ChessIO
import GameStates
import Data.Maybe
import Data.List
import System.IO
import Data.Char
import Data.List.Split

main :: IO()
main = do
    putStrLn "Welcome to Chess Game"
    introQ
