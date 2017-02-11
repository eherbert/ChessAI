module ChessIO where
import DataTypes
import PieceInformation
import GameStates
import Solver
import Text.Read
import System.IO
import Data.Char
import Data.List.Split
import System.Environment
import System.Console.GetOpt
import Data.List

{-
prettyPrintPiece :: PieceType -> Player -> String
prettyPrintPiece King p = if (p==PlayerOne) then " K1" else " K2"
prettyPrintPiece Queen p = if (p==PlayerOne) then " Q1" else " Q2"
prettyPrintPiece Bishop p = if (p==PlayerOne) then " B1" else " B2"
prettyPrintPiece Knight p = if (p==PlayerOne) then " N1" else " N2"
prettyPrintPiece Rook p = if (p==PlayerOne) then " R1" else " R2"
prettyPrintPiece Pawn p = if (p==PlayerOne) then " P1" else " P2"
-}

prettyPrintPiece :: PieceType -> Player -> String
prettyPrintPiece King p = if (p==PlayerTwo) then " ♔ " else " ♚ "
prettyPrintPiece Queen p = if (p==PlayerTwo) then " ♕ " else " ♛ "
prettyPrintPiece Bishop p = if (p==PlayerTwo) then " ♗ " else " ♝ "
prettyPrintPiece Knight p = if (p==PlayerTwo) then " ♘ " else " ♞ "
prettyPrintPiece Rook p = if (p==PlayerTwo) then " ♖ " else " ♜ "
prettyPrintPiece Pawn p = if (p==PlayerTwo) then " ♙ " else " ♟ "

prettyPrintRow :: Int -> [Piece] -> [Piece] -> [Char]
prettyPrintRow a p1Pieces p2Pieces = (intToDigit (a+1)) : " | " ++ (foldr (++) " | " ([ if (locationExistsInList (b,a) (p1Pieces++p2Pieces)) then (prettyPrintPiece (fst (findPieceFromLocation (b,a) (p1Pieces++p2Pieces))) (if (locationExistsInList (b,a) p1Pieces) then PlayerOne else PlayerTwo)) else (" □ ") | b<-[0..7] ]))

prettyPrint :: GameState -> IO()
prettyPrint (GameState (Board p1Pieces p2Pieces) gsp tc) =
   let prettyBoard = [prettyPrintRow a p1Pieces p2Pieces | a<-[7,6..0]]
       dashedLine = ["   --------------------------"]
       numLine = ["     1  2  3  4  5  6  7  8"]
   in putStr (unlines (dashedLine++(prettyBoard++(dashedLine++numLine))))

{-
createPieces :: [String] -> [Piece]
createPieces (x:y:z) =
   if ((read x) == "p2List") then [] else ((readPieceType x),((read y), read(head z))):createPieces (tail z)

createList :: [String] -> [Piece]
createList (x:xs) =
   if (x=="P1List") then createPieces xs else createList xs

readCoord :: String -> Maybe Int
readCoord = undefined
readType :: String -> Maybe PieceType
readType = undefined

readPiece :: String -> Maybe Piece
readPiece str =
   case words str of
      [tStr, xStr, yStr] ->
          do t <- readType  tStr
             x <- readCoord xStr
             y <- readCoord ySTr
             Just (t, (x,y))
-}
{-
moveIO :: Move -> GameState -> String --GameState
moveIO x (GameState b pt tc) =
  let a = move x b pt
	in if (a == Nothing) then "Not a valid move."
	else show(GameState (unJust a) pt tc)
-}

options :: [OptDescr Flag]
options = [
    Option ['w'] ["winner"] (NoArg FinalWinner ) "Print out who will win the game.",
    Option ['h'] ["help"] (NoArg Help) "Print a help message and exit.",
    Option ['d'] ["depth num"] (ReqArg Depth "num") "num as a cut-off depth.",
    Option ['m'] ["move"] (NoArg NextMove) "Make a move and print out resulting board.",
    Option ['v'] ["verbose"] (NoArg Verbose) "Return a move rating.",
    Option ['i'] ["interactive"] (NoArg Interactive) "Play a game against the computer."
 ]

findDepth :: [Flag] -> GameState -> Int
findDepth [] (GameState (Board p1 p2) p tc) = if ((length p1)+(length p2))<16 then 5 else 4
findDepth ((Depth n):xs) gs = read n
findDepth (_:xs) gs = findDepth xs gs

showPiece:: Piece -> String
showPiece (x,(y,z)) = unwords [(show x),(show y),(show z)]

showBoard :: Board -> String
showBoard (Board p1List p2List) =
  let p1 = intercalate " | " (map showPiece p1List) --or intercalate "|" (map showPiece p1List)
      p2 = intercalate " | " (map showPiece p2List)
  in p1++"\n"++p2  --or p1 ++ "\n" ++ p2

showGameState :: GameState -> String
showGameState (GameState b p tc) = (show p)++"\n"++(show tc)++"\n"++(showBoard initialBoard)

-- cases to check if correct type!!!
readInt:: String -> Maybe Int
readInt x =
   case all isDigit x of
      True -> Just $ read x
      False -> Nothing

{-
unJust :: Maybe a -> a
unJust (Just x) = x
-}

--readMaybe a = String => Maybe a
readPieceType :: String -> Maybe PieceType
--readPieceType x =  readMaybe x
readPieceType "King" = Just King
readPieceType "Queen" = Just Queen
readPieceType "Pawn" = Just Pawn
readPieceType "Rook" = Just Rook
readPieceType "Knight" = Just Knight
readPieceType "Bishop" = Just Bishop
readPieceType _ = Nothing

unJustPieceType :: Maybe PieceType -> PieceType
unJustPieceType (Just King) = King
unJustPieceType (Just Queen) = Queen
unJustPieceType (Just Pawn) = Pawn
unJustPieceType (Just Rook) = Rook
unJustPieceType (Just Knight) = Knight
unJustPieceType (Just Bishop) = Bishop

{- --
 case x of
   "King" -> Just King
     "Pawn" -> ....

         _ -> Nothing
    --}

readPiece::String -> Maybe Piece
readPiece z =
  let (p:x:y) = words z
      pt = readPieceType p
      a = readInt x
      b = readInt (head y)
   --   in Just(unJustPieceType pt,(unJustInt a, unJustInt b))
  in case (pt,(a,b)) of
     (Nothing , _ ) -> Nothing
     (_, (Nothing , _)) -> Nothing
     (_, (_ , Nothing)) -> Nothing
     _ -> Just(unJustPieceType pt,(unJust a, unJust b))

readBoard::[String] -> Maybe Board
readBoard (p1:p2) =
  let p1L= splitOn " | " p1
      p2L= splitOn " | " (head p2)
--  in Board (map readPiece p1L) (map readPiece p2L)
      readp1L = map readPiece p1L
      readp2L = map readPiece p2L
      cList = readp1L ++ readp2L
-- combine both list and check if any of them are Nothing! what
  in case Nothing `elem` cList of
      True -> Nothing
      False -> Just (Board (map unJust readp1L) (map unJust readp2L))

readPlayer :: String -> Maybe Player
readPlayer x =
  case x of
     "PlayerOne" -> Just PlayerOne
     "PlayerTwo" -> Just PlayerTwo
     _ -> Nothing

readGameState :: String -> Maybe GameState
readGameState a =
  let (pt:tc:pl) = lines a
      checkPT = readPlayer pt
      checkTC = readInt tc
      checkB = readBoard pl
  in  case (checkB,checkTC,checkPT) of
         (Nothing, _, _) -> Nothing
         (_, Nothing, _) -> Nothing
         (_, _, Nothing) -> Nothing
         _ -> Just (GameState (unJust checkB) (unJust checkPT) (unJust checkTC))

removePunc xs = [ x | x <- xs, not (x `elem` "()") ]

readLoc :: String -> Maybe Location
readLoc given =
   let a = removePunc given
       (b:c) = splitOn "," a
       x = readInt b
       y = readInt (head c)
   in case (x,y) of
      (Nothing,_) -> Nothing
      (_,Nothing) -> Nothing
      (_,_) -> Just ((unJust x)-1,(unJust y)-1)
      --_ -> Nothing




   {-
   grabFlag :: [Flag] -> String
   grabFlag flags =
       case [s | NextMove s <- flags] of
   	[] -> ""
           [s] -> read s
           _ -> error "too much flags"
   -}

getWinner :: Maybe Winner -> String
getWinner x =
  if (x == Nothing) then "Game is still in play"
  else show (unJust x)

{-
main :: IO()
main = do
    putStrLn "Welcome to Chess Game"
    introQ
-}

introQ = do
    args <- getArgs
    let (flags, others, errors) = getOpt Permute options args
    if Help `elem` flags
    then putStrLn $ usageInfo "CHESS" options
    else do
       putStrLn "Would you like to open a file with a GameState? (y/n) n = GameState -> File"
       answer <- getLine
       if (all isAlpha answer)== True
       then if answer == "y"
          then openAFile flags
          else if answer == "n"
             then letsWriteToFile
             else putStrLn "not y or n"
       else putStrLn "not valid input"

   --runFW flags

   --runMove flags gs = do
   --     let a = checkMoveIO

checkGameState flags gs = do
  if gs == Nothing
  then putStrLn "File GameState is Incorrect"
  else testFlags flags (unJust gs)

openAFile flags = do
    handle <- openFile "testing.txt" ReadMode
    text <- hGetContents handle
    let a = readGameState text
    checkGameState flags a
   --         gTwo = grabFlag flags

testFlags flags a = do
    prettyPrint a
    if FinalWinner `elem` flags
    --then putStrLn (getWinner(whoWon a))
    --then putStrLn (show (whoWillWin a 60))
    then putStrLn (show (whoWillWin a))
    else do
         if NextMove `elem` flags then getMove flags a
         --else if Verbose `elem` flags then doVerbose flags a
         else if Interactive `elem` flags then doInteractive flags a
         else do makeMove flags a
            --putStrLn "The best move for the current player is:"
            --putStrLn (show (doMath (bestMoveDepth a (findDepth flags))))

makeMove flags a = do
    let theMove = doMath (bestMoveDepth a (findDepth flags a))
    putStrLn (show theMove)
    if (Verbose `elem` flags) then doVerbose theMove a else return ()
    {-
    let nextGS = moveToNextGameState theMove a
    let ret = whoWon (unJust nextGS)
    if (ret==Nothing) then putStrLn "This move did not result in a Win, Loss, or Tie."
    else do
        putStr "This move resulting in "
        putStrLn (show (unJust ret))
    -}

doVerbose x a = do
    let nextGS = moveToNextGameState x a
    if (nextGS==Nothing) then putStrLn "This move does not result in a Win, Loss, or Tie."
    else do
        putStr "This move will result in "
        putStrLn (show (unJust (whoWon (unJust nextGS))))

doMath :: Move -> Move
doMath ((x,y),(a,b)) = ((x+1,y+1),(a+1,b+1))

doInteractive flags gs@(GameState (Board p1Piece p2Pieces) p tc) = do
  putStrLn "Your turn."
  putStrLn "Enter the location of the piece that you wish to move."
  putStrLn "Ex: (2,2) (x axis, y axis)"
  putStr ">>> "
  l1 <- getLine
  let c1 = readLoc l1
  if c1 == Nothing
  then notValidInputI flags gs
  else do
     putStrLn "Enter the location to where you wish to move the piece."
     putStrLn "Ex: (2,3) (x axis, y axis)"
     putStr ">>> "
     l2 <- getLine
     let c2 = readLoc l2
     if c2 == Nothing
     then notValidInputI flags gs
     else do
        --let gs2 = (moveToNextGameState (unJust c1, unJust c2) gs)
        if((moveToNextGameState (unJust c1, unJust c2) gs)==Nothing) then doInteractive flags gs
        else do
            let gs2 = (moveToNextGameState (unJust c1, unJust c2) gs)
            --let tmp = map (\x -> (whoWillWin x)) (allPossibleGameStates (unJust gs2))
            --putStrLn (show tmp)
            prettyPrint (unJust gs2)
            if((whoWon (unJust gs2))==Nothing) then computersTurn flags (unJust gs2)
            else if((whoWon gs)==(Just Tie)) then putStrLn "Good day."
            else putStrLn "You won!"
 
computersTurn flags a = do
  --let gs2 = (moveToNextGameState (((unJust c1)-1, (unJust c2)-1)) a)
  putStrLn "The computer's turn."
  putStrLn "The computer is thinking..."
  let gs = unJust (moveToNextGameState (bestMoveDepth a (findDepth flags a)) a)
  prettyPrint gs
  if((whoWon gs)==Nothing) then doInteractive flags gs
  else if((whoWon gs)==(Just Tie)) then putStrLn "Good day."
  else putStrLn "The computer won!!!!!!!!!!!!!!!"

notValidInputI flags a = do
  putStrLn "Not a Valid Input! Look at ex:"
  doInteractive flags a

notValidInput flags a = do
  putStrLn "Not a Valid Input! Look at ex:"
  getMove flags a

getMove flags gs = do
  putStrLn "Place Location of Piece you want to Move ex: (2,2) (x axis, y axis) "
  l1 <- getLine
  let c1 = readLoc l1
  if c1 == Nothing
  then notValidInput flags gs
  else do
     putStrLn "Place Location you want piece to move to ex: (2,3) (x axis, y axis) "
     l2 <- getLine
     let c2 = readLoc l2
     if c2 == Nothing
     then notValidInput flags gs
     else prettyPrint (unJust (moveToNextGameState ((unJust c1, unJust c2)) gs))

letsWriteToFile = do
   putStrLn "You wrote a file check folder"
   writeFile "testing.txt"  (showGameState initialGameState)
