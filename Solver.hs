module Solver where
import DataTypes
import PieceInformation
import Data.Maybe

piecePoints :: PieceType -> Int
piecePoints Pawn = 1
piecePoints Knight = 3
piecePoints Bishop = 3
piecePoints Rook = 5
piecePoints Queen = 9
piecePoints King = 100

calculatePoints :: [Piece] -> Int
calculatePoints lst =  foldr (\x y -> (piecePoints x)+y) 0 (map (\x -> fst x) lst)

currentScore :: GameState -> Int
--currentScore (GameState (Board p1 p2) gsp tc) = (calculatePoints p2) - (calculatePoints p1)
currentScore (GameState b gsp tc) = (calculatePoints (currentPieces b gsp)) - (calculatePoints (enemyPieces b gsp))

currentPieces :: Board -> Player -> [Piece]
currentPieces (Board p1Pieces p2Pieces) PlayerOne = p1Pieces
currentPieces (Board p1Pieces p2Pieces) PlayerTwo = p2Pieces

enemyPieces :: Board -> Player -> [Piece]
enemyPieces (Board p1Pieces p2Pieces) PlayerOne = p2Pieces
enemyPieces (Board p1Pieces p2Pieces) PlayerTwo = p1Pieces

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

transposePieces :: [Piece] -> Location -> Location -> [Piece]
transposePieces [] loc1 loc2 = []
transposePieces (x@(Pawn,loc):xs) loc1 loc2 =
    if loc==loc1
    then
        if ((snd loc2)==7 || (snd loc2)==0)
        then (Queen,loc2):(transposePieces xs loc1 loc2)
        else (Pawn,loc2):(transposePieces xs loc1 loc2)
    else x:(transposePieces xs loc1 loc2)
transposePieces (x@(_,loc):xs) loc1 loc2 =
    if loc==loc1
    then (fst x,loc2):(transposePieces xs loc1 loc2)
    else x:(transposePieces xs loc1 loc2)

move :: Move -> Board -> Player -> Maybe Board
move m@(loc1, loc2) b@(Board p1Pieces p2Pieces) bp =
    let myPieces = currentPieces b bp
        oppPieces = enemyPieces b bp
        piece = findPieceFromLocation loc1 myPieces
        newOppPieces = if (locationExistsInList loc2 oppPieces)
                       then filter (\x -> not ((snd x)==loc2)) oppPieces
                       else oppPieces
        --newMyPieces = map (\x -> if((snd x)==loc1) then (fst x, loc2) else x) myPieces
        newMyPieces = transposePieces myPieces loc1 loc2
    in if (validDest bp piece loc2 myPieces oppPieces)
       then case bp of
              PlayerOne -> Just (Board newMyPieces newOppPieces)
              PlayerTwo ->  Just (Board newOppPieces newMyPieces)
       else Nothing

--nextGameState :: Move -> GameState -> GameState
--nextGameState (Maybe m) gs@(GameState b gsp tc) = GameState (move m b gsp) (opponent gsp) (tc-1)

moveToNextGameState :: Move -> GameState -> Maybe GameState
moveToNextGameState m gs@(GameState b gsp tc) =
    let newBoard = move m b gsp
        state = case newBoard of
                    Just value -> if(gsp==PlayerTwo) then Just (GameState value (opponent gsp) (tc-1)) else Just (GameState value (opponent gsp) tc)
                    Nothing -> Nothing
    in state 

allPossibleMoves (GameState b@(Board p1Pieces p2Pieces) gsp tc) =
    let possibleMoves x = [ (snd x, y) | y<-(validDestinations gsp x (currentPieces b gsp) (enemyPieces b gsp))]
        accPossibleMoves = map (possibleMoves) (currentPieces b gsp)
    in foldr (++) [] accPossibleMoves

allPossibleGameStates gs@(GameState b@(Board p1Pieces p2Pieces) gsp tc) =
    let moves = allPossibleMoves gs
    in (map (\x -> moveToNextGameState x gs) moves)

whoWon :: GameState -> Maybe Winner
whoWon (GameState (Board p1 p2) gsp tc)
    | not (King `elem` (map (fst) p1)) = Just (Won PlayerTwo)
    | not (King `elem` (map (fst) p2)) = Just (Won PlayerOne)
    | (tc == 0) = Just Tie
    | otherwise = Nothing

whoWillWin gs@(GameState (Board p1 p2) gsp tc) = 
    let res = snd (alphaBetaMax (((0,0),(0,0)),gs) (((0,0),(0,0)),-200) (((0,0),(0,0)),200) 60 0 ((0,0),(0,0)))
    in if(res>0) then Won PlayerOne
       else if(res<0) then Won PlayerTwo
       else Tie

{-
bestMove :: GameState -> Move
bestMove gs@(GameState b gsp tc) =
    let nextList = map (\x -> (x,moveToNextGameState x gs)) (allPossibleMoves gs)
    in fst (fst (bestScenario gsp nextList 60))

bestMoveDepth :: GameState -> Int -> Move
bestMoveDepth gs@(GameState b gsp tc) n =
    let nextList = map (\x -> (x,moveToNextGameState x gs)) (allPossibleMoves gs)
    in fst (fst (bestScenario gsp nextList n))
-}

unJust :: Maybe a -> a
unJust (Just x) = x

maxi :: GameState -> Int -> Int
maxi gs@(GameState b gsp tc) 0 = currentScore gs
maxi gs@(GameState b gsp tc) n =
    let nextMoves = catMaybes (allPossibleGameStates gs)
        next x = maxi x (n-1)
        resLst = map next nextMoves
        best = foldr (\x y -> if x>y then x else y) (head resLst) resLst
    in if (not ((whoWon gs)==Nothing)) then currentScore gs
       else if (length nextMoves)==0 then (currentScore gs)-100
       else best

mini :: GameState -> Int -> Int
mini gs@(GameState b gsp tc) 0 = currentScore gs
mini gs@(GameState b gsp tc) n =
    let nextMoves = catMaybes (allPossibleGameStates gs)
        next x = maxi x (n-1)
        resLst = map next nextMoves
        best = foldr (\x y -> if x<y then x else y) (head resLst) resLst
    in if (not ((whoWon gs)==Nothing)) then currentScore gs
       else if (length nextMoves)==0 then (currentScore gs)-100
       else best

{-
bestMove gs@(GameState b gsp tc) n = undefined
bestMoveDepth :: GameState -> Int -> Move
bestMoveDepth gs@(GameState b gsp tc) n =
    let nextMoves = map (\x -> (x,unJust (moveToNextGameState x gs))) (allPossibleMoves gs)
        resList = map (\x -> (fst x, maxi (snd x) n)) nextMoves
        best lst = foldr (\x y -> if (snd x)>(snd y) then x else y) (head lst) lst
    in fst (best resList)
-}

alphaBetaMaxCalculate :: [(Move,Int)] -> (Move,Int) -> (Move,Int) -> (Move,Int)
alphaBetaMaxCalculate [] alpha beta = alpha
alphaBetaMaxCalculate (x:xs) alpha beta =
    let recSol1 = alphaBetaMaxCalculate xs x beta
        recSol2 = alphaBetaMaxCalculate xs alpha beta
    in if ((snd x)>=(snd beta)) then beta
       else if ((snd x)>(snd alpha)) then recSol1
       else recSol2

alphaBetaMax :: (Move,GameState) -> (Move,Int) -> (Move,Int) -> Int -> Int -> Move -> (Move, Int)
alphaBetaMax (m,gs) alpha beta 0 y res = (res,currentScore gs)
alphaBetaMax (m,gs) alpha beta n y res =
    let nextMoves = (allPossibleMoves gs)
        nextGS = map (\x -> (x, moveToNextGameState x gs)) nextMoves
        next x = if y==0 then alphaBetaMax x alpha beta (n-1) (y+1) (fst x) else alphaBetaMax x alpha beta (n-1) (y+1) res
        resLst = if y>0 then map next (map (\x -> (res,unJust (snd x))) nextGS) else map next (map (\x -> (fst x,unJust (snd x))) nextGS)
        best = alphaBetaMaxCalculate resLst alpha beta
    in if (not ((whoWon gs)==Nothing)) then (res,currentScore gs)
       else if (length nextMoves)==0 then (res,(currentScore gs)-100)
       else best

bestMoveDepth :: GameState -> Int -> Move
bestMoveDepth gs n = fst (alphaBetaMax (((0,0),(0,0)),gs) (((0,0),(0,0)),-200) (((0,0),(0,0)),200) n 0 ((0,0),(0,0)))

{-
alphaBetaMinCalculate :: [Int] -> Int -> Int -> Int
alphaBetaMinCalculate [] alpha beta = alpha
alphaBetaMinCalculate (x:xs) alpha beta =
    let recSol1 = alphaBetaMinCalculate xs alpha x
        recSol2 = alphaBetaMinCalculate xs alpha beta
    in if (x<=alpha) then alpha
       else if (x<beta) then recSol1
       else recSol2

alphaBetaMin :: GameState -> Int -> Int -> Int -> (Move, Int)
alphaBetaMin gs alpha beta 0 = currentScore gs
alphaBetaMin gs alpha beta n =
    let nextMoves = (allPossibleNextMoves gs)
        nextGS = map (\x -> (x, moveToNextGameState x gs)) nextMoves
        next x = alphaBetaMax x alpha beta (n-1)
        resLst = map (\x -> (fst x, next x)) nextMoves
        best = alphaBetaMinCalculate resLst alpha beta
    in if (not ((whoWon gs)==Nothing)) then currentScore gs
       else if (length nextMoves)==0 then (currentScore gs)-100
       else best

    
bestMove gs@(GameState b gsp tc) n = undefined
bestMoveDepth :: GameState -> Int -> Move
bestMoveDepth gs@(GameState b gsp tc) n =
    let nextMoves = map (\x -> (x,unJust (moveToNextGameState x gs))) (allPossibleMoves gs)
        resList = map (\x -> (fst x, alphaBetaMax (snd x) ((-1)*(200)) 200 n)) nextMoves
        best lst = foldr (\x y -> if (snd x)>(snd y) then x else y) (head lst) lst
    in fst (best resList)
-}


