module PieceInformation where
import DataTypes

isOutOfBounds :: (Ord a1, Ord a, Num a1, Num a) => a -> a1 -> Bool
isOutOfBounds x y = ( (x>7) || (x<0) || (y>7) || (y<0) )

inBounds :: (Ord a1, Ord a, Num a1, Num a) => (a, a1) -> Bool
inBounds (x,y) = not (isOutOfBounds x y)

locationExistsInList :: (Int,Int) -> [Piece] -> Bool
locationExistsInList loc [] = False
locationExistsInList loc (x:xs) = if (loc==(snd x)) then True else (locationExistsInList loc xs)

findPieceFromLocation :: (Int,Int) -> [Piece] -> Piece
findPieceFromLocation loc [] = error "Piece not found"
findPieceFromLocation loc (x:xs) = if (loc==(snd x)) then x else (findPieceFromLocation loc xs)

followVector :: (Int,Int) -> (Int,Int) -> Player -> [Piece] -> [Piece] -> [(Int,Int)]
followVector (x,y) (a,b) p myPieces oppPieces =
   let loc = (x+a,y+b)
   in  if isOutOfBounds (fst loc) (snd loc) then []
       else if (locationExistsInList loc myPieces) then []
       else if (locationExistsInList loc oppPieces) then [loc]
       else [loc] ++ (followVector loc (a,b) p myPieces oppPieces)

validDestinations :: Player -> (PieceType, (Int,Int)) -> [(PieceType, (Int,Int))] -> [(PieceType, (Int,Int))] -> [(Int,Int)]
validDestinations p (King, (x,y)) myPieces oppPieces =
    let spaces = [(x+1, y), (x-1, y), (x,y+1), (x,y-1), (x+1,y+1), (x+1,y-1), (x-1,y+1), (x-1,y-1)]
        noCollide = filter (\loc -> not (locationExistsInList loc myPieces)) spaces
    in (filter inBounds noCollide)
validDestinations p (Queen, (x,y)) myPieces oppPieces =
    let dirToSquares dir = followVector (x,y) dir p myPieces oppPieces
        vectorDirs = [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
        vectorList = map dirToSquares vectorDirs
    in foldr (++) [] vectorList
validDestinations p (Rook, (x,y)) myPieces oppPieces =
    let dirToSquares dir = followVector (x,y) dir p myPieces oppPieces
        vectorDirs = [(1,0),(-1,0),(0,1),(0,-1)]
        vectorList = map dirToSquares vectorDirs
    in foldr (++) [] vectorList
validDestinations p (Bishop, (x,y)) myPieces oppPieces =
    let dirToSquares dir = followVector (x,y) dir p myPieces oppPieces
        vectorDirs = [(1,1),(1,-1),(-1,1),(-1,-1)]
        vectorList = map dirToSquares vectorDirs
    in foldr (++) [] vectorList
validDestinations p (Knight, (x,y)) myPieces oppPieces =
    let check loc = if (not (locationExistsInList loc myPieces)) then [loc] else []
        vectorDirs = [(x+1,y-2),(x+2,y-1),(x+1,y+2),(x+2,y+1),(x-1,y+2),(x-2,y+1),(x-2,y-1),(x-1,y-2)]
        vectorList = map check vectorDirs
    in filter inBounds (foldr (++) [] vectorList)
validDestinations p (Pawn, (x,y)) myPieces oppPieces =
    filter (\a -> (isOutOfBounds (fst a) (snd a))==False)
    (if p == PlayerOne
    then (
        if (y==1)
        then (
            (if ((locationExistsInList (x,y+1) (myPieces++oppPieces))==False) then ([(x,y+1)]) else ([])) ++
            (if ((locationExistsInList (x,y+2) (myPieces++oppPieces))==False && (locationExistsInList (x,y+1) (myPieces++oppPieces))==False) then ([(x,y+2)]) else ([])) ++
            (if (locationExistsInList (x+1,y+1) oppPieces) then ([(x+1,y+1)]) else ([])) ++
            (if (locationExistsInList (x-1,y+1) oppPieces) then ([(x-1,y+1)]) else ([]))
        )
        else (
            (if ((locationExistsInList (x,y+1) (myPieces++oppPieces))==False) then ([(x,y+1)]) else ([])) ++
            (if (locationExistsInList (x+1,y+1) oppPieces) then ([(x+1,y+1)]) else ([])) ++
            (if (locationExistsInList (x-1,y+1) oppPieces) then ([(x-1,y+1)]) else ([]))
        )
    )
    else (
        if (y==6)
        then (
            (if ((locationExistsInList (x,y-1) (myPieces++oppPieces))==False) then ([(x,y-1)]) else ([])) ++
            (if ((locationExistsInList (x,y-2) (myPieces++oppPieces))==False && (locationExistsInList (x,y-1) (myPieces++oppPieces))==False) then ([(x,y-2)]) else ([])) ++
            (if (locationExistsInList (x+1,y-1) oppPieces) then ([(x+1,y-1)]) else ([])) ++
            (if (locationExistsInList (x-1,y-1) oppPieces) then ([(x-1,y-1)]) else ([]))
        )
        else (
            (if ((locationExistsInList (x,y-1) (myPieces++oppPieces))==False) then ([(x,y-1)]) else ([])) ++
            (if (locationExistsInList (x+1,y-1) oppPieces) then ([(x+1,y-1)]) else ([])) ++
            (if (locationExistsInList (x-1,y-1) oppPieces) then ([(x-1,y-1)]) else ([]))
        )
    ))

validDest :: Player -> Piece -> (Int,Int) -> [Piece] -> [Piece] -> Bool
validDest player piece loc myPieces oppPieces = loc `elem` (validDestinations player piece myPieces oppPieces)
