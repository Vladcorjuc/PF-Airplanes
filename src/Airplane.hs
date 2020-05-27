{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Airplane where

type Field = [[Bool]]
type Coordinate = (Int, Int)
type Plane = [Coordinate]
data Direction=N|S|E|W  deriving (Eq,Show)


fieldSize = 10
numberOfPlanes=3



select :: Int -> [a] -> a
select n xs = head (drop (n-1) (take n xs))

replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n-1) xs ++ [x] ++ drop n xs

initField :: Field
initField = take fieldSize (repeat (take fieldSize (repeat False)))

showRow::[Bool]->Int->String
showRow [] _= "\n"
showRow (x:xs) 1= if x == True then ("| ✘ |"++(showRow (xs) 2 ))
	else ("|   |"++(showRow xs 2))
showRow (x:xs) n= if x == True then (" ✘ |"++(showRow (xs) (n+1)))
	else ("   |"++(showRow xs (n+1)))

showField::Field->Int->String
showField [] _= "\n"
showField (x:xs) 1= "\nThis is the enemy field:\n+---+---+---+---+---+---+---+---+---+---+\n"++(showRow x 1)++"+---+---+---+---+---+---+---+---+---+---+\n|"++(showField xs (1+1))
showField (x:xs) 10= (showRow x 10)++"+---+---+---+---+---+---+---+---+---+---+\n"++(showField xs (10+1))
showField (x:xs) n= (showRow x n)++"+---+---+---+---+---+---+---+---+---+---+\n|"++(showField xs (n+1))


showOwnField::[Plane]->Int->Int->String
showOwnField planes 1 1= if (occupiedCoord planes 1 1) ==True then "Your field is:\n+---+---+---+---+---+---+---+---+---+---+\n| ✈ |"++(showOwnField planes (1) (1+1) )
	else "+---+---+---+---+---+---+---+---+---+---+\n|   |"++(showOwnField planes (1) (1+1) )
showOwnField planes 10 10 = if (occupiedCoord planes 10 10) ==True then " ✈ |"++"\n+---+---+---+---+---+---+---+---+---+---+\n"
	else "   |"++"\n"++"+---+---+---+---+---+---+---+---+---+---+\n"
showOwnField planes x 10 =  if (occupiedCoord planes x 10) ==True then " ✈ |"++"\n+---+---+---+---+---+---+---+---+---+---+\n"++(showOwnField planes (x+1) 1 )
	else "   |"++"\n"++"+---+---+---+---+---+---+---+---+---+---+\n"++(showOwnField planes (x+1) 1 )
showOwnField planes x y| y==1  =  if (occupiedCoord planes x y) ==True then "| ✈ |"++(showOwnField planes (x) (y+1) )
	else "|   |"++(showOwnField planes (x) (y+1) )
	|otherwise = if (occupiedCoord planes x y) ==True then " ✈ |"++(showOwnField planes (x) (y+1) )
	else "   |"++(showOwnField planes (x) (y+1) )


occupiedCoord::[Plane]->Int->Int->Bool
occupiedCoord (p:xs) x y = if ((x,y) `elem` p) then True else (occupiedCoord xs x y)
occupiedCoord [] x y = False

markShot :: Field -> Int -> Int -> Field
markShot field x y = replace x field (replace y (select x field) True)


markShotAll::Plane->Field->Field
markShotAll ((p1,p2):planes) field = markShotAll planes (markShot field p1 p2) 
markShotAll [] field = field


removePlane::Plane->[Plane]->[Plane]
removePlane p [] = []
removePlane p (x:xs)| x == p = xs
					| otherwise = [x] ++ removePlane p xs

shot::Field->[Plane]->Int->Int->[Plane]->IO (Field,[Plane])
shot f [] x y initPlanes = return (markShot f x y ,[])
shot f (p:plane) x y initPlanes = do
	if ( (x,y) `elem` p) then
		if ( (x,y) == (p!!0)) then
			do
				let field=markShotAll p f
				let planes= removePlane p initPlanes
				if planes == [] then
					return ([],[])
				else
					return(field,planes)
		else
			do
				let field=markShot f x y
				return (field,initPlanes)
	else
		shot f plane x y initPlanes


alreadyHit::Coordinate->Field->Bool
alreadyHit (x,y) field= select y (select x (field))



removeDestroyedPlane:: [Plane] -> [Plane]
removeDestroyedPlane [] = []
removeDestroyedPlane (x:xs) | null x    = removeDestroyedPlane xs
                            | otherwise = x : removeDestroyedPlane xs


checkCoords :: Coordinate -> Bool
checkCoords coord = and [ fst coord >= 1,
                                 snd coord >= 1,
                                 fst coord <= fieldSize,
                                 snd coord <= fieldSize
                        ]


checkPlainContains::Plane->Coordinate->Coordinate->Coordinate->Coordinate->Coordinate->Coordinate->Coordinate->Coordinate->Coordinate->Coordinate->Bool
checkPlainContains plane a1 b1 b2 b3 b4 b5 c1 d1 e1 e2  = a1 `elem` plane || b1 `elem` plane || b2 `elem` plane || b3 `elem` plane || b4 `elem` plane || b5 `elem` plane || c1 `elem` plane || d1 `elem` plane || e1 `elem` plane || e2 `elem` plane 

checkColission::Plane->Coordinate->Direction->Bool
checkColission plane headCoord S = if( checkCoords (fst headCoord+1,snd headCoord-2) == False) then True 
	else
		if( checkPlainContains plane headCoord (fst headCoord+1,snd headCoord-2) (fst headCoord+1,snd headCoord-1) (fst headCoord+1,snd headCoord-0) 
		(fst headCoord+1,snd headCoord+1) (fst headCoord+1,snd headCoord+2) (fst headCoord+2,snd headCoord-0)
		(fst headCoord+3,snd headCoord-0) (fst headCoord+3,snd headCoord-1) (fst headCoord+3,snd headCoord+1) ==True) then True
		else False 

checkColission plane headCoord N = if( checkCoords (fst headCoord-1,snd headCoord-2) == False) then True 
	else
		if( checkPlainContains plane headCoord (fst headCoord-1,snd headCoord-2) (fst headCoord-1,snd headCoord-1) (fst headCoord-1,snd headCoord-0) 
		(fst headCoord-1,snd headCoord+1) (fst headCoord-1,snd headCoord+2) (fst headCoord-2,snd headCoord-0) 
		(fst headCoord-3,snd headCoord-0) (fst headCoord-3,snd headCoord-1) (fst headCoord-3,snd headCoord+1) ==True) then True
		else False 

checkColission plane headCoord E = if( checkCoords (fst headCoord+2,snd headCoord+1) == False) then True 
	else
		if( checkPlainContains plane headCoord (fst headCoord+2,snd headCoord+1) (fst headCoord+1,snd headCoord+1) (fst headCoord,snd headCoord+1)
		(fst headCoord-1,snd headCoord+1) (fst headCoord-2,snd headCoord+1) (fst headCoord,snd headCoord+2)
		(fst headCoord-1,snd headCoord+3) (fst headCoord,snd headCoord+3) (fst headCoord+1,snd headCoord+3) ==True) then True
		else False

checkColission plane headCoord W = if( checkCoords (fst headCoord+2,snd headCoord-1) == False) then True 
	else
		if( checkPlainContains plane headCoord (fst headCoord+2,snd headCoord-1) (fst headCoord+1,snd headCoord-1) (fst headCoord,snd headCoord-1)
		(fst headCoord-1,snd headCoord-1) (fst headCoord-2,snd headCoord-1) (fst headCoord,snd headCoord-2)
		(fst headCoord+1,snd headCoord-3) (fst headCoord,snd headCoord-3) (fst headCoord-1,snd headCoord-3) ==True) then True
		else False 




checkIfNotOccupied::[Plane]->Coordinate->Direction->Bool
checkIfNotOccupied [] headCoord direction = True
checkIfNotOccupied (plane:rest) headCoord direction= if (checkColission plane headCoord direction == True) then False
	else checkIfNotOccupied rest headCoord direction


checkIfCanPutPlane::[Plane]->Coordinate->Direction->Bool
checkIfCanPutPlane planes headCoord direction = if( (not (False `elem` ( checkCoordsPlane headCoord direction )) ) && (checkIfNotOccupied planes headCoord direction ) ==True ) then True 
	else False


checkCoordsPlane::Coordinate->Direction->[Bool]
checkCoordsPlane coord dir = map checkCoords (generatePlane coord dir)


generatePlane::Coordinate->Direction->Plane
generatePlane headCoord S = [ headCoord,(fst headCoord+1,snd headCoord-2),(fst headCoord+1,snd headCoord-1),(fst headCoord+1,snd headCoord-0), 
		(fst headCoord+1,snd headCoord+1),(fst headCoord+1,snd headCoord+2),(fst headCoord+2,snd headCoord-0), 
		(fst headCoord+3,snd headCoord-0),(fst headCoord+3,snd headCoord-1),(fst headCoord+3,snd headCoord+1) ]

generatePlane headCoord N = [ headCoord,(fst headCoord-1,snd headCoord-2),(fst headCoord-1,snd headCoord-1),(fst headCoord-1,snd headCoord-0), 
		(fst headCoord-1,snd headCoord+1),(fst headCoord-1,snd headCoord+2),(fst headCoord-2,snd headCoord-0), 
		(fst headCoord-3,snd headCoord-0),(fst headCoord-3,snd headCoord-1),(fst headCoord-3,snd headCoord+1) ]

generatePlane headCoord E =[ headCoord,(fst headCoord+2,snd headCoord+1),(fst headCoord+1,snd headCoord+1),(fst headCoord,snd headCoord+1), 
		(fst headCoord-1,snd headCoord+1),(fst headCoord-2,snd headCoord+1),(fst headCoord,snd headCoord+2), 
		(fst headCoord-1,snd headCoord+3),(fst headCoord,snd headCoord+3),(fst headCoord+1,snd headCoord+3) ]

generatePlane headCoord W =[ headCoord,(fst headCoord+2,snd headCoord-1),(fst headCoord+1,snd headCoord-1),(fst headCoord,snd headCoord-1), 
		(fst headCoord-1,snd headCoord-1),(fst headCoord-2,snd headCoord-1),(fst headCoord,snd headCoord-2), 
		(fst headCoord+1,snd headCoord-3),(fst headCoord,snd headCoord-3),(fst headCoord-1,snd headCoord-3) ]


putPlane::[Plane]->Coordinate->Direction->([Plane],Bool)
putPlane planes headCoord direction= if ( checkIfCanPutPlane planes headCoord direction ==True ) then (newPlanes,True) else (planes,False) where newPlanes= planes++[(generatePlane headCoord direction)] 

hasPlacedAllPlanes::[Plane]->Bool
hasPlacedAllPlanes planes = length planes == numberOfPlanes