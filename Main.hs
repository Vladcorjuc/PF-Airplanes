{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import GHC.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Network.Socket
import System.IO
import Data.List.Split
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Airplane



main :: IO ()
main =  do
	sock <- socket AF_INET Stream 0  
	setSocketOption sock ReuseAddr 1   
	bind sock (SockAddrInet 4242  0x0100007f)
	putStrLn "Listen on 127.0.0.1:4242"   
	listen sock 2
	mainLoop sock    


mainLoop :: Socket -> IO ()
mainLoop sock = do
	conn <- accept sock   
	runConn conn


parseHitInput::[String]->(String,Int,Int)
parseHitInput (w:rest) = if(length (rest) >1) then (w,parseIntArgs (rest !! 0),parseIntArgs (rest !! 1)) else ("error",0,0)

parseInput::[String]->(String,Int,Int,Direction)
parseInput (w:rest)= if(length (rest) >2) then (w,parseIntArgs (rest !! 0),parseIntArgs (rest !! 1) ,parseDirArgs (rest !! 2)) else ("error",0,0,N)

parseIntArgs::String->Int
parseIntArgs w = read w::Int

parseDirArgs::String->Direction
parseDirArgs w = if w=="N" then N 
	else
		if w=="S" then S 
			else
				if w=="E" then E
					else
						W



vPutPlane::String->[Plane]->[Plane]
vPutPlane line planes= do
		let (command,num1,num2,pos) = parseInput (words line)
		if command == "plane" then do
			let (newPlanes,succes) = putPlane planes ((num1,num2)) pos
			if succes==True then
				newPlanes
			else
				[]
		else
			[]

vHitField::String->IO (Int,Int)
vHitField line= do
	let (command,num1,num2) = parseHitInput (words line)
	if command == "fire" then
		return (num1,num2)
	else
		return (0,0)
	

hputPlanesLoop::Handle ->Int-> [Plane] -> IO [Plane]
hputPlanesLoop _ 3 planes = return planes
hputPlanesLoop hdl n planes = do
	line <-hGetLine hdl
	let newPlanes = vPutPlane line planes
	if(newPlanes==[]) then
		do
			hPutStrLn hdl "Incorrect position, select another"
			hputPlanesLoop hdl n planes

	else
		do
			hPutStrLn hdl "Plane placed succesfully!"
			hputPlanesLoop hdl (n+1) newPlanes


putPlanesLoop::Int-> [Plane] -> IO [Plane]
putPlanesLoop 3 planes = return planes
putPlanesLoop n planes = do
	line <-getLine
	let newPlanes = vPutPlane line planes
	if(newPlanes==[]) then
		do
			putStrLn "Incorrect position, select another"
			putPlanesLoop  n planes

	else
		do
			putStrLn  "Plane placed succesfully!"
			putPlanesLoop (n+1) newPlanes


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hSetEncoding hdl utf8
    let field1=initField
    let field2=initField



    putStrLn ("\n\n░█████╗░██╗██████╗░██████╗░██╗░░░░░░█████╗░███╗░░██╗███████╗░██████╗\n██╔══██╗██║██╔══██╗██╔══██╗██║░░░░░██╔══██╗████╗░██║██╔════╝██╔════╝\n███████║██║██████╔╝██████╔╝██║░░░░░███████║██╔██╗██║█████╗░░╚█████╗░\n██╔══██║██║██╔══██╗██╔═══╝░██║░░░░░██╔══██║██║╚████║██╔══╝░░░╚═══██╗\n██║░░██║██║██║░░██║██║░░░░░███████╗██║░░██║██║░╚███║███████╗██████╔╝\n╚═╝░░╚═╝╚═╝╚═╝░░╚═╝╚═╝░░░░░╚══════╝╚═╝░░╚═╝╚═╝░░╚══╝╚══════╝╚═════╝░\n\n\n")
    hPutStrLn hdl ("\n\n░█████╗░██╗██████╗░██████╗░██╗░░░░░░█████╗░███╗░░██╗███████╗░██████╗\n██╔══██╗██║██╔══██╗██╔══██╗██║░░░░░██╔══██╗████╗░██║██╔════╝██╔════╝\n███████║██║██████╔╝██████╔╝██║░░░░░███████║██╔██╗██║█████╗░░╚█████╗░\n██╔══██║██║██╔══██╗██╔═══╝░██║░░░░░██╔══██║██║╚████║██╔══╝░░░╚═══██╗\n██║░░██║██║██║░░██║██║░░░░░███████╗██║░░██║██║░╚███║███████╗██████╔╝\n╚═╝░░╚═╝╚═╝╚═╝░░╚═╝╚═╝░░░░░╚══════╝╚═╝░░╚═╝╚═╝░░╚══╝╚══════╝╚═════╝░\n\n\n")


    hPutStrLn hdl ("The enemy is putting his planes,please wait\n")
    myThread<-forkIO (hnotTurnLoop hdl)


    putStrLn ("Put 3 planes like : plane 1 2 S")
    planes1 <- putPlanesLoop 0 []
    putStrLn (showField (field1) 1)
    putStrLn (showOwnField (planes1) 1 1)

    killThread myThread


    hPutStrLn hdl ("It is your turn now!")
    putStrLn ("The enemy is putting his planes,please wait\n")
    mySecondThread <- forkIO (notTurnLoop)

    hPutStrLn hdl ("Put 3 planes like : plane 1 2 S")
    planes2 <- hputPlanesLoop hdl 0 []

    hPutStrLn hdl (showField (field2) 1)
    hPutStrLn hdl (showOwnField planes2 1 1)

    killThread mySecondThread

    newId1<- forkIO notTurnLoop
    newId2<-forkIO (hnotTurnLoop hdl)

    initGame hdl field1 field2 planes1 planes2 1 newId1 newId2
    hClose hdl



initGame::Handle->Field->Field->[Plane]->[Plane]->Int->ThreadId->ThreadId->IO ()
initGame hdl field1 field2 planes1 planes2 1 id1 id2= do
	if planes1 == [] then
		initGame hdl field1 field2 planes1 planes2 4 id1 id2
	else
		do
			killThread id1
			putStrLn ("It is your turn now!")
			hPutStrLn hdl ("Enemy turn now!")
			(newField1,response,newPlanes2)<-playFirstTurn field1 planes2
			if newField1 == [] then
				do
					newId1<- forkIO notTurnLoop
					putStrLn "Invalid move"
					initGame hdl field1 field2 planes1 planes2 1 newId1 id2
			else
				do
					putStrLn "Fire!!"
					putStrLn response
					putStrLn "this is the result!"
					putStrLn (showField newField1 1)
					newId1<- forkIO notTurnLoop
					initGame hdl newField1 field2 planes1 newPlanes2 2 newId1 id2

initGame hdl field1 field2 planes1 planes2 2 id1 id2= do
	if planes2 == [] then
		initGame hdl field1 field2 planes1 planes2 3 id1 id2
	else
		do
			killThread id2
			putStrLn ("It is the Enemy turn now!")
			hPutStrLn hdl ("It is your turn now!")
			(newField2,response,newPlanes1)<-playSecondTurn hdl field2 planes1
			if newField2 == [] then
				do
					newId2<-forkIO (hnotTurnLoop hdl)
					hPutStrLn hdl ("Invalid move")
					initGame hdl field1 field2 planes1 planes2 2 id1 newId2
			else
				do
					newId2<-forkIO (hnotTurnLoop hdl)
					hPutStrLn hdl ("Fire!!")
					hPutStrLn hdl response
					hPutStrLn hdl ("this is the result!")
					hPutStrLn hdl (showField newField2 1)
					initGame hdl field1 newField2 newPlanes1 planes2 1 id1 newId2

initGame hdl field1 field2 planes1 planes2 3 id1 id2= do
	killThread id1
	killThread id2
	putStrLn "Game Ended the Winner is player number 1"
	hPutStrLn hdl "Game Ended the Winner is player number 1"

initGame hdl field1 field2 planes1 planes2 4 id1 id2= do
	killThread id1
	killThread id2
	putStrLn "Game Ended the Winner is player number 2"
	hPutStrLn hdl "Game Ended the Winner is player number 2"



playFirstTurn::Field->[Plane]->IO (Field,String,[Plane])
playFirstTurn field plane= do
	line<-getLine
	(num1,num2)<-vHitField (line)
	if((checkCoords (num1,num2))==False) then
		do
			return ([],"invalid coords",[])
	else
		do
			if alreadyHit (num1,num2) field == True then
				return ([],"Already shotted here",[])
			else
				do
					(newField,newPlanes)<-shot field plane num1 num2 plane
					if newField==[] then do
						return(markShotAll (plane!!0) field,"shotted all planes",[])
					else
						do
							if newPlanes==[] then
								return (newField,"Shot in the water",plane)
							else
								return (newField,"Shotted a plane",newPlanes)



playSecondTurn::Handle->Field->[Plane]->IO (Field,String,[Plane])
playSecondTurn hdl field plane= do
	line<-hGetLine hdl
	(num1,num2)<-vHitField (line)
	if((checkCoords (num1,num2))==False) then
		do
			return ([],"invalid coords",[])
	else
		do
			if alreadyHit (num1,num2) field == True then
				return ([],"Already shotted here",[])
			else
				do
					(newField,newPlanes)<-shot field plane num1 num2 plane
					if newField==[] then
						return(markShotAll (plane!!0) field,"shotted all planes",[])
					else
						do
							if newPlanes==[] then
								return (newField,"Shot in the water",plane)
							else
								return (newField,"Shotted a plane",newPlanes)



hnotTurnLoop::Handle->IO ()
hnotTurnLoop hdl= do
	line<-hGetLine hdl
	if ((words line ) !! 0 )=="help" then
		do
			hPutStrLn hdl "HELP:\n~~plane 10 8 E (place plane in position 10 8 in E direction (N,E,S,W) use when it is your turn and initial turn)\n~~fire 1 1 (fire at position 1 1 use when is attacking turn)\n~~help(show this help messages)"
			hnotTurnLoop hdl
	else
		do
			hPutStrLn hdl "Not your turn but you can use 'help'"
			hnotTurnLoop hdl

notTurnLoop::IO ()
notTurnLoop= do
	line<-getLine
	if ((words line ) !! 0 )=="help" then
		do
			putStrLn "HELP:\n~~plane 10 8 E (place plane in position 10 8 in E direction (N,E,S,W) use when it is your turn and initial turn)\n~~fire 1 1 (fire at position 1 1 use when is attacking turn)\n~~(help show this help messages)"
			notTurnLoop
	else
		do
			putStrLn "Not your turn but you can use 'help'"
			notTurnLoop