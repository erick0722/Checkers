{-

Erick Yip
30028293
449 Asg1 - Part 2

-}

module Moves where

import GameLogic


moves :: GameState -> [Move]
moves st
        | jumpmoves /= [] = jumpmoves
        | otherwise = simplemoves
        where
            simplemoves = simple_moves st
            jumpmoves = jump_moves st
            
simple_moves:: GameState -> [Move]
simple_moves st
        | _status st == Red = (simpleKing (_redKings st))++(simplePiece (_redPieces st))
        | _status st == Black = (simpleKing (_blackKings st))++ (simplePiece (_blackPieces st))
        | otherwise = [] 
        where
            simplePiece [] = []
            simplePiece xs = [[(x,y),(x',y')]
                | (x,y) <- xs, (x',y') <- let y'=y+dir in [(x+1,y'),(x-1,y')], notoccupied (x',y') st && onboard (x',y')]

            simpleKing [] = []
            simpleKing xs = [[(x,y),(x',y')]
                | (x,y) <- xs, (x',y') <- [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)], notoccupied (x',y') st && onboard (x',y')]
            
            dir = case (_status st) of
                {Red -> -1 ; Black -> 1} 
    
jump_moves :: GameState -> [Move]
jump_moves st
        | _status st == Red = (jumpKing (_redKings st) st)++(jumpPiece (_redPieces st) st)
        | _status st == Black = (jumpKing (_blackKings st) st)++ (jumpPiece (_blackPieces st) st)
        | otherwise = []
        where
            jumpPiece [] st = []
            jumpPiece xs st = [(x,y):ys | (x,y) <- xs, ys <- jumpPiece' (x,y) [] (x,y)]
                
            jumpPiece' start rem (x,y)  = [(x'', y''):ys 
                    | ((x',y'),(x'',y'')) <- [((x+dir,y+dir),(x+2*dir,y+2*dir))
                    ,((x-dir,y+dir),(x-2*dir,y+2*dir))], not ((x',y') `elem` rem) 
                    && opponent_occupied (x',y') st && (start == (x'',y'') || notoccupied (x'',y'') st) 
                    && onboard (x'',y''), ys <- jump_over (if (isRedKing (x'',y'') || isBlackKing (x'',y''))  
                        then (jumpKing' start ((x',y'):rem) (x'',y'')) 
                        else (jumpPiece' start ((x',y'):rem) (x'',y'')))]

            jumpKing [] st = []
            jumpKing xs st = [(x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y)]
            
            jumpKing' start rem (x,y) =
                [ (x'',y''):ys
                    | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                    , not ((x',y') `elem` rem) && opponent_occupied (x',y') st && (start==(x'',y'') || notoccupied (x'',y'') st) && onboard (x'',y'')
                    , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'')) ]

            jump_over [] = [[]]
            jump_over z = z
            
            dir = case (_status st) of
                {Red -> -1 ; Black -> 1} 

notoccupied :: Coord -> GameState -> Bool
notoccupied c st
        | c `elem` _redPieces st = False
        | c `elem` _redKings st = False
        | c `elem` _blackPieces st = False
        | c `elem` _blackKings st = False
        | otherwise = True

opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied c st 
        | _status st == Black = c `elem` (_redPieces st) || c `elem` (_redKings st)
        | _status st == Red = c `elem` (_blackPieces st) || c `elem` (_blackKings st)   
        | otherwise = False

onboard :: Coord -> Bool
onboard c
        | (fst c >= 0 && fst c <= 7 && snd c >= 0 && snd c <= 7) = True
        | otherwise = False

isRedKing :: Coord -> Bool
isRedKing x
        | snd x == 0 = True
        | otherwise = False

isBlackKing :: Coord -> Bool
isBlackKing x
        | snd x == 7 = True
        | otherwise = False
            