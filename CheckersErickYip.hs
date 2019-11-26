{-

Erick Yip
30028293
449 Asg1 - Part 2

-}

module CheckersErickYip where

import GameLogic
import Checkers 

redAI :: GameState -> Move
redAI st = fst (last (sort_heuristic (getMoves st Red (moves st))))

blackAI :: GameState -> Move
blackAI st = fst (last (sort_heuristic (getMoves st Black (moves st))))

getMoves :: GameState -> Status -> [Move] -> [(Move,Int)]
getMoves st stat [] = []
getMoves st stat (x:xs) = (x, (abMinPrune (apply_move x st) stat (-1000) (1000) 8)) : (getMoves st stat xs) 

get_heuristic :: GameState -> Status -> Int
get_heuristic st stat
    | stat == Red = length (_redPieces st) - length (_blackPieces st) + 2 * (length (_redKings st)  - length (_blackKings st))
    | otherwise = length (_blackPieces st) - length (_redPieces st) + 2 * (length (_blackKings st)  - length (_redKings st))

sort_heuristic :: [(Move,Int)] -> [(Move,Int)]
sort_heuristic [] = []
sort_heuristic (x:xs)
    | xs == [] = [x]
    | snd x > snd (head xs) = head xs : sort_heuristic (x : tail xs)
    | otherwise = x : sort_heuristic xs

abMaxPrune :: GameState -> Status -> Int -> Int -> Int -> Int
abMaxPrune st stat alpha beta depth
    | alpha == beta = alpha
    | depth == 0 = min (max (get_heuristic st stat) alpha) beta
    | otherwise = abMaxPrune' st stat (moves st) alpha beta depth

abMaxPrune' :: GameState -> Status -> [Move] -> Int -> Int -> Int -> Int
abMaxPrune' st stat [] alpha beta depth = alpha
abMaxPrune' st stat (x:xs) alpha beta depth = abMaxPrune' st stat xs newAlpha beta depth
    where
        newAlpha = abMinPrune (apply_move x st) stat alpha beta (depth - 1)

abMinPrune :: GameState -> Status -> Int -> Int -> Int -> Int
abMinPrune st stat alpha beta depth
    | alpha == beta = beta
    | depth == 0 = max (min (get_heuristic st stat) beta) alpha
    | otherwise = abMinPrune' st stat (moves st) alpha beta depth

abMinPrune' :: GameState -> Status -> [Move] -> Int -> Int -> Int -> Int
abMinPrune' st stat [] alpha beta depth = beta
abMinPrune' st stat (x:xs) alpha beta depth = abMinPrune' st stat xs alpha newBeta depth
    where
        newBeta = abMaxPrune (apply_move x st) stat alpha beta (depth - 1)

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

apply_move :: Move -> GameState -> GameState
apply_move mv st
        | moves st == [] = restartGame st
        | mv `elem` jump_moves st = make_jump_move mv st
        | mv `elem` simple_moves st && (jump_moves st == [])= make_simple_move mv st
        | otherwise = st {_message = show (moves st)}
        where
            make_simple_move [start,end] st
                | _status st == Red && start `elem` (_redKings st)
                    = st{_redKings = remove start (_redKings st) ++ [end]
                        ,_status = Black
                        ,_message = "Moved"}
                | _status st == Red && start `elem` (_redPieces st) && (isRedKing end == False)
                    = st{_redPieces = remove start (_redPieces st) ++ [end]
                        ,_status = Black 
                        ,_message = "Moved"}         
                | _status st == Black && start `elem` (_blackKings st)
                    = st{_blackKings = remove start (_blackKings st) ++ [end]
                        ,_status = Red
                        ,_message = "Moved"}
                | _status st == Black && start `elem` (_blackPieces st) && (isBlackKing end == False)
                    = st{_blackPieces = remove start (_blackPieces st) ++ [end]
                        ,_status = Red
                        ,_message = "Moved"}
                | _status st == Red && start `elem` (_redPieces st) && (isRedKing end)
                    = st{_redPieces = remove start (_redPieces st)
                        ,_redKings = _redKings st ++ [end] 
                        ,_status = Black 
                        ,_message = "Moved"}  
                | _status st == Black && start `elem` (_blackPieces st) && (isBlackKing end)
                    = st{_blackPieces = remove start (_blackPieces st)
                        ,_blackKings = _blackKings st ++ [end]
                        ,_status = Red
                        ,_message = "Moved"}

            make_jump_move (start:[]) st
                | _status st == Red = st{_status = Black}
                | otherwise = st{_status = Red}
            make_jump_move (start:(next:rest)) st
                | (_status st == Black && start `elem` (_blackPieces st)) && (isBlackKing next)
                       = make_jump_move (next:rest) 
                                        (st{_redKings = remove (midPoint start next) (_redKings st)
                                        ,_redPieces = remove (midPoint start next) (_redPieces st)
                                        ,_blackPieces = (remove start (_blackPieces st))
                                        ,_blackKings = (_blackKings st) ++ [next]
                                        ,_message = "Jumped"})
                | (_status st == Red && start `elem` (_redPieces st)) && (isRedKing next)
                         = make_jump_move (next:rest) 
                                        (st{_blackKings = remove (midPoint start next) (_blackKings st)
                                        ,_blackPieces = remove (midPoint start next) (_blackPieces st)
                                        ,_redPieces = (remove start (_redPieces st))
                                        ,_redKings = (_redKings st) ++ [next]
                                        ,_message = "Jumped"})
                | _status st == Red && start `elem` (_redKings st)
                            = make_jump_move (next:rest)
                                        (st{_blackKings = remove (midPoint start next) (_blackKings st)
                                        ,_blackPieces = remove (midPoint start next) (_blackPieces st)
                                        ,_redKings = (remove start (_redKings st)) ++ [next]
                                        ,_message = "Jumped"})
                | _status st == Red && start `elem` (_redPieces st)
                            = make_jump_move (next:rest)
                                        (st{_blackKings = remove (midPoint start next) (_blackKings st)
                                        ,_blackPieces = remove (midPoint start next) (_blackPieces st)
                                        ,_redPieces = (remove start (_redPieces st)) ++ [next]
                                        ,_message = "Jumped"})
                | _status st == Black && start `elem` (_blackKings st)
                              = make_jump_move (next:rest)
                                        (st{_redKings = remove (midPoint start next) (_redKings st)
                                        ,_redPieces = remove (midPoint start next) (_redPieces st)
                                        ,_blackKings = (remove start (_blackKings st)) ++ [next]
                                        ,_message = "Jumped"})
                | _status st == Black && start `elem` (_blackPieces st)
                             = make_jump_move (next:rest)
                                       (st{_redKings = remove (midPoint start next) (_redKings st)
                                        ,_redPieces = remove (midPoint start next) (_redPieces st)
                                        ,_blackPieces = (remove start (_blackPieces st)) ++ [next]
                                        ,_message = " Jumped"})
                
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

midPoint :: Coord -> Coord -> Coord            
midPoint x y = (((fst x + fst y)`div`2),((snd x + snd y)`div`2))

remove :: Coord -> [Coord] -> [Coord]
remove x y = filter (/=x) y 

restartGame :: GameState -> GameState
restartGame st
        | _status st == Red = st{_message = "Black Wins!", _status = GameOver}
        | otherwise = st{_message = "Red Wins!", _status = GameOver}