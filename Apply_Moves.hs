{-

Erick Yip
30028293
449 Asg1 - Part 2

-}

module Apply_Moves where

import GameLogic
import Moves

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

midPoint :: Coord -> Coord -> Coord            
midPoint x y = (((fst x + fst y)`div`2),((snd x + snd y)`div`2))

remove :: Coord -> [Coord] -> [Coord]
remove x y = filter (/=x) y 

restartGame :: GameState -> GameState
restartGame st
        | _status st == Red = st{_message = "Black Wins!", _status = GameOver}
        | otherwise = st{_message = "Red Wins!", _status = GameOver}