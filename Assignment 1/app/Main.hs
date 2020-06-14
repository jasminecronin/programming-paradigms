module Main where

--import Tui
import Checkers
import GameLogic
 
main :: IO ()
--main = tui

--main :: ApplyMove -> GameState -> IO()
--main = human apply_move initialGameState
main = blackAi blackAI apply_move initialGameState
--main = redAi redAI apply_move initialGameState

apply_move :: ApplyMove
apply_move mv st | mv `elem` (move st) = apply_move' mv st {_message = ""}
                 | otherwise = st {_message = "Illegal move! Try again."}

--apply_move' :: [Move] -> GameState -> GameState
apply_move' ((x1,y1):((x2,y2):[])) st 
	| ((x1-x2) == 1) || ((x2-x1) == 1)
		= make_simple_move [(x1,y1),(x2,y2)] st
	| otherwise = make_jump_move [(x1,y1),(x2,y2)] st
apply_move' mvs st = make_jump_move mvs st

make_simple_move :: ApplyMove 
make_simple_move [start, end] st | start `elem` (_redKings st)
				= st { _redKings = replace start end (_redKings st)
				     , _status = changePlayer (_status st)
				     , _message = ""}
			    | start `elem` (_blackKings st)
				= st { _blackKings = replace start end (_blackKings st)
				     , _status = changePlayer (_status st)
				     , _message = ""}
			    | start `elem` (_redPieces st)
				= st { _redPieces = kingMeCheckRed start end st
				     , _redKings = kingMeRed end st
				     , _status = changePlayer (_status st)
				     , _message = ""}
			    | start `elem` (_blackPieces st)
				= st { _blackPieces = kingMeCheckBlack start end st
				     , _blackKings = kingMeBlack end st
				     , _status = changePlayer (_status st)
				     , _message = ""}
make_simple_move _ st = st {_message = "Cannot apply move!"}

make_jump_move :: [Coord] -> GameState -> GameState
make_jump_move (start:[]) st = st { _status = changePlayer (_status st)
				, _message = ""} 
make_jump_move (start:(next:rest)) st
	| _status st == Red && (start `elem` (_redKings st))
		= make_jump_move (next:rest)
			(st { _blackKings = remove (jumped start next) (_blackKings st)
                            , _blackPieces = remove (jumped start next) (_blackPieces st)
                            , _redKings = next:(remove start (_redKings st))
                            , _message = ""})
	| _status st == Red && (start `elem` (_redPieces st))
		= make_jump_move (next:rest)
			(st { _blackKings = remove (jumped start next) (_blackKings st)
			    , _blackPieces = remove (jumped start next) (_blackPieces st)
                            , _redPieces = kingMeCheckRed start next st--next:(remove start (_redPieces st))
			    , _redKings = kingMeRed next st
                            , _message = ""})
	| _status st == Black && (start `elem` (_blackKings st))
		= make_jump_move (next:rest)
			(st { _redKings = remove (jumped start next) (_redKings st)
			    , _redPieces = remove (jumped start next) (_redPieces st)
			    , _blackKings = next:(remove start (_blackKings st))
			    , _message = ""})
	| _status st == Black && (start `elem` (_blackPieces st))
		= make_jump_move (next:rest)
			(st { _redKings = remove (jumped start next) (_redKings st)
			    , _redPieces = remove (jumped start next) (_redPieces st)
			    , _blackPieces = kingMeCheckBlack start next st--next:(remove start (_blackPieces st))
			    , _blackKings = kingMeBlack next st
			    , _message = ""})
make_jump_move _ st = st{_message = "Cannot apply jump move!"}

jumped :: Coord -> Coord -> Coord
jumped (x1,y1) (x2,y2) = ((x1+x2) `div` 2, (y1+y2) `div` 2)

-- check if a piece became a king
-- if not, do the normal replace
-- if so, return the piece list with the piece removed
kingMeCheckRed :: Coord -> Coord -> GameState -> [Coord]
kingMeCheckRed start end@(x,y) st 
	| y /= 0 = replace start end (_redPieces st)
	| otherwise = remove start (_redPieces st)

kingMeCheckBlack :: Coord -> Coord -> GameState -> [Coord]
kingMeCheckBlack start end@(x,y) st 
	| y /= 7 = replace start end (_blackPieces st)
	| otherwise = remove start (_blackPieces st)

-- check if a piece became a king
-- if not, return the unaltered kings list
-- if so, return the kings list with piece added
kingMeRed :: Coord -> GameState -> [Coord]
kingMeRed end@(x,y) st
	| y /= 0 = (_redKings st)
	| otherwise = end:(_redKings st)

kingMeBlack :: Coord -> GameState -> [Coord]
kingMeBlack end@(x,y) st
	| y /= 7 = (_blackKings st)
	| otherwise = end:(_blackKings st)

move :: GameState -> [Move] -- returns the list of valid moves given a gamestate
move st | jumpmoves /= [] = jumpmoves
        | otherwise = (simple_moves st)
	where jumpmoves = (jump_moves st)

move_look :: GameState -> [(Move, GameState)] -- returns a list of valid moves and potential updated gamestates
move_look st = map (\m -> (m, (apply_move' m st))) (move st)

simple_moves :: GameState -> [Move]
simple_moves st | (_status st) == Red 
	= (simpleKing_moves (_redKings st) st) ++ (simplePiece_moves (_redPieces st) (_status st) st)
		| (_status st) == Black
	= (simpleKing_moves (_blackKings st) st) ++ (simplePiece_moves (_blackPieces st) (_status st) st)

simpleKing_moves :: [Coord] -> GameState -> [Move]
simpleKing_moves coords st = 
	[[(x,y), (x',y')] | (x,y) <- coords
			  , (x',y') <- [(x+1, y+1), (x-1, y+1), (x+1, y-1), (x-1, y-1)]
			  , emptyPos (x',y') st
			  , onBoard (x',y')]

simplePiece_moves :: [Coord] -> Status -> GameState -> [Move]
simplePiece_moves coords stat st = 
	[[(x,y), (x',y')] | (x,y) <- coords
			  , (x',y') <- [(x+1, y+dir), (x-1, y+dir)]
			  , emptyPos (x',y') st
			  , onBoard (x',y')]
	where dir = case stat of {Red -> -1; Black -> 1}

jump_moves :: GameState -> [Move]
jump_moves st | (_status st) == Red
	= (jumpKing_moves (_redKings st) st) ++ (jumpPiece_moves (_redPieces st) (_status st) st)
	      | (_status st) == Black
	= (jumpKing_moves (_blackKings st) st) ++ (jumpPiece_moves (_blackPieces st) (_status st) st)

jumpKing_moves :: [Coord] -> GameState -> [Move]
jumpKing_moves coords st = [(x,y):ys | (x,y) <- coords
	           		     , ys <- jumpKing_moves' (x,y) [] (x,y) st]

jumpKing_moves' :: Coord -> [Coord] -> Coord -> GameState -> [[Coord]]
jumpKing_moves' start removed (x,y) st = 
	[(x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1, y+1), (x+2, y+2))
                                               ,((x-1, y+1), (x-2, y+2))
                                               ,((x+1, y-1), (x+2, y-2))
                                               ,((x-1, y-1), (x-2, y-2))]
                      , not ((x',y') `elem` removed)
                      && opponentOccupied st (x',y')
                      && emptyPos (x'',y'') st
                      && onBoard (x'',y'')
                      , ys <- jump_over (jumpKing_moves' start ((x',y'):removed) (x'',y'') st)]

--jump_over :: [Coord] -> 
jump_over [] = [[]]
jump_over z = z

jumpPiece_moves :: [Coord] -> Status -> GameState -> [Move]
jumpPiece_moves coords stat st = [(x,y):ys | (x,y) <- coords
                                           , ys <- jumpPiece_moves' (x,y) [] (x,y) st]

-- will need to call jumpKingmoves if the piece becomes a king
jumpPiece_moves' :: Coord -> [Coord] -> Coord -> GameState -> [[Coord]]
jumpPiece_moves' start removed (x,y) st = 
	[(x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1, y+dir), (x+2, y+(2*dir)))
                                               ,((x-1, y+dir), (x-2, y+(2*dir)))]
                      , not ((x',y') `elem` removed)
                      && opponentOccupied st (x',y')
                      && emptyPos (x'',y'') st
                      && onBoard (x'',y'')
                      , ys <- jump_over (jumpPiece_moves' start ((x',y'):removed) (x'',y'') st)]
	where dir = case (_status st) of {Red -> -1; Black -> 1}


emptyPos :: Coord -> GameState -> Bool
emptyPos pos st = not (pos `elem` ((_blackKings st) ++ (_redKings st) ++ (_blackPieces st) ++ (_redPieces st)))

opponentOccupied :: GameState -> Coord -> Bool
opponentOccupied st pos | (_status st) == Red = ((pos `elem` (_blackKings st)) || (pos `elem` (_blackPieces st)))
                        | otherwise = ((pos `elem` (_redKings st)) || (pos `elem` (_redPieces st)))

onBoard :: Coord -> Bool
onBoard (x,y) | (x>=0) && (x<=7) && (y>=0) && (y<=7) = True
	      | otherwise = False

changePlayer :: Status -> Status
changePlayer Red = Black
changePlayer Black = Red

replace :: Eq a => a -> a -> [a] -> [a]
replace start end [] = []
replace start end (a:as) | a == start = end:as
			 | otherwise = a:(replace start end as)

remove :: Coord -> [Coord] -> [Coord]
remove _ [] = []
remove x (y:ys) | x == y = remove x ys
		| otherwise = y:(remove x ys)

ply = 8
top = 30000
bot = (-30000)

red_heuristic :: GameState -> Int
red_heuristic st = (length (_redPieces st)) - (length (_blackPieces st)) + (2 * (length (_redKings st)) - (length (_blackKings st)))

black_heuristic :: GameState -> Int
black_heuristic st = (length (_blackPieces st)) - (length (_redPieces st)) + (2 * (length (_blackKings st)) - (length (_redKings st)))

getHeuristic :: GameState -> Int
getHeuristic st = case (_status st) of
	Red -> black_heuristic st
	Black -> red_heuristic st

abmaxprune :: GameState -> Int -> Int -> Int -> Int
abmaxprune st ply bot top | bot == top = top
		          | ply == 0 = min (max score bot) top
		          | otherwise = abmaxprunes ts ply bot top
			where ts = map snd (move_look st)
			      score = getHeuristic st

abmaxprunes :: [GameState] -> Int -> Int -> Int -> Int
abmaxprunes [] ply bot top = bot
abmaxprunes (t:ts) ply bot top = abmaxprunes ts ply newbot top
	where newbot = abminprune t (ply-1) bot top

abminprune :: GameState -> Int -> Int -> Int -> Int
abminprune st ply bot top | bot == top = top
		         | ply == 0 = max (min score top) bot
		         | otherwise = abminprunes ts ply bot top
			where ts = map snd (move_look st)
			      score = getHeuristic st

abminprunes :: [GameState] -> Int -> Int -> Int -> Int
abminprunes [] ply bot top = top
abminprunes (t:ts) ply bot top  = abminprunes ts ply bot newtop
	where newtop = abmaxprune t (ply-1) bot top 

pick_from :: [(Move, Int)] -> Move
pick_from mvs = fst (foldr (\m1 m2 -> if (snd m1 > snd m2) then m1 else m2) (last mvs) mvs)
--pick_from [] = bot
--pick_from (m:ms) = max (snd m) (pick_from ms)

-- redAI function
redAI :: GameState -> Move
redAI st = pick_from moves
	where moves = map (\(mv,s) -> (mv, (abminprune s ply bot top))) (move_look st)

-- blackAI function
blackAI :: GameState -> Move
blackAI st = pick_from moves
	where moves = map (\(mv,s) -> (mv, (abminprune s ply bot top))) (move_look st)

