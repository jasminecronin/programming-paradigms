% Programming Paradigms - Peg Solitaire Solver
% Jasmine Cronin
% Solves 6 standard games of peg solitaire and prints out the list of
% board states to solve the game.
% I verbally collaborated with Quincent Masters on this assignment

% Functions to solve each type of game

peg(crossbow) :-
    write("Starting Board:"), nl,
    displayBoard([31,32,34,35,41,42,43,44,45,53]), % starting board for crossbow
    write("Goal Board:"), nl,
    displayBoard([3]), % goal state for crossbow
    write("Moves:"), nl,
    % get the list of winning moves
    solitaire_steps(crossbow,[31,32,34,35,41,42,43,44,45,53],[],X),
    % print all the board states from the list of winning moves
    displayMoves([31,32,34,35,41,42,43,44,45,53],X).

peg(longbow) :-
    write("Starting Board:"), nl,
    displayBoard([20,26,30,31,33,35,36,41,43,45,52,53,54,63]),
    write("Goal Board:"), nl,
    displayBoard([3]),
    write("Moves:"), nl,
    solitaire_steps(longbow,[20,26,30,31,33,35,36,41,43,45,52,53,54,63],[],X),
    displayMoves([20,26,30,31,33,35,36,41,43,45,52,53,54,63],X).

peg(standard) :-
    write("Starting Board:"), nl,
    displayBoard([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64]),
    write("Goal Board:"), nl,
    displayBoard([33]),
    write("Moves:"), nl,
    solitaire_steps(standard,[2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],[],X),
    displayMoves([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],X).

peg(not_quite_dead) :-
    write("Starting Board:"), nl,
    displayBoard([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64]),
    write("Goal Board:"), nl,
    displayBoard([33]),
    write("Moves:"), nl,
    solitaire_steps(not_quite_dead,[2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64],[],X),
    displayMoves([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64],X).

peg(half_dead) :-
    write("Starting Board:"), nl,
    displayBoard([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64]),
    write("Goal Board:"), nl,
    displayBoard([33]),
    write("Moves:"), nl,
    solitaire_steps(half_dead,[20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64],[],X),
    displayMoves([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64],X).

peg(almost_dead) :-
    write("Starting Board:"), nl,
    displayBoard([22,23,24,34,35,42,43,44]),
    write("Goal Board:"), nl,
    displayBoard([33]),
    write("Moves:"), nl,
    solitaire_steps(almost_dead,[22,23,24,34,35,42,43,44],[],X),
    displayMoves([22,23,24,34,35,42,43,44],X).

% Given a list of winning moves, display the list of board states
displayMoves(SB,[]).
displayMoves(SB,[(Start,End)|T]) :-
    jump(Start,J,End), % validate the move
    select(Start,SB,SB1), % remove the starting piece
    select(J,SB1,SB2), % remove the jumped piece
    select(End,SB3,SB2), % replace the starting piece in the end slot
    displayBoard(SB3), % print the board
    displayMoves(SB3,T). % recursively display the rest of the moves
    
% Given a board state, prints a representation to the console
displayBoard(B) :-
    printLine([0,1,2,3,4,5,6],B),
    printLine([10,11,12,13,14,15,16],B),
    printLine([20,21,22,23,24,25,26],B),
    printLine([30,31,32,33,34,35,36],B),
    printLine([40,41,42,43,44,45,46],B),
    printLine([50,51,52,53,54,55,56],B),
    printLine([60,61,62,63,64,65,66],B),
    nl.

% Prints a single line of a board state
printLine([],_) :- nl.
printLine([H|T],Board) :-
    % if the slot is not on the board, print a space
    % else if a peg occupies the slot, print an X
    % otherwise print a small o
    ((not(onboard(H)), write(" ")); (member(H,Board), write("X"));
    write("o")),
    write(" "), % space out the peg markers
    printLine(T,Board). % recursively print the rest of the line

% Finds the list of moves that lead to a winning state
% taken from code given in lecture
solitaire_steps(G,B,_,[]) :-
    final_board(G,B).
solitaire_steps(G,B,Hist,[Mv|Moves]) :-
    make_jump(B,Start,Jumped,End,NewBoard),
    Mv = (Start,End),
    independent(Mv,Hist),
    findall((P,W),(member(P,[asymmetric,rotated,top]),
                   wgt(P,NewBoard,W)),Wgts),
    check_wgts(G,Wgts),
    solitaire_steps(G,NewBoard,[Mv|Hist],Moves).
    
% Given a move, modifies a board to perform the move
% taken from code given in lecture
make_jump(SB,Start,Jumped,End,NewBoard) :-
    select(Start,SB,SB1),
    jump(Start,Jumped,End),
    select(Jumped,SB1,SB2),
    not(member(End,SB2)),
    append(SB2,[End],SB3),
    sort(SB3,NewBoard).

% Independence checking
% taken from code given in lecture
independent(Mv,[]).
independent(Mv,[H|T]) :-
    overlap(Mv,H),!.
independent(Mv,[H|T]) :-
    lexorder(Mv,H),
    independent(Mv,T).
    
% Check if two moves are overlapping, used in independence checking
overlap((Start1,End1),(Start2,End2)) :-
    jump(Start1,Jumped1,End1), % calculate each jumped piece
    jump(Start2,Jumped2,End2),
    (member(Start1,[Start2,Jumped2,End2]); % check for overlap
    member(Jumped1,[Start2,Jumped2,End2]);
    member(End1,[Start2,Jumped2,End2])).

% Checking weights using pagoda functions
% taken from code given in lecture
check_wgts(G,[]).
check_wgts(G,[(P,WgtP)|Rest]) :-
    goal_wgt(G,P,WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(G,Rest).
    
% Goal weights for pagoda functions
% taken from code given in lecture
wgt(_,[],0).
wgt(P,[Pos|Rest],Wgt) :-
    (pagoda(P,Pos,Pwgt); Pwgt = 0),!,
    wgt(P,Rest,Wgtrest),
    Wgt is Wgtrest + Pwgt.

% Determines ordering of move list
lexorder((Start1,End1),(Start2,End2)) :-
    Start1 =< Start2.

% Jump validation functions
% taken from code given in lecture
% jump right
jump(Start,Jumped,End) :-
    Jumped is Start + 1,
    End is Start + 2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% jump left
jump(Start,Jumped,End) :-
    Jumped is Start - 1,
    End is Start - 2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% jump up
jump(Start,Jumped,End) :-
    Jumped is Start + 10,
    End is Start + 20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% jump down
jump(Start,Jumped,End) :-
    Jumped is Start - 10,
    End is Start - 20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).
    
% Facts to determine if a given piece is on the board
% taken from code given in lecture
onboard(Pos) :- 2 =< Pos, Pos =< 4.
onboard(Pos) :- 12 =< Pos, Pos =< 14.
onboard(Pos) :- 20 =< Pos, Pos =< 26.
onboard(Pos) :- 30 =< Pos, Pos =< 36.
onboard(Pos) :- 40 =< Pos, Pos =< 46.
onboard(Pos) :- 52 =< Pos, Pos =< 54.
onboard(Pos) :- 62 =< Pos, Pos =< 64.
    
% Definitions of final goal states for each game type
final_board(crossbow,[3]).
final_board(longbow,[3]).
final_board(standard,[33]).
final_board(not_quite_dead,[33]).
final_board(half_dead,[33]).
final_board(almost_dead,[33]).

% Pagoda functions
% modelled after code given in lecture

pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,43,1).

pagoda(asymmetric,13,1).
pagoda(asymmetric,20,-1).
pagoda(asymmetric,21,1).
pagoda(asymmetric,23,1).
pagoda(asymmetric,25,1).
pagoda(asymmetric,26,-1).
pagoda(asymmetric,31,2).
pagoda(asymmetric,33,2).
pagoda(asymmetric,35,2).
pagoda(asymmetric,40,-1).
pagoda(asymmetric,41,1).
pagoda(asymmetric,43,1).
pagoda(asymmetric,45,1).
pagoda(asymmetric,46,-1).
pagoda(asymmetric,53,1).

pagoda(rotated,2,-1).
pagoda(rotated,4,-1).
pagoda(rotated,12,1).
pagoda(rotated,13,2).
pagoda(rotated,14,1).
pagoda(rotated,31,1).
pagoda(rotated,32,1).
pagoda(rotated,33,2).
pagoda(rotated,34,1).
pagoda(rotated,35,1).
pagoda(rotated,52,1).
pagoda(rotated,53,2).
pagoda(rotated,54,1).
pagoda(rotated,62,-1).
pagoda(rotated,64,-1).

pagoda(top,3,21).
pagoda(top,13,13).
pagoda(top,20,-8).
pagoda(top,21,8).
pagoda(top,23,8).
pagoda(top,25,8).
pagoda(top,26,-8).
pagoda(top,30,5).
pagoda(top,31,5).
pagoda(top,33,5).
pagoda(top,35,5).
pagoda(top,36,5).
pagoda(top,40,-3).
pagoda(top,41,3).
pagoda(top,43,3).
pagoda(top,45,3).
pagoda(top,46,-3).
pagoda(top,53,2).
pagoda(top,63,1).

pagoda(simple2,2,-1).
pagoda(simple2,4,-1).
pagoda(simple2,12,1).
pagoda(simple2,14,1).
pagoda(simple2,30,1).
pagoda(simple2,32,1).
pagoda(simple2,34,1).
pagoda(simple2,36,1).
pagoda(simple2,52,1).
pagoda(simple2,54,1).
pagoda(simple2,62,-1).
pagoda(simple2,64,-1).

% Facts to determine a goal board weight, given a game type and a pagoda function
% modelled after code given in lecture

goal_wgt(crossbow,simple,0).
goal_wgt(longbow,simple,0).
goal_wgt(standard,simple,1).
goal_wgt(half_dead,simple,1).
goal_wgt(almost_dead,simple,1).
goal_wgt(not_quite_dead,simple,1).

goal_wgt(crossbow,asymmetric,0).
goal_wgt(longbow,asymmetric,0).
goal_wgt(standard,asymmetric,2).
goal_wgt(half_dead,asymmetric,2).
goal_wgt(almost_dead,asymmetric,2).
goal_wgt(not_quite_dead,asymmetric,2).

goal_wgt(crossbow,rotated,0).
goal_wgt(longbow,rotated,0).
goal_wgt(standard,rotated,2).
goal_wgt(half_dead,rotated,2).
goal_wgt(almost_dead,rotated,2).
goal_wgt(not_quite_dead,rotated,2).

goal_wgt(crossbow,top,21).
goal_wgt(longbow,top,21).
goal_wgt(standard,top,5).
goal_wgt(half_dead,top,5).
goal_wgt(almost_dead,top,5).
goal_wgt(not_quite_dead,top,5).

goal_wgt(crossbow,simple2,0).
goal_wgt(longbow,simple2,0).
goal_wgt(standard,simple2,0).
goal_wgt(half_dead,simple2,0).
goal_wgt(almost_dead,simple2,0).
goal_wgt(not_quite_dead,simple2,0).