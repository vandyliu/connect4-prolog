:- dynamic testNewBoard/1, testDrawFilledBoard/1.
:- discontiguous computerMove/4.
:- use_module(library(clpfd)).
:- include(boards).

%%%%%%%%%%%%%%%%%% Starting Game Logistics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tutorial :- write("Type the column followed by a period to place a marker. Eg. \"4.\" to place a marker in column 4.").

playAgain :- nl,write("Type \"play.\" to play again!"),nl.

chooseAlgo(FinalAlgo) :-
    nl, write('Type \"simple.\" to play against a simple AI, \"random.\" to play against a random AI and anything else followed by a period to play agains Monte Carlo AI.'),
    read(Algo),
    getAlgo(Algo, FinalAlgo).

getAlgo(Algo, simple) :- Algo = 'simple', write('You chose simple AI.').
getAlgo(Algo, random) :- Algo = 'random', write('You chose random AI.').
getAlgo(Algo, random) :- \+ Algo = 'simple', \+ Algo = 'random', write('You chose Monte Carlo AI.'). % Change later

chooseColour(FinalColour) :- 
    nl,write('Type \"red.\" to play as red or anything else followed by a period to play as yellow.'),nl,
    read(Colour),
    getColour(Colour, FinalColour).

getColour(Colour, red) :- Colour = 'red', write('You chose red.').
getColour(Colour, yellow) :- \+ Colour = 'red', write('You chose yellow.').

play :- tutorial, chooseColour(Colour), chooseAlgo(Algo), startGame(Colour, Algo).

startGame(red, Algo) :- initialBoard(Board), move(Board, player, red, Algo).
startGame(yellow, Algo) :- initialBoard(Board), move(Board, computer, red, Algo).

testPlay :- testBoardAlmostFilled(Board), move(Board, player, red, simple).

%%%%%%%%%%%%%%%%% Computer Algorithms %%%%%%%%%%%%%%%%%%%%%

% computerMove(Board, AI, AvailableMoves, Move) is true if Move is the best move out of AvailableMoves given the Board and AI\

% Simple AI
computerMove(_, simple, AvailableMoves, Move) :- getSimpleMove(AvailableMoves, Move, [4,5,3,6,2,7,1]). 
getSimpleMove([X], X, _).
getSimpleMove(AvailableMoves, Column, [Column|_]) :- 
    member(Column, AvailableMoves).
getSimpleMove(AvailableMoves, Move, [Column|T]) :- 
    notMember(Column, AvailableMoves), getSimpleMove(AvailableMoves, Move, T).

% Random AI
computerMove(_, random, AvailableMoves, Move) :- random_member(Move, AvailableMoves).


%%%%%%%%%%%%%%%%%%%%%%% Gameplay Logistics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move(Board, player, Colour, Algo) :-
    nl,write('Your turn as '), write(Colour), write('.'),
    getAvailableMoves(Board, AvailableMoves),
    nl,write("Choose from: "),write(AvailableMoves),nl,
    drawBoard(Board),
    read(Move),
    validMoveCheck(Move, AvailableMoves, Board, Colour, Algo).

move(Board, computer, Colour, Algo) :-
    getAvailableMoves(Board, AvailableMoves),
    computerMove(Board, Algo, AvailableMoves, CPUMove),
    newBoard(Board, CPUMove, Colour, NewBoard),
    transitionMove(NewBoard, computer, Colour, Algo).

validMoveCheck(Move, AvailableMoves, Board, Colour, Algo) :-
    member(Move, AvailableMoves),
    newBoard(Board, Move, Colour, NewBoard),
    transitionMove(NewBoard, player, Colour, Algo).
validMoveCheck(Move, AvailableMoves, Board, Colour, Algo) :-
    notMember(Move, AvailableMoves),
    nl,write("Invalid move. Try again."),
    move(Board, player, Colour, Algo).

transitionMove(Board, player, _, _) :- checkForWin(Board, player).
transitionMove(Board, player, Colour, Algo) :- otherColour(Colour, OtherColour), move(Board, computer, OtherColour, Algo).

transitionMove(Board, computer, _, _) :- checkForWin(Board, computer).
transitionMove(Board, computer, Colour, Algo) :- otherColour(Colour, OtherColour), move(Board, player, OtherColour, Algo).

checkForWin(Board, computer) :- win(Board), nl, write('The computer won!'), nl, drawBoard(Board), playAgain.
checkForWin(Board, player) :- win(Board), nl, write('You won!'), nl, drawBoard(Board), playAgain.
checkForWin(Board, _) :- 
    getAvailableMoves(Board, Moves),
    length(Moves, L),
    L = 0,
    nl, write('It\'s a tie!'), nl,
    drawBoard(Board),
    playAgain.

getAvailableMoves([], []).
getAvailableMoves([Col|RestCol], [Index|AvailableMoves]) :- 
    member(empty, Col),
    length([Col|RestCol], N),
    Index is (8-N),
    getAvailableMoves(RestCol, AvailableMoves).
getAvailableMoves([Col|RestCol], AvailableMoves) :- 
    notMember(empty, Col),
    getAvailableMoves(RestCol, AvailableMoves).

testGetAvailableMoves(Y) :- testBoard1(X), getAvailableMoves(X, Y).

% Game State Logics

teamColour(yellow).
teamColour(red).

otherColour(yellow, red).
otherColour(red, yellow).

testDrawFilledBoard :- testBoard1(Board), drawBoard(Board).
testNewBoard :- testBoard1(Board), newBoard(Board, 5, yellow, NB), drawBoard(NB).

% newBoard(OldBoard, Move, TurnColour, NewBoard) is true when NewBoard is the board after TurnColour is played in column Move in the board OldBoard.
newBoard([],_,_,_,_).
newBoard(Board, Move, TurnColour, NewBoard) :-
    nth1(Move, Board, OldColumn), % Get nth column
    placeMarkerOntoFirstEmptySpot(TurnColour, OldColumn, NewColumn), % Place marker in column
    replace(Board, Move, NewColumn, NewBoard). % Replace column in old board with new column

% True when 3rd argument equals the Marker placed at the first empty slot of 2nd argument
placeMarkerOntoFirstEmptySpot(Marker, [H|RestMarkers], [H|Result]) :- 
    teamColour(Marker), teamColour(H), placeMarkerOntoFirstEmptySpot(Marker, RestMarkers, Result).
placeMarkerOntoFirstEmptySpot(Marker, [empty|RestMarkers], [Marker|RestMarkers]) :- teamColour(Marker).
% Tests:
% placeMarkerOntoFirstEmptySpot(red, [red, yellow, empty, empty], X).
% placeMarkerOntoFirstEmptySpot(red, [red, yellow, red, empty], X).
% placeMarkerOntoFirstEmptySpot(red, [red, yellow, red, red], X).

%%%%%%%% Win conditions %%%%%%%%%%%%
win(Board) :- fourVertical(Board).
win(Board) :- fourHorizontal(Board).
win(Board) :- fourDiagonal(Board).

fourHorizontal(Board) :- transpose(Board, TBoard), fourVertical(TBoard).

fourVertical([Col|_]) :- fourInARow(Col).
fourVertical([_|RestCol]) :- fourVertical(RestCol).

fourDiagonal(Board) :- zip([1,1,1,1,1,1,2,3,4,5,6,7],[1,2,3,4,5,6,1,1,1,1,1,1], L), checkBLTRAtPoints(Board, L).
fourDiagonal(Board) :- zip([1,1,1,1,1,1,2,3,4,5,6,7],[1,2,3,4,5,6,6,6,6,6,6,6], L), checkTLBRAtPoints(Board, L).

checkBLTRAtPoints(Board, [(C,R)|_]) :- getDiagonalBottomLeftToTopRight(Board, C, R, D), fourInARow(D).
checkBLTRAtPoints(Board, [_|T]) :- checkBLTRAtPoints(Board, T).

checkTLBRAtPoints(Board, [(C,R)|_]) :- getDiagonalTopLeftToBottomRight(Board, C, R, D), fourInARow(D).
checkTLBRAtPoints(Board, [_|T]) :- checkTLBRAtPoints(Board, T).

% L is list on the diagonal from given starting coords, going bot left -> top right
getDiagonalBottomLeftToTopRight(_, C, _, []) :- C < 1.
getDiagonalBottomLeftToTopRight(_, C, _, []) :- C > 7.
getDiagonalBottomLeftToTopRight(_, _, R, []) :- R < 1.
getDiagonalBottomLeftToTopRight(_, _, R, []) :- R > 6.
getDiagonalBottomLeftToTopRight(Board, C, R, [Marker|L]) :-
    nth1(C, Board, Col),
    nth1(R, Col, Marker),
    C1 is C+1,
    R1 is R+1,
    getDiagonalBottomLeftToTopRight(Board, C1, R1, L).

% L is list on the diagonal from given starting coords, going topleft-> bottomright
getDiagonalTopLeftToBottomRight(_, C, _, []) :- C < 1.
getDiagonalTopLeftToBottomRight(_, C, _, []) :- C > 7.
getDiagonalTopLeftToBottomRight(_, _, R, []) :- R < 1.
getDiagonalTopLeftToBottomRight(_, _, R, []) :- R > 6.
getDiagonalTopLeftToBottomRight(Board, C, R, [Marker|L]) :-
    nth1(C, Board, Col),
    nth1(R, Col, Marker),
    C1 is C+1,
    R1 is R-1,
    getDiagonalTopLeftToBottomRight(Board, C1, R1, L).

% fourInARow is true if the list contains four markers in a row
fourInARow(Row) :- take(4, Row, Elements), allSameElements(Elements).
fourInARow([_|T]) :- fourInARow(T).

allSameElements([H|T]) :- teamColour(H), allSameElements([H|T], H).
allSameElements([H], H).
allSameElements([H|T], H) :- allSameElements(T, H).

%%%%%%%%%%% Board Drawing %%%%%%%%%%%%%%%%%%%%%%%%%

testDrawBoard :- testBoard1(Board), drawBoard(Board).

drawBoard([]).
drawBoard(Board) :- drawHeadings, drawRows(Board, [6,5,4,3,2,1]).

drawHeadings :- ansi_format([bold,fg(black)], '1  2  3  4  5  6  7 ~w', [' ']), nl.

drawRows(_, []).
drawRows(Board, [H|T]) :-
    drawRow(Board, H),
    nl,
    drawRows(Board, T).

drawRow(Board, N) :-
    getRow(Board, N, Row),
    drawList(Row).

getRow([], _, []).
getRow([Col|RestCols], N, [Colour|Row]) :-
    nth1(N, Col, Colour),
    getRow(RestCols, N, Row).
        
drawList([]).
drawList([yellow|RestRow]) :- ansi_format([bold,fg(yellow)], 'O ~w', [' ']), drawList(RestRow).
drawList([red|RestRow]) :- ansi_format([bold,fg(red)], 'O ~w', [' ']), drawList(RestRow).
drawList([empty|RestRow]) :- ansi_format([bold,fg(black)], 'O ~w', [' ']), drawList(RestRow).


%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% member(X,L) is true if X is an element of list L
member(X,[X|_]).
member(X,[_|R]) :-
    member(X,R).

notMember(X, L) :- \+ member(X, L).

% headOfList(L, H) is true if H is the head of list L
headOfList([H|_], H).

% replace(OriginalList, Index, Element, NewList).
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% take(N, List, Front) is true if Front is the first N elements of List
take(N, List, Front):- length(Front, N), append(Front, _, List).

% zip(X, Y, Z) is true if Z is a list of pairs corresponding to values (X,Y).
zip([], [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|Zs]) :- zip(Xs,Ys,Zs).