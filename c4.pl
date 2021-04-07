:- dynamic testNewBoard/1, testDrawFilledBoard/1.
:- use_module(library(clpfd)).

play :- initialBoard(Board), move(Board, player).

initialBoard(Board) :- Board = [
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty]
    ].

testBoard1(Board) :- Board = [
    [red, empty, empty, empty, empty, empty],
    [red, yellow, empty, empty, empty, empty],
    [yellow, yellow, red, red, empty, empty],
    [yellow, red, yellow, red, yellow, empty],
    [empty, empty, empty, empty, empty, empty],
    [yellow, empty, empty, empty, empty, empty],
    [yellow, yellow, red, red, red, yellow]
    ].

testBoardColWin(Board) :- Board = [
    [red, empty, empty, empty, empty, empty],
    [red, yellow, empty, empty, empty, empty],
    [yellow, yellow, yellow, yellow, empty, empty],
    [yellow, red, yellow, red, yellow, empty],
    [empty, empty, empty, empty, empty, empty],
    [yellow, empty, empty, empty, empty, empty],
    [yellow, yellow, red, red, red, yellow]
    ].

testBoardRowWin(Board) :- Board = [
    [red, empty, empty, empty, empty, empty],
    [red, yellow, empty, empty, empty, empty],
    [yellow, yellow, red, yellow, empty, empty],
    [yellow, red, yellow, red, yellow, empty],
    [red, red, empty, empty, empty, empty],
    [yellow, red, empty, empty, empty, empty],
    [yellow, red, red, red, yellow, yellow]
    ].

% computerMove(Board, AI, AvailableMoves, Move) is true if Move is the best move out of AvailableMoves given the Board and AI\
% Random AI
computerMove(_, random, AvailableMoves, Move) :- random_member(Move, AvailableMoves).

% Game State Logics

teamColour(yellow).
teamColour(red).

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
% win(Board) :- fourDiagonal(Board). TODO

fourHorizontal(Board) :- transpose(Board, TBoard), fourVertical(TBoard).

fourVertical([Col|RestCol]) :- fourInARow(Col).
fourVertical([Col|RestCol]) :- fourVertical(RestCol).

% fourInARow is true if the list contains four markers in a row
fourInARow(Row) :- take(4, Row, Elements), allSameElements(Elements).
fourInARow([H|T]) :- fourInARow(T).

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

% headOfList(L, H) is true if H is the head of list L
headOfList([H|_], H).

% replace(OriginalList, Index, Element, NewList).
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

take(N, List, Front):- length(Front, N), append(Front, _, List).