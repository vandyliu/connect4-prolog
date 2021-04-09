:- dynamic testNewBoard/1, testDrawFilledBoard/1.
:- use_module(library(clpfd)).
:- include(testBoards).

play :- initialBoard(Board), move(Board, player).

move(Board, player) :-
    nl,write('Your turn.'),nl,
    drawBoard(Board),
    read(Move),
    newBoard(Board, Move, red, NewBoard),
    transitionMove(NewBoard, player).

move(Board, computer) :-
    getAvailableMoves(Board, AvailableMoves),
    nl,write(AvailableMoves),nl,
    computerMove(Board, random, AvailableMoves, CPUMove),
    newBoard(Board, CPUMove, yellow, NewBoard),
    transitionMove(NewBoard, computer).

transitionMove(Board, player) :- checkForWin(Board, player).
transitionMove(Board, player) :- move(Board, computer).

transitionMove(Board, computer) :- checkForWin(Board, computer).
transitionMove(Board, computer) :- move(Board, player).

checkForWin(Board, computer) :- win(Board), nl, write('The computer won!'), nl, drawBoard(Board).
checkForWin(Board, player) :- win(Board), nl, write('You won!'), nl, drawBoard(Board).

initialBoard(Board) :- Board = [
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty]
    ].

getAvailableMoves([], []).
getAvailableMoves([Col|RestCol], [Index|AvailableMoves]) :- member(empty, Col),
                                                        length([Col|RestCol], N),
                                                        Index is (8-N),
                                                        getAvailableMoves(RestCol, AvailableMoves).


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