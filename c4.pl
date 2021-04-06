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

% computerMove(Board, AI, AvailableMoves, Move) is true if Move is the best move out of AvailableMoves given the Board and AI\
% Random AI
computerMove(_, random, AvailableMoves, Move) :- random_member(Move, AvailableMoves).

% Game State Logics

teamColour(yellow).
teamColour(red).

% testNewBoard :- testBoard1(Board), newBoard(Board, 5, yellow, NB, 1), drawBoard(NB).

% DOESNT WORK
% newBoard([],_,_,_,_).
% newBoard([OldCol|RestOldCols], Move, TurnColour, [NewCol|RestOldCols], I) :-
%     Move = I,
%     placeMarkerOntoFirstEmptySpot(TurnColour, OldCol, NewCol).
% newBoard([OldCol|RestOldCols], Move, TurnColour, [OldCol|RestNewCols], I1) :-
%     Move \= I1,
%     newBoard(RestOldCols, Move, TurnColour, RestNewCols, I),
%     I1 is I-1.



% True when 3rd argument equals the Marker placed at the first empty slot of 2nd argument
placeMarkerOntoFirstEmptySpot(Marker, [H|RestMarkers], [H|Result]) :- 
    teamColour(Marker), teamColour(H), placeMarkerOntoFirstEmptySpot(Marker, RestMarkers, Result).
placeMarkerOntoFirstEmptySpot(Marker, [empty|RestMarkers], [Marker|RestMarkers]) :- 
    teamColour(Marker).  

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

getRow([],_,[]).
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