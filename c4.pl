play :- initialBoard(Board), move(Board, player).

initialBoard(Board) :- Board = [
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty]
    ].

% computerMove(Board, AI, AvailableMoves, Move) is true if Move is the best move out of AvailableMoves given the Board and AI\
% Random AI
computerMove(_, random, AvailableMoves, Move) :- random_member(Move, AvailableMoves).

%%%%%%%%%%% Board Drawing %%%%%%%%%%%%%%%%%%%%%%%%%
testDrawBoard :- initialBoard(Board), drawBoard(Board).

drawBoard([]).
drawBoard(Board) :- drawHeadings, drawRows(Board).

drawHeadings :- ansi_format([bold,fg(black)], '1  2  3  4  5  6  7 ~w', [' ']), nl.

drawRows([LastRow]) :- drawRow(LastRow).
drawRows([H|T]) :- drawRow(H), nl, drawRows(T).
        
drawRow([]).
drawRow([yellow|RestRow]) :- ansi_format([bold,fg(yellow)], 'O ~w', [' ']), drawRow(RestRow).
drawRow([red|RestRow]) :- ansi_format([bold,fg(red)], 'O ~w', [' ']), drawRow(RestRow).
drawRow([empty|RestRow]) :- ansi_format([bold,fg(black)], 'O ~w', [' ']), drawRow(RestRow).


%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% member(X,L) is true if X is an element of list L
member(X,[X|_]).
member(X,[_|R]) :-
    member(X,R).

% headOfList(L, H) is true if H is the head of list L
headOfList([H|_], H).