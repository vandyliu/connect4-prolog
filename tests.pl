% Some simple tests that we had while programming the game

:- include(c4).

% Tests:
% placeMarkerOntoFirstEmptySpot(red, [red, yellow, empty, empty], X).
% placeMarkerOntoFirstEmptySpot(red, [red, yellow, red, empty], X).
% placeMarkerOntoFirstEmptySpot(red, [red, yellow, red, red], X).

testDrawBoard :- testBoard1(Board), drawBoard(Board).

testDrawFilledBoard :- testBoard1(Board), drawBoard(Board).

testNewBoard :- testBoard1(Board), newBoard(Board, 5, yellow, NB), drawBoard(NB).

testGetAvailableMoves(Y) :- testBoard1(X), getAvailableMoves(X, Y).

testPlay :- testBoardAlmostFilled(Board), move(Board, player, red, simple).

