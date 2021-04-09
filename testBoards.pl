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

testBoardDiagBLTRWin(Board) :- Board = [
    [red, empty, empty, empty, empty, empty],
    [red, yellow, empty, empty, empty, empty],
    [yellow, yellow, red, yellow, empty, empty],
    [red, red, yellow, red, yellow, empty],
    [red, red, empty, empty, empty, empty],
    [yellow, yellow, red, empty, empty, empty],
    [yellow, red, red, red, yellow, yellow]
    ].

testBoardDiagTLBRWin(Board) :- Board = [
    [red, empty, empty, empty, empty, empty],
    [red, yellow, empty, empty, empty, empty],
    [yellow, yellow, red, yellow, empty, empty],
    [red, red, yellow, yellow, yellow, empty],
    [red, red, yellow, empty, empty, empty],
    [yellow, yellow, yellow, empty, empty, empty],
    [yellow, red, red, red, yellow, yellow]
    ].