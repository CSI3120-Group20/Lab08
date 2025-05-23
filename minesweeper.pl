% Global Variables
:- dynamic mine/2.
:- dynamic revealed/2.
:- dynamic flagged/2.

% 1. Game Setup

% Initialize a new grid for a new game
% Must be called on 6
init_grid(6) :-
    retractall(mine(_,_)), % Clear all previous mines
    retractall(revealed(_,_)),
    retractall(flagged(_,_)),
    % Initialize random coordinates for the first mine
    X is random(6)+1,
    Y is random(6)+1,
    init_mines(6, X, Y).

% Base
init_mines(0, _, _) :- !.

% Recursive Case: mine does not exist at (X,Y)
init_mines(N, X, Y) :-
    not(mine(X,Y)),
    assertz(mine(X,Y)),
    X1 is random(6)+1,
    Y1 is random(6)+1,
    Remaining is N - 1,
    init_mines(Remaining, X1, Y1).

% Recursive Case: already a mine at (X,Y)
init_mines(N, X, Y) :-
    mine(X,Y),
    X1 is random(6)+1,
    Y1 is random(6)+1,
    init_mines(N, X1, Y1).

% 2. Cell Checking and Neighbour Count
% Cell Checking
reveal_cell(X,Y) :- assertz(revealed(X,Y)).

% Neighbour Count
adjacent(X,Y,Count9) :- X1 is X+1, Y1 is Y+1, XM1 is X-1, YM1 is Y-1,
    count_mine(X1, Y1, 0, Count1),
    count_mine(X, Y1, Count1, Count2),
    count_mine(XM1, Y1, Count2, Count3),

    count_mine(X1,Y, Count3, Count4),
    count_mine(X,Y, Count4, Count5),
    count_mine(XM1,Y, Count5, Count6),

    count_mine(X1,YM1, Count6, Count7),
    count_mine(X,YM1, Count7, Count8),
    count_mine(XM1,YM1, Count8, Count9).

% Increments Count only if a mine exists at X,Y
count_mine(X,Y, Count, Count1) :- mine(X,Y), Count1 is Count + 1.
count_mine(_,_, Count, Count1) :- Count1 is Count.

% 3. Game Interaction
% Start game
start_game() :- init_grid(6), display_grid(6).

% Flag
flag(X,Y) :- flagged(X,Y), write('Already flagged.').
flag(X,Y) :- mine(X,Y), all_mines_flagged(X,Y), write('You flagged all the mines. You win!\n'), display_grid(6), end_game().
flag(X,Y) :- assertz(flagged(X,Y)), write('Square flagged.\n'), display_grid(6).

all_mines_flagged(NewX,NewY) :- assertz(flagged(NewX,NewY)), setof((X,Y), (flagged(X,Y), mine(X,Y)), MinesFlagged), length(MinesFlagged, L), L is 6.

% Unflag
unflag(X,Y) :- retract(flagged(X,Y)).

% Check
check(X,Y) :- flagged(X,Y), write('The cell has been flagged.\n'), display_grid(6). % The cell has been flagged
check(X,Y) :- mine(X,Y), reveal_cell(X,Y), write('You hit a mine. You lose!\n'), display_grid(6), end_game(). % A mine was revealed
check(X,Y) :- write('Safe square.\n'), reveal_cell(X,Y), display_grid(6). % A safe square was revealed

% End Game
end_game() :- write('\nNew Game:\n'), init_grid(6), display_grid(6).

% 4. Display and Feedback

% Base case: N = 0, meaning no more characters needs to be printed
print_row(0,_) :- !. % cuts further backtracking when base case is reached

% Recursive case: (X,Y) is flagged
print_row(X,Y) :-
    flagged(X,Y),
    write('# '), % Symbol for flag
    Remaining is X - 1, % update the number of characters left to be printed
    print_row(Remaining,Y).

% Recursive case: (X,Y) has not been revealed
% Comment out this predicate to automatically reveal the whole grid (for testing purposes)
print_row(X,Y) :-
    not(revealed(X,Y)),
    write('□ '), % Funky UTF-16 symbol for unrevealed cell
    Remaining is X - 1, % update the number of characters left to be printed
    print_row(Remaining,Y).

% Recursive case: (X,Y) is a mine
print_row(X,Y) :-
    mine(X,Y),
    write('¤ '), % Funky UTF-16 symbol for mine
    Remaining is X - 1, % update the number of characters left to be printed
    print_row(Remaining,Y).

% Recursive case: (X,Y) is not a mine
print_row(X,Y) :-
    adjacent(X, Y, A),
    write(A), write(' '),
    Remaining is X - 1, % update the number of characters left to be printed
    print_row(Remaining,Y).

% Display the grid
display_grid(6) :- 
    print_row(6,6), write('\n'),
    print_row(6,5), write('\n'),
    print_row(6,4), write('\n'),
    print_row(6,3), write('\n'),
    print_row(6,2), write('\n'),
    print_row(6,1).