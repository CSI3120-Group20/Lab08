% 1. Destination Management
:- dynamic destination/4. % Declare the destination/4 predicate as dynamic
% Add a new destination
add_destination(Name, StartDate, EndDate, Budget) :-
    % Check if the destination already exists
    \+ destination(Name, _, _, _),
    % Assert the new destination
    assertz(destination(Name, StartDate, EndDate, Budget)),
    format("Destination '~w' added successfully.\n", [Name]).% suggested by GPT

% Remove a destination by name
remove_destination(Name) :-
    % Check if the destination exists
    destination(Name, StartDate, EndDate, Budget),
    % Retract all matching facts
    retract(destination(Name, StartDate, EndDate, Budget)),
    format("Destination '~w' removed successfully.\n", [Name]).
% 2. Expense Tracking
:- dynamic expense/3.
% Add an expense for a destination
add_expense(Destination, Category, Amount) :-
    destination(Destination, _, _, _),  % Make sure the destination exists
    assertz(expense(Destination, Category, Amount)),
    format("Expense added: ~w - ~w - ~w\n", [Destination, Category, Amount]).

% Remove an expense for a destination
remove_expense(Destination, Category, Amount) :-
    expense(Destination, Category, Amount), % Make sure the expense exists
    retract(expense(Destination, Category, Amount)),
    format("Expense removed: ~w - ~w - ~w\n", [Destination, Category, Amount]).

%REMOVE UNNECESSARY BACKTRACKING
% Base case: No more expenses for the destination
total_expenses(Destination, 0) :-
    \+ expense(Destination, _, _), !.
% Recursive case: Sum expenses
total_expenses(Destination, Total) :-
    expense(Destination, _, Amount),
    retract(expense(Destination, _, Amount)),  % Temporarily retract to avoid duplicates
    total_expenses(Destination, Remaining),
    Total is Amount + Remaining,
    assertz(expense(Destination, _, Amount)).  % Re-assert the retracted fact 

% 3. Budget Validation
% Update the budget for a destination
set_budget(Destination, NewBudget) :-
    destination(Destination, StartDate, EndDate, _),
    retract(destination(Destination, StartDate, EndDate, _)),
    assertz(destination(Destination, StartDate, EndDate, NewBudget)),
    format("Budget for '~w' updated to ~w.\n", [Destination, NewBudget]).

% Validate the budget for a destination
validate_budget(Destination) :-
    destination(Destination, _, _, Budget),
    total_expenses(Destination, TotalExpenses),
    (
        TotalExpenses =< Budget ->
        format("Expenses for '~w' are within the budget. Total: ~w, Budget: ~w\n", [Destination, TotalExpenses, Budget]);
        format("Warning: Expenses for '~w' exceed the budget! Total: ~w, Budget: ~w\n", [Destination, TotalExpenses, Budget])
    ),!.

% 4. Filtering Destinations and Expenses

% 5. Command Parsing with DCGs

% 6. Saving and Loading Journey (File I/O)