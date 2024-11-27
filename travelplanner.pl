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
    Total is Amount + Remaining,!,
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
% Check if a date is within the given range
date_in_range(Start, End, Check) :-
    Check @>= Start,  % Check is greater than or equal to Start
    Check @=< End.    % Check is less than or equal to End

% Filter destinations by date range
filter_destinations_by_date(StartDate, EndDate, Destination) :- 
    destination(Destination, DestinationStart, DestinationEnd, _),
    date_in_range(StartDate, EndDate, DestinationStart), % Ensure start date is within range
    date_in_range(StartDate, EndDate, DestinationEnd)   % Ensure end date is within range
    .  % Cut to prevent unnecessary backtracking

% Filter destinations by expense category
filter_destinations_by_expense_category(Category,Destination,Amount):-
    expense(Destination,Category,Amount),!.

% check if expense is within a specific range
expense_in_range(Low,High,Check):-
    Check >= Low,
    Check =< High.

% filter by expense amount range
filter_destinations_by_expense_amount(Low,High,Destination):-
    destination(Destination, _, _, _),  % Ensure 'Destination' exists
    findall(Amount, expense(Destination, _, Amount), Amounts),  % Collect all expenses for the destination
    sum_list(Amounts, Total),  % Calculate the total expenses
    expense_in_range(Low,High,Total).

% 5. Command Parsing with DCGs

% 6. Saving and Loading Journey (File I/O)
save(File):-
    open(File, write, Stream), % open file for writing
    % Save to destination, adds a '.' after each fact to ensure syntax correctness, adds a newline character afterward
    forall(destination(Name, StartDate, EndDate, Budget),
           (write(Stream, destination(Name, StartDate, EndDate, Budget)), write(Stream, '.'), nl(Stream))),
    % Save expenses to destintation
    forall(expense(Destination, Category, Amount),
           (write(Stream, expense(Destination, Category, Amount)), write(Stream, '.'), nl(Stream))),
    close(Stream),  % Close the file
    format("Journey saved to file: ~w\n", [File]). % prints a message to the console indicating the journey have been saved successfully



load(File) :-
    (   exists_file(File) -> % checks if file exists
        % error handling
        catch(
            (open(File, read, Stream), % open file for reading
             load_terms(Stream), % Load a fact 
             close(Stream),
             format("Journey loaded successfully from file: ~w\n", [File]) % output a message to console if file is successfully loaded
            ),
            Error, % catch any error raised 
            handle_error(Error, File)
        )
    ;   format("Error: File ~w does not exist.\n", [File]) % outputs a messag if file does not exist
    ).

% Auxiliary predicate to load terms from a file
load_terms(Stream) :-
    % loop until terminated by !
    repeat,
    % error handling
    catch(
        read(Stream, Term),  % Attempt to read a term
        Error,
        (format("Error reading from file: ~w\n", [Error]), fail) % if an error is caught, outputs a message
    ),
    (   Term == end_of_file -> % check if reached end of file
        !
    ;   (   validate_term(Term) -> % Validate format of the term
            assertz(Term)  % Assert valid terms
        ;   format("Warning: Invalid term skipped - ~w\n", [Term]) % outputs a message if term has invaid format
        ),
        fail % forces backtracking
    ).

% Auxiliary predicate to validate the format of terms
validate_term(Term) :-
    (   Term = destination(Name, StartDate, EndDate, Budget),
        atom(Name),              % Name must be an atom
        atom(StartDate),         % StartDate must be an atom
        atom(EndDate),           % EndDate must be an atom
        number(Budget)           % Budget must be a number
    ;   Term = expense(Destination, Category, Amount),
        atom(Destination),       % Destination must be an atom
        atom(Category),          % Category must be an atom
        number(Amount)           % Amount must be a number
    ).


% Error handler
handle_error(Error, File) :-
    format("Error while processing file ~w: ~w\n", [File, Error]). % outputs a message if error caught when processing the file

       