% 1. Destination Management
% Implement a dynamic predicate destination/4 to store travel destinations with the following attributes:
% ▪ Name: Destination name (e.g., "Paris").
% ▪ Start Date: Date of arrival.
% ▪ End Date: Date of departure.
% ▪ Budget: Budget allocated for this destination.

% Declare the destination/4 predicate as dynamic
% Allowing add facts and remove facts during runtime.
:- dynamic destination/4.

% Add a new destination
add_destination(Name, StartDate, EndDate, Budget) :-
    % Check if the destination already exists (\+ like negation, return false if the destination is found)
    % The `Name` is unique
    \+ destination(Name, _, _, _),

    % Assert the new destination
    assertz(destination(Name, StartDate, EndDate, Budget)),

    % Print a confirmation message, `~w` is a placeholder for [Name]
    format("Destination '~w' added successfully.\n", [Name]).


% Remove a destination by name
remove_destination(Name) :-
    % Check if the destination already exists
    % The `Name` is unique
    destination(Name, _, _, _),

    % Retract the matching destination
    retract(destination(Name, _, _, _)),

    % Print a confirmation message, `~w` is a placeholder for [Name]
    format("Destination '~w' removed successfully.\n", [Name]).



% 2. Expense Tracking
% Define a dynamic predicate expense/3 to track expenses associated with each destination:
% ▪ Destination: Name of the destination (e.g., "Paris").
% ▪ Category: Expense category (e.g., "food", "transport").
% ▪ Amount: Cost of the expense.

% Declare the expense/3 predicate as dynamic
:- dynamic expense/3.

% Add an expense
add_expense(Destination, Category, Amount) :-
    % Make sure the destination exists
    destination(Destination, _, _, _),
    
    % Make sure the expense does not exists
    % The `Destination` and `Category` are unique
    \+ expense(Destination, Category, _),

    % Assert the new expense
    assertz(expense(Destination, Category, Amount)),

    % Print a confirmation message, `~w - ~w - ~w` is a placeholder for [Destination, Category, Amount]
    format("Expense '~w - ~w - ~w' added successfully\n", [Destination, Category, Amount]).


% Remove an expense for a destination
remove_expense(Destination, Category) :-
    % Make sure the expense exists
    % The `Destination` and `Category` are unique
    expense(Destination, Category, _),

    % Retract the matching destination
    retract(expense(Destination, Category, _)),

    format("Expense '~w - ~w' removed successfully\n", [Destination, Category]),

    % Cut here to stop after finding the first match, since the `Destination` and `Category` are unique for each expense
    !.


% Calculate total expenses for each `destination`

% Wrapper predicate: Gather expenses and compute the total
total_expenses(Destination, Total) :-
    findall(Expense, expense(Destination, _, Expense), ExpensesList),
    sum_expenses(ExpensesList, Total).

% Base case: No expenses in the `ExpensesList`
sum_expenses([], 0).

% Recursive case: Add the first expense from the `ExpensesList` and process the rest
sum_expenses([HeadExpense | Rest], Total) :-
    sum_expenses(Rest, RemainingExpenses),
    Total is HeadExpense + RemainingExpenses.



% 3. Budget Validation
% Write a predicate to set and validate a budget for each destination.
% Ensure the 2 predicate checks whether total expenses exceed the budget.

% Update the budget for a destination
set_budget(Destination, NewBudget) :-
    % Ensure the destination exists
    destination(Destination, StartDate, EndDate, _),

    % Note: Prolog Facts are Immutable:
    % Retract the old destination fact
    retract(destination(Destination, StartDate, EndDate, _)),

    % Assert the new fact with the updated budget
    assertz(destination(Destination, StartDate, EndDate, NewBudget)),

    format("Budget for '~w' updated to ~w.\n", [Destination, NewBudget]).


% Validate the budget for a destination
validate_budget(Destination) :-
    destination(Destination, _, _, Budget),
    total_expenses(Destination, TotalExpenses),
    (
        TotalExpenses =< Budget 
        
        % If the expenses are within the budget
        ->  format("Expenses for '~w' are within the budget. Total: ~w, Budget: ~w\n", [Destination, TotalExpenses, Budget])
        
        % else
        ;   format("Warning: Expenses for '~w' exceed the budget! Total: ~w, Budget: ~w\n", [Destination, TotalExpenses, Budget])
    ),

    % Cut here to stop after finding the first match, since each destination entry is unique
    !.



% 4. Filtering Destinations and Expenses
% Write predicates to filter destinations by date or expense category.
% Implement backtracking control with the cut operator where necessary

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
% o Use Definite Clause Grammars (DCGs) to parse user commands, including:
%   ▪ Adding a destination
%   ▪ Removing a destination
%   ▪ Listing expenses for a destination
%   ▪ Checking budget

% Main rule
% Parse input and execute corresponding command
process_command(InputList) :-
    % The predicate `phrase/2` attempts to match the `InputList` with the grammar rule defined in `command(Command)`
    % If the match is successful, The variable `Command` will store the parsed result.
    phrase(command(Command), InputList),
    
    % Execute the matched command
    execute_command(Command).


% Define Grammar Rules for Parsing Commands

% 5.1. Adding a destination
% `add_destination(Name, StartDate, EndDate, Budget)` is used to store the phrased result
command(add_destination(Name, StartDate, EndDate, Budget)) -->
    % The actual input is instantiated when invoking `phrase/2`

    % The expected input format (i.e. how the input is matched)
    ['add', 'destination', Name, 'from', StartDate, 'to', EndDate, 'with', 'budget', Budget],

    {
        % Ensures that `Name` is an atom. In Prolog, atoms are used for symbolic values or constants (like 'Paris' or 'London').
        atom(Name),
        atom(StartDate),
        atom(EndDate),

        % Check if `Budget` is a number. return true if `Budget` is numeric (integer or float)
        number(Budget)
    }.


% 5.2. Removing a destination
% if the input format defined below matches, the output will be a Prolog term `remove_destination(Name)`
command(remove_destination(Name)) -->
    % The expected input format: `['remove', 'destination', Name]`, start with the word 'remove', followed by the word 'destination'.
    ['remove', 'destination', Name],

    { atom(Name) }.


% 5.3. Listing expenses for a destination
command(list_expenses(Destination)) -->
    ['list', 'expenses', 'for', Destination],
    { atom(Destination) }.

% 5.4. Checking budget
command(validate_budget(Destination)) -->
    ['check', 'budget', 'for', Destination],
    { atom(Destination) }.


% Execute the command
% If the argument passed to `execute_command` matches the pattern `add_destination(Name, StartDate, EndDate, Budget)`
% then the clause in the body (i.e. add_destination(Name, StartDate, EndDate, Budget)) will be executed
execute_command(add_destination(Name, StartDate, EndDate, Budget)) :-
    add_destination(Name, StartDate, EndDate, Budget).

execute_command(remove_destination(Name)) :-
    remove_destination(Name).

execute_command(list_expenses(Destination)) :-
    format("Expenses for destination '~w':\n", [Destination]),
    forall(expense(Destination, Category, Amount),
           format("  - ~w: ~w\n", [Category, Amount])).

execute_command(validate_budget(Destination)) :-
    validate_budget(Destination).

% Handle unknown commands
execute_command(_) :-
    format("Unknown command or invalid format.\n").



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