:- lib(ic).
:- lib(util).
:- compile("sudex_toledo").
%% :- import alldifferent/1 from ic_global.

solve_puzzles :-
    solve_name(_),
    fail.

solve_name(Name) :-
    puzzles(P,Name),
    write(Name),nl,
    cputime(T1),
    solve(P,SudokuArray),
    search(SudokuArray, 0, input_order,indomain,complete,[backtrack(B)]),
    cputime(T2),
    TimeTaken is T2 - T1,
    write(TimeTaken),nl,
    write(B),nl.

solve_puzzles_channel :-
    solve_name_channel(_),
    fail.

solve_name_channel(Name) :-
    puzzles(P,Name),
    write(Name),nl,
    cputime(T1),
    solve_channel(P,SudokuArray),
    search(SudokuArray, 0, first_fail,indomain,complete,[backtrack(B)]),
    cputime(T2),
    TimeTaken is T2 - T1,
    write(TimeTaken),nl,
    write(B),nl.

solve_puzzles_bool :-
    solve_name_bool(_),
    fail.

solve_name_bool(Name) :-
    puzzles(P,Name),
    write(Name),nl,
    cputime(T1),
    solve_bool(P,B_Sudoku),
    search(B_Sudoku, 0, first_fail,indomain,complete,[backtrack(B)]),
    cputime(T2),
    TimeTaken is T2 - T1,
    write(TimeTaken),nl,
    write(B),nl.

solve(Sudoku,SudokuArray) :-
    % Convert list to array
    (foreach(Row,Sudoku), foreach(RowArray,Out)
    do
      array_list(RowArray,Row)
    ),
    array_list(SudokuArray,Out),

    % Dimension and domain
    dim(SudokuArray, [N,N]),
    SudokuArray[1..N,1..N] :: 1..N,
    
    % Check if columns and rows different
    ( for(I,1,N), param(SudokuArray) do
        alldifferent(SudokuArray[I,*]),
        alldifferent(SudokuArray[*,I])
    ),

    % Check if blocks are different
    Block is integer(sqrt(N)),
    ( multifor([I,J],1,N,Block), param(SudokuArray,Block) do
        alldifferent(concat(SudokuArray[I..I+Block-1, J..J+Block-1]))
    ).

solve_bool(Sudoku, B_Sudoku) :-
    (foreach(Row,Sudoku), foreach(RowArray,Out)
    do
      array_list(RowArray,Row)
    ),
    array_list(SudokuArray,Out),

    % Dimension and domain
    dim(SudokuArray, [N,N]),
    SudokuArray[1..N,1..N] :: 1..N,
    % Dimension and domain
    dim(B_Sudoku, [N,N,N]),
    B_Sudoku[1..N,1..N,1..N] :: 0..1,
    convert_sudoku_to_bool(SudokuArray, B_Sudoku),

    % Check if each square only possesses one element
    ( multifor([I,J],1,N), param(B_Sudoku) do
        sum(B_Sudoku[I,J,*]) #= 1
    ),

    % Check if columns and rows have a sum of one for each element
    ( multifor([I,J],1,N), param(B_Sudoku) do
        sum(B_Sudoku[I,*,J]) #= 1,
        sum(B_Sudoku[*,I,J]) #= 1
    ),

    % Check if blocks have a sum of one for each element
    Block is integer(sqrt(N)),
    ( multifor([I,J],1,N,Block), param(B_Sudoku,Block,N) do
        ( for(K,1,N), param(B_Sudoku,Block, I, J) do
            sum(concat(B_Sudoku[I..I+Block-1, J..J+Block-1, K])) #= 1
        )
    ).


solve_channel(Sudoku,SudokuArray) :-
    solve(Sudoku,SudokuArray),
    solve_bool(Sudoku, B_Sudoku),
    sudoku_channel(Sudoku, B_Sudoku).


convert_sudoku_to_bool(SudokuArray, B_Sudoku) :-
    dim(SudokuArray, [N,N]),
    dim(B_Sudoku, [N,N,N]),
    ( for(Row,1,N), param(SudokuArray,B_Sudoku, N) do
        ( for(Col,1,N), param(SudokuArray,B_Sudoku, N, Row) do
            Value is SudokuArray[Row, Col],
            ( for(Element,1,N), param(B_Sudoku, Row, Col, Value) do
                BoolValue is B_Sudoku[Row, Col, Element],
                ( number(Value), Element =:= Value ->
                    BoolValue #= 1
                    ;
                    true
                )
            )
        )
    ).


sudoku_channel(Sudoku, B_Sudoku) :-
    (foreach(Row,Sudoku), foreach(RowArray,Out)
    do
      array_list(RowArray,Row)
    ),
    array_list(SudokuArray,Out),
    dim(SudokuArray, [N,N]),
    dim(B_Sudoku, [N,N,N]),
    ( for(Row,1,N), param(SudokuArray,B_Sudoku, N) do
        ( for(Col,1,N), param(SudokuArray,B_Sudoku, N, Row) do
            Value is SudokuArray[Row,Col], 
            ValueBools is B_Sudoku[Row,Col,1..N], 
            ic_global:bool_channeling(Value,ValueBools,1)
        )
    ).