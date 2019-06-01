:- lib(ic).
:- import alldifferent/1 from ic_global.

solve(Sudoku) :-
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
    ),
    labeling(SudokuArray).

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
    ),
    labeling(B_Sudoku).


solve_channel(Sudoku) :-
    solve(Sudoku),
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


lambda(P) :- P =
        [[1,_,_, _,_,_, _,_,_],
         [_,_,2, 7,4,_, _,_,_],
         [_,_,_, 5,_,_, _,_,4],

         [_,3,_, _,_,_, _,_,_],
         [7,5,_, _,_,_, _,_,_],
         [_,_,_, _,_,9, 6,_,_],

         [_,4,_, _,_,6, _,_,_],
         [_,_,_, _,_,_, _,7,1],
         [_,_,_, _,_,1, _,3,_]].

b_lambda(P) :- P =
        [[[1,0,0,0,0,0,0,0,0],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_]],
         [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[0,1,0,0,0,0,0,0,0], [0,0,0,0,0,0,1,0,0],[0,0,0,1,0,0,0,0,0],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_]],
         [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [0,0,0,0,1,0,0,0,0],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[0,0,0,1,0,0,0,0,0]],

         [[_,_,_,_,_,_,_,_,_],[0,0,1,0,0,0,0,0,0],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_]],
         [[0,0,0,0,0,0,1,0,0],[0,0,0,0,1,0,0,0,0],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_]],
         [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[0,0,0,0,0,0,0,0,1], [0,0,0,0,0,1,0,0,0],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_]],

         [[_,_,_,_,_,_,_,_,_],[0,0,0,1,0,0,0,0,0],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[0,0,0,0,0,1,0,0,0], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_]],
         [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[0,0,0,0,0,0,1,0,0],[1,0,0,0,0,0,0,0,0]],
         [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_], [_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_],[1,0,0,0,0,0,0,0,0], [_,_,_,_,_,_,_,_,_],[0,0,1,0,0,0,0,0,0],[_,_,_,_,_,_,_,_,_]]].
