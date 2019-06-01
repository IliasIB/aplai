:- use_module(library(chr)).
%% solve_bool/2 solve_channel/1 sudoku_channel/2. convert_sudoku_to_bool/2 sudoku/4
:- chr_constraint solve/1.
:- chr_constraint cleanup/0, solver/1, fill/0, square/3.
%% :- chr_constraint print_solution/0, print_solution/1, print_solution/2.
:- consult('sudex_toledo.pl').

solve(Sudoku) <=>
    make_squares(Sudoku, 0),
    fill,
    print_solution,
    cleanup.

make_squares([], _, _):- true.
make_squares([Value|Rest], I, J) :-
    NewJ is J+1,
    make_squares(Rest, I, NewJ),
    numlist(1, 9, Domain),
    (number(Value)->square(I,J,num(Value)) ; 
    square(I,J,domain(Domain))).
make_squares([],_):- !.
make_squares([Value|Rest], I):-
    NewI is I+1,
    make_squares(Value, I, 0),
    make_squares(Rest, NewI).

square(X,Y1,num(Value)), square(X,Y2,num(Value)) # passive <=> Y1 \= Y2 |
    false.
square(X1,Y,num(Value)), square(X2,Y,num(Value)) # passive <=> X1 \= X2 |
    false.
square(X1,Y1,num(Value)), square(X2,Y2,num(Value)) # passive <=> (X1 \= X2; Y1 \= Y2), same_block(X1, Y1, X2, Y2) |
    false.

fill, square(_,_,domain([])) ==> fail.
fill \ square(X,Y,domain([Value|Rest])) <=> length(Rest, 0) |
    square(X,Y,num(Value)).
fill, square(X,Y1,num(Value)) \ square(X,Y2,domain(Domain)) <=> Y1 \= Y2, select(Value, Domain, NewDomain) |
    square(X,Y2,domain(NewDomain)).
fill, square(X1,Y,num(Value)) \ square(X2,Y,domain(Domain)) <=> X1 \= X2, select(Value, Domain, NewDomain) |
    square(X2,Y,domain(NewDomain)).
fill, square(X1,Y1,num(Value)) \ square(X2,Y2,domain(Domain)) <=> select(Value, Domain, NewDomain), (X1 \= X2; Y1 \= Y2), same_block(X1, Y1, X2, Y2) |
    square(X2,Y2,domain(NewDomain)).

fill <=> solver(2).

solver(N), square(X,Y,domain(Domain)) # passive <=> length(Domain, Len), Len =:= N | member(Value, Domain), square(X,Y,num(Value)), fill.

solver(N) <=> N == 9 | true.
solver(N) <=> NN is N + 1, solver(NN).


same_block(X1, Y1, X2, Y2):-
    X1 // 3 =:= X2 // 3,
    Y1 // 3 =:= Y2 // 3.

print_solution :-
    print_solution(0).
print_solution(9) :- 
    write('\n\n').
print_solution(I) :-
    (0 =:= I mod 3,
        write('\n\n')
        ;
        true
    ),
    print_solution(I, 0),
    NewI is I + 1,
    print_solution(NewI).
print_solution(_, 9) :-
    write('\n').
print_solution(I, J) :-
    (0 =:= J mod 3,
        write(' ')
        ;
        true
    ),
    (find_chr_constraint(square(I,J, num(Value))),
        write(Value)
        ;
        write('X')
    ),
    NewJ is J + 1,
    print_solution(I, NewJ).



cleanup \ square(_, _, _) <=> true.
cleanup <=> true.
