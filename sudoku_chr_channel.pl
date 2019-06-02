:- use_module(library(chr)).
:- chr_option(debug, off).
:- chr_constraint solve/1.
:- chr_constraint clean_store/0, solver/1, fill/0, b_square/3, square/3, remove_number/3,single_value/3.
:- consult('sudex_toledo.pl').

solve_puzzles :-
    solve_name(_),
    fail.

solve_name(Name) :-
    puzzles(P,Name),
    write(Name),nl,
    once(time(solve(P))).

solve(Sudoku) <=>
    make_squares(Sudoku, 0),
    make_b_squares(Sudoku, 0),
    fill,
    print_solution,
    clean_store.

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

make_b_squares([], _, _):- true.
make_b_squares([Value|Rest], I, J) :-
    NewJ is J+1,
    make_b_squares(Rest, I, NewJ),
    (number(Value),
        to_bool(Value,1,Bools),b_square(I,J,num(Bools))
        ; 
        b_square(I,J,domain([1,1,1,1,1,1,1,1,1]))
    ).
make_b_squares([],_):- !.
make_b_squares([Value|Rest], I):-
    NewI is I+1,
    make_b_squares(Value, I, 0),
    make_b_squares(Rest, NewI).

square(X,Y1,num(Value)), square(X,Y2,num(Value)) # passive <=> Y1 \= Y2 |
    false.
square(X1,Y,num(Value)), square(X2,Y,num(Value)) # passive <=> X1 \= X2 |
    false.
square(X1,Y1,num(Value)), square(X2,Y2,num(Value)) # passive <=> (X1 \= X2; Y1 \= Y2), same_block(X1, Y1, X2, Y2) |
    false.

b_square(X1,Y1,num(Bools)), b_square(X2,Y2,num(_)) # passive <=> (X1 \= X2; Y1 \= Y2), sum_list(Bools,0) |
    false.
b_square(X,Y1,num(Bools1)), b_square(X,Y2,num(Bools2)) # passive <=> Y1 \= Y2, \+one_zero_lists(Bools1,Bools2) |
    false.
b_square(X1,Y,num(Bools1)), b_square(X2,Y,num(Bools2)) # passive <=> X1 \= X2, \+one_zero_lists(Bools1,Bools2) |
    false.
b_square(X1,Y1,num(Bools1)), b_square(X2,Y2,num(Bools2)) # passive <=> (X1 \= X2; Y1 \= Y2), same_block(X1, Y1, X2, Y2), \+one_zero_lists(Bools1,Bools2) |
    false.


single_value(X, Y, num(Value)) \ square(X, Y, domain(_)) <=> square(X, Y, num(Value)).
single_value(X, Y, num(Value)) \ b_square(X, Y, domain(_)) <=> to_bool(Value, 1, Bools), b_square(X, Y, num(Bools)).
single_value(_,_,num(_)) <=> true.

remove_number(X, Y, num(Value)), fill \ square(X,Y,domain(Domain)) # passive <=> select(Value, Domain, NewDomain) |
    square(X, Y, domain(NewDomain)).
remove_number(X, Y, num(Value)), fill \ b_square(X,Y,domain(Possible)) # passive <=> nth1(Value, Possible, 1), to_bool(Value, 1, Bools), subtract_lists(Possible, Bools, NewPossible) |
    b_square(X, Y, domain(NewPossible)).
remove_number(_,_,num(_)) <=> true.

fill, square(_,_,domain([])) ==> fail.
fill, square(X,Y,domain([Value|Rest])) ==> length(Rest, 0) |
    single_value(X,Y,num(Value)).
fill, square(X,Y1,num(Value)), square(X,Y2,domain(Domain)) ==> Y1 \= Y2, member(Value, Domain) |
    remove_number(X,Y2,num(Value)).
fill, square(X1,Y,num(Value)), square(X2,Y,domain(Domain)) ==> X1 \= X2, member(Value, Domain) |
    remove_number(X2,Y,num(Value)).
fill, square(X1,Y1,num(Value)), square(X2,Y2,domain(Domain)) ==> (X1 \= X2; Y1 \= Y2), member(Value, Domain), same_block(X1, Y1, X2, Y2) |
    remove_number(X2,Y2,num(Value)).

fill \ b_square(_,_,domain([0,0,0,0,0,0,0,0,0])) <=> fail.
fill, b_square(X,Y,domain(Bools)) <=> sum_list(Bools,Amount), Amount =:= 1, nth1(Index, Bools, 1) |
    single_value(X,Y,num(Index)).
fill, b_square(X,Y1,num(Bools)), b_square(X,Y2,domain(Possible)) ==> Y1 \= Y2, nth1(Index, Bools, 1), nth1(Index, Possible, 1) |
    remove_number(X,Y2,num(Index)).
fill, b_square(X1,Y,num(Bools)), b_square(X2,Y,domain(Possible)) ==> X1 \= X2, nth1(Index, Bools, 1), nth1(Index, Possible, 1) |
    remove_number(X2,Y,num(Index)).
fill, b_square(X1,Y1,num(Bools)), b_square(X2,Y2,domain(Possible)) ==> nth1(Index, Bools, 1), (X1 \= X2; Y1 \= Y2), nth1(Index, Possible, 1), same_block(X1, Y1, X2, Y2) |
    remove_number(X2,Y2,num(Index)).
fill <=> solver(2).

solver(N), square(X,Y,domain(Domain)) # passive ==> length(Domain, Len), Len =:= N | member(Value, Domain), single_value(X,Y,num(Value)), fill.
solver(N), b_square(X,Y,domain(Possible)) # passive ==> sum_list(Possible, Len), Len =:= N | nth1(Index, Possible, 1), single_value(X,Y,num(Index)), fill.

solver(N) <=> N == 9 | true.
solver(N) <=> NewN is N + 1, solver(NewN).

same_block(X1, Y1, X2, Y2):-
    X1 // 3 =:= X2 // 3,
    Y1 // 3 =:= Y2 // 3.

subtract_lists([],[],[]).
subtract_lists([Value1|Rest1],[Value2|Rest2],[Value3|Rest3]) :-
    Value3 is Value1 - Value2,
    Value3 >= 0,
    Value3 < 2,
    subtract_lists(Rest1,Rest2,Rest3).

one_zero_lists([], []).
one_zero_lists([B_Value|B_Rest], [P_Value|P_Rest]) :-
    B_Value + P_Value < 2,
    one_zero_lists(B_Rest, P_Rest).

to_bool(_, 10, []).
to_bool(Number, I, [B_Val|B_Rest]) :-
    I < 10,
    NewI is I + 1,
    to_bool(Number, NewI, B_Rest),
    (I =:= Number,
        B_Val is 1
        ;
        B_Val is 0
    ).

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

clean_store \ square(_, _, _) <=> true.
clean_store \ b_square(_, _, _) <=> true.
clean_store <=> true.
