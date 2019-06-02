:- use_module(library(chr)).
:- chr_option(debug, off).
:- chr_constraint solve/1.
:- chr_constraint cleanup/0, solver/1, fill/0, square/3.
:- consult('sudex_toledo.pl').

solve_puzzles :-
    solve_name(_),
    fail.

solve_name(Name) :-
    puzzles(P,Name),
    write(Name),nl,
    time(solve(P)).

solve(Sudoku) <=>
    make_squares(Sudoku, 0),
    fill,
    print_solution,
    cleanup.

make_squares([], _, _):- true.
make_squares([Value|Rest], I, J) :-
    NewJ is J+1,
    make_squares(Rest, I, NewJ),
    (number(Value),
        to_bool(Value,1,Bools),square(I,J,num(Bools))
        ; 
        square(I,J,domain([1,1,1,1,1,1,1,1,1]))
    ).
make_squares([],_):- !.
make_squares([Value|Rest], I):-
    NewI is I+1,
    make_squares(Value, I, 0),
    make_squares(Rest, NewI).

square(X1,Y1,num(Bools1)), square(X2,Y2,num(_)) # passive <=> (X1 \= X2; Y1 \= Y2), sum_list(Bools1,0) |
    false.
square(X,Y1,num(Bools1)), square(X,Y2,num(Bools2)) # passive <=> Y1 \= Y2, \+one_zero_lists(Bools1,Bools2) |
    false.
square(X1,Y,num(Bools1)), square(X2,Y,num(Bools2)) # passive <=> X1 \= X2, \+one_zero_lists(Bools1,Bools2) |
    false.
square(X1,Y1,num(Bools1)), square(X2,Y2,num(Bools2)) # passive <=> (X1 \= X2; Y1 \= Y2), same_block(X1, Y1, X2, Y2), \+one_zero_lists(Bools1,Bools2) |
    false.

fill \ square(_,_,domain([0,0,0,0,0,0,0,0,0])) <=> fail.
fill \ square(X,Y,domain(Bools)) <=> sum_list(Bools,Amount), Amount =:= 1 |
    square(X,Y,num(Bools)).
fill, square(X,Y1,num(Bools)) \ square(X,Y2,domain(Possible)) <=> Y1 \= Y2, subtract_lists(Possible, Bools, NewPossible) |
    square(X,Y2,domain(NewPossible)).
fill, square(X1,Y,num(Bools)) \ square(X2,Y,domain(Possible)) <=> X1 \= X2, subtract_lists(Possible, Bools, NewPossible) |
    square(X2,Y,domain(NewPossible)).
fill, square(X1,Y1,num(Bools)) \ square(X2,Y2,domain(Possible)) <=> subtract_lists(Possible, Bools, NewPossible), (X1 \= X2; Y1 \= Y2), same_block(X1, Y1, X2, Y2) |
    square(X2,Y2,domain(NewPossible)).

fill <=> solver(2).

solver(N), square(X,Y,domain(Possible)) # passive <=> sum_list(Possible, Len), Len =:= N | nth1(Index, Possible, 1), to_bool(Index,1,Bools), square(X,Y,num(Bools)), fill.

solver(N) <=> N == 9 | true.
solver(N) <=> NewN is N + 1, solver(NewN).

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
    (find_chr_constraint(square(I,J, num(Bools))),
        nth1(Index, Bools, 1),
        write(Index)
        ;
        write('X')
    ),
    NewJ is J + 1,
    print_solution(I, NewJ).

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

cleanup \ square(_, _, _) <=> true.
cleanup <=> true.

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
