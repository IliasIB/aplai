% Partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  % uses the integer constraint library
:- lib(branch_and_bound).

% 1. bridges run horizontally or vertically
% 2. bridges run in one straight line
% 3. bridges cannot cross other bridges or islands
% 4. at most two bridges connect a pair of islands
% 5. sum constraint
% 6. connectedness


hashi(Name) :-
        ( integer(Name) ->
            puzzle_to_board(Name, Board)
        ;
            board(Name, Board)
        ),
        dim(Board, [Imax,Jmax]),
        dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
        dim(Flow, [Imax,Jmax,4]),
        first_node(Board, SinkX, SinkY),
        count_2d_non_zero(Board, IslandCount),
        ( foreachindex([I,J],Board), param(Board,NESW,Imax,Jmax,SinkX,SinkY,IslandCount,Flow) do
            Sum is Board[I,J],
            
            N is NESW[I,J,1],
            E is NESW[I,J,2],
            S is NESW[I,J,3],
            W is NESW[I,J,4],
            
            Nf is Flow[I,J,1],
            Ef is Flow[I,J,2],
            Sf is Flow[I,J,3],
            Wf is Flow[I,J,4],

            % Constraints 1 and 2:
            % The combination of N=S etc on tiles without island
            % and f.i. north on current tile must equal south on
            % other northern tile, express that bridges can only run
            % in a straight line, horizontally or vertically.
            % They also express another constraint that was not stated
            % explicitly: that bridges must be on both sides connected
            % to islands.
            ( I > 1    -> N #= NESW[I-1,J,3] ; N = 0, Nf = 0 ),
            ( I < Imax -> S #= NESW[I+1,J,1] ; S = 0, Sf = 0 ),
            ( J > 1    -> W #= NESW[I,J-1,2] ; W = 0, Wf = 0 ),
            ( J < Jmax -> E #= NESW[I,J+1,4] ; E = 0, Ef = 0 ),
            ( Sum > 0 ->
            % Is island
                % Constraint 4
                [N,E,S,W] #:: 0..2,
                [Nf,Ef,Sf,Wf] #:: -100000..100000,

                % Constraint 5
                N + E + S + W #= Sum,
              
                % Constraint 6.4 and 6.5
                ( SinkX =:= I, SinkY =:= J ->
                % Is sink island
                    Nf + Ef + Sf + Wf #= (IslandCount - 1) * -1,
                    
                    Nf #=< 0,
                    Ef #=< 0,
                    Sf #=< 0,
                    Wf #=< 0
                ;
                % Is non sink island
                    Nf + Ef + Sf + Wf #= 1
                )
            ;
            % Is not island
                
                N = S, E = W,
                
                % Constraint 3
                (N #= 0) or (E #= 0),
            
                
                % Is empty cell or bridge
                % Constraint 6.1 - not set
                (N + E + S + W #> 0) or (Nf #= 0 , Ef #= 0 , Sf #= 0 , Wf #= 0),
                not(N + E + S + W #= 0 and Nf #\= 0 and Ef #\= 0 and Sf #\= 0 and Wf #\= 0),
                %(N #> 0) or (Nf #= 0),
                %(E #> 0) or (Ef #= 0),
                %(S #> 0) or (Sf #= 0),
                %(W #> 0) or (Wf #= 0),
            
                % Constraint 6.2
                Nf + Sf #= 0,
                Ef + Wf #= 0,
                
                %Constraint 6.3
                ( I > 1    -> Nf + Flow[I-1,J,3] #= 0 ; true),
                ( I < Imax -> Sf + Flow[I+1,J,1] #= 0 ; true),
                ( J > 1    -> Wf + Flow[I,J-1,2] #= 0 ; true),
                ( J < Jmax -> Ef + Flow[I,J+1,4] #= 0 ; true)
                
            )
        ),

        % find a solution
        labeling(NESW),
        labeling(Flow),
        print_board(Board, NESW),
        nl.
        
hashi_alternative_solution(Name) :-
        ( integer(Name) ->
            puzzle_to_board(Name, Board)
        ;
            board(Name, Board)
        ),
        dim(Board, [Imax,Jmax]),
        dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
        ( foreachindex([I,J],Board), param(Board,NESW,Imax,Jmax) do
            Sum is Board[I,J],
            
            N is NESW[I,J,1],
            E is NESW[I,J,2],
            S is NESW[I,J,3],
            W is NESW[I,J,4],

            % Constraints 1 and 2:
            % The combination of N=S etc on tiles without island
            % and f.i. north on current tile must equal south on
            % other northern tile, express that bridges can only run
            % in a straight line, horizontally or vertically.
            % They also express another constraint that was not stated
            % explicitly: that bridges must be on both sides connected
            % to islands.
            ( I > 1    -> N #= NESW[I-1,J,3] ; N = 0 ),
            ( I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
            ( J > 1    -> W #= NESW[I,J-1,2] ; W = 0 ),
            ( J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
            ( Sum > 0 ->
            % Is island
                % Constraint 4
                [N,E,S,W] #:: 0..2,

                % Constraint 5
                N + E + S + W #= Sum
            ;
            % Is not island
                
                N = S, E = W,
                
                % Constraint 3
                (N #= 0) or (E #= 0)
            )
        ),
        minimize((labeling(NESW),avg(NESW,Avg)), Avg),
        print_board(Board, NESW),
        nl.


avg(List, Avg) :-
    flatten(List, FlatList),
    count_1d_non_zero(FlatList, Count),
    sum_1d_non_zero(FlatList, Sum),
    Sum100 #= Sum * 100,
    Avg #= Sum100 // Count.
    
count_2d_non_zero(List, Count) :-
    flatten(List, FlatList),
    count_1d_non_zero(FlatList, CountFlat),
    Count #= CountFlat.

count_1d_non_zero([], 0).
count_1d_non_zero([X|Xs], Count) :-
    (X #= 0 ->
        CountX = 0
    ;   
        CountX = 1
    ),
    count_1d_non_zero(Xs, CountXs),
    Count #= CountX + CountXs.

sum_1d_non_zero([], 0).
sum_1d_non_zero([X|Xs], Sum) :-
    sum_1d_non_zero(Xs, SumXs),
    Sum #= X + SumXs.

first_node(List, I, J) :-
    flatten(List, FlatList),
    dim(List, [Xmax, _]),
    first_node_flatlist(FlatList, Xmax, 1, 1, I, J).
    
first_node_flatlist([], _, _, _, -1, -1).
first_node_flatlist([H|T], Xmax, X, Y, I, J) :-
    (H > 0 ->
        I is Y,
        J is X
    ;
        (X =:= Xmax ->
            first_node_flatlist(T, Xmax, X, Y+1, I, J)
        ;
            first_node_flatlist(T, Xmax, X+1, Y, I, J)
        )
    ).

print_board(Board, NESW) :-
    ( foreachindex([I,J],Board), param(Board,NESW) do
        ( J > 1 -> true ; nl ),
        Sum is Board[I,J],
        ( Sum > 0 ->
            write(Sum)
        ;
            NS is NESW[I,J,1],
            EW is NESW[I,J,2],
            symbol(NS, EW, Char),
            write(Char)
        ),
        write(' ')
    ),
    nl.
        
puzzle_to_board(PuzzleId, Board) :-
    puzzle(PuzzleId, Dim, Nodes),
    BoardDim #= Dim * 2 - 1,
    dim(Board, [BoardDim,BoardDim]),
    ( foreachindex([I,J],Board), param(Board,Nodes) do
        It #= I + 1,
        Jt #= J + 1,
        Ip #= It // 2,
        Jp #= Jt // 2,
        ( memberchk((Ip,Jp,N), Nodes) , I #= _*2-1 , J #= _*2-1 -> 
            Board[I,J] #= N
        ;
            Board[I,J] #= 0
        )
    ).

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, 'X').


% Examples
board(test0,[]([](0))).

board(test1,[]([](1,0,0),
               [](0,0,0),
               [](1,0,0))).

board(test2,[]([](2,0,0),
               [](0,0,0),
               [](2,0,0))).
               
board(test3,[]([](1,0,0),
               [](0,0,0),
               [](2,0,1))).
               
board(test4,[]([](1,0,1),
               [](0,0,0),
               [](2,0,2))).
               
board(test5,[]([](2,0,2),
               [](0,0,0),
               [](2,0,2))).
               
board(test6,[]([](1,0,0,0,0),
               [](0,0,0,0,0),
               [](0,0,0,0,1),
               [](0,0,0,0,0),
               [](2,0,0,0,2))).
               
board(test7,[]([](1,0,0,0,0),
               [](0,0,0,0,0),
               [](0,0,1,0,0),
               [](0,0,0,0,0),
               [](2,0,3,0,1))).
               
board(test8,[]([](1,0,0,0,0),
               [](0,0,0,0,0),
               [](0,0,1,0,2),
               [](0,0,0,0,0),
               [](2,0,0,0,2))).
               
board(test9,[]([](1,0,0,0,0),
               [](0,2,0,0,2),
               [](0,0,0,0,0),
               [](0,2,0,1,0),
               [](2,0,2,0,2))).
               
%solution(test5,[]([]([](0, 1, 1, 0),
%               [](0, 1, 0, 1),
%               [](0, 0, 1, 1)),
%            []([](1, 0, 1, 0),
%               [](0, 0, 0, 0),
%               [](1, 0, 1, 0)),
%            []([](1, 1, 0, 0),
%               [](0, 1, 0, 1),
%               [](1, 0, 0, 1))
%           )).

board(stackoverflow,
     []([](4, 0, 6, 0, 0, 0, 6, 0, 3),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 1, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 1, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](1, 0, 3, 0, 0, 2, 0, 0, 0),
        [](0, 3, 0, 0, 0, 0, 4, 0, 1))
    ).
board(wikipedia,
     []([](2, 0, 4, 0, 3, 0, 1, 0, 2, 0, 0, 1, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1),
        [](0, 0, 0, 0, 2, 0, 3, 0, 2, 0, 0, 0, 0),
        [](2, 0, 3, 0, 0, 2, 0, 0, 0, 3, 0, 1, 0),
        [](0, 0, 0, 0, 2, 0, 5, 0, 3, 0, 4, 0, 0),
        [](1, 0, 5, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0),
        [](0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 2),
        [](0, 0, 4, 0, 4, 0, 0, 3, 0, 0, 0, 3, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](2, 0, 2, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3),
        [](0, 0, 0, 0, 0, 2, 0, 4, 0, 4, 0, 3, 0),
        [](0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 3, 0, 1, 0, 2, 0, 0, 2))
    ).
    
puzzle(1,7,P) :-
    P=[ (1,1,2),(1,2,3),(1,4,4),(1,6,2),
        (2,7,2),
        (3,1,1),(3,2,1),(3,5,1),(3,6,3),(3,7,3),
        (4,1,2),(4,4,8),(4,6,5),(4,7,2),
        (5,1,3),(5,3,3),(5,7,1),
        (6,3,2),(6,6,3),(6,7,4),
        (7,1,3),(7,4,3),(7,5,1),(7,7,2)].

puzzle(0, 2, P) :-
    P=[ (1,1,1),(2,1,2),(2,2,1)].

puzzle(2,13,P):-
    P=[ (1,1,2),(1,3,4),(1,5,3),(1,7,1),(1,9,2),(1,12,1),
        (2,10,3),(2,13,1),
        (3,5,2),(3,7,3),(3,9,2),
        (4,1,2),(4,3,3),(4,6,2),(4,10,3),(4,12,1),
        (5,5,2),(5,7,5),(5,9,3),(5,11,4),
        (6,1,1),(6,3,5),(6,6,2),(6,8,1),(6,12,2),
        (7,7,2),(7,9,2),(7,11,4),(7,13,2),
        (8,3,4),(8,5,4),(8,8,3),(8,12,3),
        (10,1,2),(10,3,2),(10,5,3),(10,9,3),(10,11,2),(10,13,3),
        (11,6,2),(11,8,4),(11,10,4),(11,12,3),
        (12,3,1),(12,5,2),
        (13,1,3),(13,6,3),(13,8,1),(13,10,2),(13,13,2)].

puzzle(3,6,P) :- 
    P = [ (1,1,1),(1,3,4),(1,5,2),
        (2,4,2),(2,6,3),
        (3,1,4),(3,3,7),(3,5,1),
        (4,4,2),(4,6,5),
        (5,3,3),(5,5,1),
        (6,1,3),(6,4,3),(6,6,3)].

puzzle(4,8,P) :- 
      P = [ (1,1,2),(1,3,2),(1,5,5),(1,7,2),
            (2,6,1),(2,8,3),
            (3,1,6),(3,3,3),
            (4,2,2),(4,5,6),(4,7,1),
            (5,1,3),(5,3,1),(5,6,2),(5,8,6),
            (6,2,2),
            (7,1,1),(7,3,3),(7,5,5),(7,8,3),
            (8,2,2),(8,4,3),(8,7,2)].

puzzle(5,10,P) :-
    P = [(1,2,4),(1,4,5),(1,6,3),(1,8,3),(1,10,2),(2,1,1),(3,5,2),(3,7,4),(3,10,3),(4,2,3),(4,4,2),(4,6,1),(4,9,1),(5,1,3),(5,5,1),(5,7,3),(5,10,2),(6,2,4),(6,4,4),(6,6,3),(6,9,2),(7,1,3),(7,10,2),(8,2,3),(8,5,3),(8,7,4),(8,9,2),(9,8,2),(9,10,3),(10,1,3),(10,3,3),(10,5,5),(10,7,4),(10,9,1)].

puzzle(6,20,P) :-
    P = [(1,2,3),(1,4,3),(1,6,4),(1,8,5),(1,11,4),(1,14,4),(1,17,3),(1,20,3),(2,1,2),(2,3,2),(2,5,5),(2,7,2),(3,2,4),(3,4,1),(3,8,3),(3,10,3),(3,12,3),(3,14,5),(3,16,3),(3,19,2),(4,1,4),(4,3,3),(4,6,3),(4,9,2),(4,11,3),(4,13,3),(4,15,2),(4,17,1),(4,20,3),(5,2,4),(5,4,3),(5,7,1),(5,10,5),(5,12,4),(5,14,3),(5,16,4),(5,18,4),(6,3,3),(6,5,3),(6,13,2),(6,15,5),(6,17,3),(6,19,2),(7,1,4),(7,4,4),(7,6,5),(7,9,2),(7,12,4),(7,14,2),(7,16,2),(7,18,4),(7,20,3),(8,2,4),(8,5,1),(8,10,2),(8,13,1),(8,15,3),(8,17,2),(8,19,2),(9,1,4),(9,4,5),(9,6,5),(9,9,6),(9,12,6),(9,14,2),(9,16,4),(10,2,4),(10,5,4),(10,8,2),(10,13,2),(10,15,3),(10,18,3),(10,20,2),(11,1,3),(11,4,4),(11,7,1),(11,9,5),(11,11,2),(11,16,3),(11,19,3),(12,3,3),(12,5,5),(12,8,3),(12,10,3),(12,12,6),(12,15,3),(12,18,3),(13,1,2),(13,4,4),(13,6,2),(13,13,2),(13,16,2),(13,20,3),(14,2,3),(14,5,1),(14,7,3),(14,9,4),(14,11,1),(14,15,4),(14,18,4),(15,1,2),(15,4,3),(15,6,3),(15,13,1),(15,17,1),(15,19,3),(16,3,4),(16,5,4),(16,7,6),(16,9,6),(16,12,7),(16,15,6),(16,18,2),(16,20,4),(17,2,3),(17,4,4),(17,10,2),(17,13,1),(17,16,3),(17,19,2),(18,1,4),(18,3,4),(18,6,2),(18,8,3),(18,11,2),(18,18,2),(18,20,4),(19,13,3),(19,15,4),(19,19,1),(20,1,4),(20,3,5),(20,5,2),(20,8,3),(20,10,3),(20,12,4),(20,14,3),(20,16,5),(20,18,6),(20,20,4)]
    