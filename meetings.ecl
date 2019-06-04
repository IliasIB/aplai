:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_edge_finder).
:- compile("benchmarksMeeting").
%% :- import alldifferent/1 from ic_global.

meeting(NbofPersons, Durations, OnWeekend, Rank, Precs, StartingDay, Start, EndTime , Viol) :-
    dim(Start, [NbofPersons]),
    dim(Precs, [NbPrecs, 2]),
    Start[1..NbofPersons] #:: 0..100,
    % Precedence rules
    ( for(I,1,NbPrecs), param(Start, Precs) do
        Start[Precs[I, 1]] #< Start[Precs[I, 2]]
    ),
    %% % No overlap
    %% ( for(I,1,NbofPersons), param(Start, NbofPersons, Durations) do 
    %%     ( for(J,1,NbofPersons), param(Start, Durations, I) do
    %%         ( I \= J -> 
    %%             ( for(K,0,Durations[I] - 1), param(Start, I, J) do 
    %%                 Start[J] #\= Start[I] + K
    %%             )
    %%             ;
    %%             true
    %%         )
    %%     )
    %% ),
    disjunctive(Start, Durations),
    % Weekend rules
    ( for(I,1,NbofPersons), param(OnWeekend, StartingDay, Start, Durations) do
        ( OnWeekend[I] #= 0 ->
            ( for(J,0,Durations[I] - 1), param(StartingDay, Start, I) do
                X #:: 0..inf,
                Y #:: 0..inf,
                (StartingDay + Start[I] + J) #\= 5 + (7 * X),
                (StartingDay + Start[I] + J) #\= 6 + (7 * Y)
            )
            ;
            true
        )
    ),
    maxlist(Start, MaxValue),
    violations(Start, Rank, Viol),
    Cost #= (2 * MaxValue) + Viol,
    minimize(labeling(Start), Cost),
    element(MaxIndex, Start, MaxValue),
    EndTime #= MaxValue + Durations[MaxIndex].

violations(Start, Rank, Viol) :-
    dim(Start, [NbofPersons]),
    dim(Violations, [NbofPersons,NbofPersons]),
    ( for(I,1,NbofPersons), param(Start, NbofPersons, Rank,Violations) do 
        ( for(J,1,NbofPersons), param(Start, Rank, I,Violations) do 
            ( J > I ->
                Violations[I,J] #= (Rank[I] #< Rank[J] and Start[I] #> Start[J])
                ;
                Violations[I,J] #= 0
            )
        )
    ),
    flatten(Violations,FlatList),
    sumlist(FlatList,Viol).
