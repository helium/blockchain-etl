-module(be_utils).

-export([pmap/2, pmap/3]).
-export([make_values_list/2]).
-export([split_list/2]).

split_list(List, N) ->
    RevList = do_split_list(List, N),
    lists:map(
        fun lists:reverse/1,
        lists:reverse(RevList)
    ).

do_split_list(List, Max) ->
    element(
        1,
        lists:foldl(
            fun
                (E, {[Buff | Acc], C}) when C < Max ->
                    {[[E | Buff] | Acc], C + 1};
                (E, {[Buff | Acc], _}) ->
                    {[[E], Buff | Acc], 1};
                (E, {[], _}) ->
                    {[[E]], 1}
            end,
            {[], 0},
            List
        )
    ).

make_values_list(NumberElements, NumberRows) ->
    make_values_list(NumberElements, NumberRows, 1).

make_values_list(_, 0, _) ->
    [];
make_values_list(NumberElements, NumberRows, Offset) ->
    [
        $(,
        [[$$, integer_to_list(E), $,, $\s] || E <- lists:seq(Offset, Offset + NumberElements - 2)],
        [$$, integer_to_list(Offset + NumberElements - 1), $), [$, || NumberRows /= 1], $\s]
        | make_values_list(NumberElements, NumberRows - 1, Offset + NumberElements)
    ].

pmap(F, L) ->
    Width = cpus(),
    pmap(F, L, Width).

pmap(F, L, Width) ->
    Parent = self(),
    Len = length(L),
    Min = floor(Len / Width),
    Rem = Len rem Width,
    Lengths = lists:duplicate(Rem, Min + 1) ++ lists:duplicate(Width - Rem, Min),
    OL = partition_list(L, Lengths, []),
    {St, WorkerPids} = lists:foldl(
        fun
            ([], Acc) ->
                Acc;
            (IL, {N, Workers}) ->
                P = spawn_opt(
                    fun() ->
                        try lists:map(F, IL) of
                            Res ->
                                Parent ! {pmap, N, Res}
                        catch
                            What:Why ->
                                Parent ! {pmap_crash, What, Why}
                        end
                    end,
                    [{fullsweep_after, 0}]
                ),
                {N + 1, [P | Workers]}
        end,
        {0, []},
        OL
    ),
    L2 = [
        receive
            {pmap_crash, What, Why} ->
                %% kill all the others
                [catch erlang:exit(P, normal) || P <- WorkerPids],
                flush_pmap_messages(),
                erlang:What(Why);
            {pmap, N, R} ->
                {N, R}
        end
     || _ <- lists:seq(1, St)
    ],
    {_, L3} = lists:unzip(lists:keysort(1, L2)),
    lists:flatten(L3).

flush_pmap_messages() ->
    receive
        {pmap, _N, _R} ->
            flush_pmap_messages()
    after 0 ->
        ok
    end.

partition_list([], [], Acc) ->
    lists:reverse(Acc);
partition_list(L, [0 | T], Acc) ->
    partition_list(L, T, Acc);
partition_list(L, [H | T], Acc) ->
    {Take, Rest} = lists:split(H, L),
    partition_list(Rest, T, [Take | Acc]).

cpus() ->
    Ct = erlang:system_info(schedulers_online),
    max(2, ceil(Ct / 2) + 1).
