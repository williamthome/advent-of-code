-module(day1_part2).

-export([ file/1, reduce/1 ]).

-record(state, {current = [], sum = 0}).

-define(EXAMPLE_RESULT, 281).
-define(INPUT_RESULT, 55614).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day1), Filename])),
    reduce(Bin).

reduce(Bin) ->
    reduce(Bin, #state{}).

reduce(<<$\n, Rest/bitstring>>, State) ->
    Int = to_integer(State#state.current),
    reduce(Rest, State#state{current = [], sum = State#state.sum + Int});
reduce(<<"one", Rest/bitstring>>, State) ->
    reduce(<<$e, Rest/bitstring>>, State#state{current = [$1 | State#state.current]});
reduce(<<"two", Rest/bitstring>>, State) ->
    reduce(<<$o, Rest/bitstring>>, State#state{current = [$2 | State#state.current]});
reduce(<<"three", Rest/bitstring>>, State) ->
    reduce(<<$e, Rest/bitstring>>, State#state{current = [$3 | State#state.current]});
reduce(<<"four", Rest/bitstring>>, State) ->
    reduce(<<$r, Rest/bitstring>>, State#state{current = [$4 | State#state.current]});
reduce(<<"five", Rest/bitstring>>, State) ->
    reduce(<<$e, Rest/bitstring>>, State#state{current = [$5 | State#state.current]});
reduce(<<"six", Rest/bitstring>>, State) ->
    reduce(Rest, State#state{current = [$6 | State#state.current]});
reduce(<<"seven", Rest/bitstring>>, State) ->
    reduce(<<$n, Rest/bitstring>>, State#state{current = [$7 | State#state.current]});
reduce(<<"eight", Rest/bitstring>>, State) ->
    reduce(<<$t, Rest/bitstring>>, State#state{current = [$8 | State#state.current]});
reduce(<<"nine", Rest/bitstring>>, State) ->
    reduce(<<$e, Rest/bitstring>>, State#state{current = [$9 | State#state.current]});
reduce(<<H, Rest/bitstring>>, State) when H >= $1, H =< $9 ->
    reduce(Rest, State#state{current = [H | State#state.current]});
reduce(<<_, Rest/bitstring>>, State) ->
    reduce(Rest, State);
reduce(<<>>, State) ->
    State#state.sum + to_integer(State#state.current).

to_integer([H]) ->
    list_to_integer([H, H]);
to_integer([Last | Rest]) ->
    [First | _] = lists:reverse(Rest),
    list_to_integer([First, Last]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(?EXAMPLE_RESULT, file("part2_example")).

input_test() ->
    ?assertEqual(?INPUT_RESULT, file("input")).

-endif.
