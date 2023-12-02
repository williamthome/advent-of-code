-module(day1_part1).

-export([ file/1, reduce/1 ]).

-record(state, {current = [], sum = 0}).

-define(EXAMPLE_RESULT, 142).
-define(INPUT_RESULT, 55488).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day1), Filename])),
    reduce(Bin).

reduce(Bin) ->
    reduce(Bin, #state{}).

reduce(<<$\n, Rest/bitstring>>, State) ->
    Int = to_integer(State#state.current),
    reduce(Rest, State#state{current = [], sum = State#state.sum + Int});
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
    ?assertEqual(?EXAMPLE_RESULT, file("part1_example")).

input_test() ->
    ?assertEqual(?INPUT_RESULT, file("input")).

-endif.
