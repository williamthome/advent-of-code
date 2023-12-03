-module(day2_part2).

-export([ file/1, reduce/1 ]).

-record(state, {
    red,
    green,
    blue,
    min_red,
    min_green,
    min_blue,
    current_id,
    valid_ids,
    power
}).

-define(EXAMPLE_RESULT, 2286).
-define(INPUT_RESULT, 62811).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day2), Filename])),
    reduce(Bin).

reduce(Bin) ->
    reduce(Bin, reset_game(undefined, #state{
        valid_ids = [],
        power = 0
    })).

reduce(<<"Game ", Rest0/bitstring>>, State0) ->
    {Id, Rest1} = collect_id(Rest0),
    {State, Rest} = collect_set(Rest1, reset_game(Id, State0)),
    reduce(Rest, State);
reduce(<<>>, State) ->
    State#state.power.

reset_game(Id, State) ->
    State#state{
        current_id = Id,
        min_red = 0,
        min_green = 0,
        min_blue = 0
    }.

reset_set(State) ->
    State#state{
        red = 0,
        green = 0,
        blue = 0
    }.

collect_id(<<H, Rest/bitstring>>) when H >= $1, H =< $9 ->
    do_collect_id(Rest, <<H>>).

do_collect_id(<<$:, Rest/bitstring>>, Acc) ->
    {binary_to_integer(Acc), Rest};
do_collect_id(<<H, Rest/bitstring>>, Acc) when H >= $0, H =< $9 ->
    do_collect_id(Rest, <<Acc/binary, H>>).

collect_set(Bin, State0) ->
    case do_collect_set(Bin, reset_set(State0), <<>>) of
        {next, State, Rest} ->
            collect_set(Rest, State);
        {end_of_game, State, Rest} ->
            {update_power(State), Rest}
    end.

do_collect_set(<<H, Rest/bitstring>>, State, Acc) when H >= $0, H =< $9 ->
    do_collect_set(Rest, State, <<Acc/binary, H>>);
do_collect_set(<<"red", Rest/bitstring>>, State, Acc) ->
    N = State#state.red + binary_to_integer(Acc),
    do_collect_set(Rest, update_red(N, State), <<>>);
do_collect_set(<<"green", Rest/bitstring>>, State, Acc) ->
    N = State#state.green + binary_to_integer(Acc),
    do_collect_set(Rest, update_green(N, State), <<>>);
do_collect_set(<<"blue", Rest/bitstring>>, State, Acc) ->
    N = State#state.blue + binary_to_integer(Acc),
    do_collect_set(Rest, update_blue(N, State), <<>>);
do_collect_set(<<$;, Rest/bitstring>>, State, <<>>) ->
    {next, State, Rest};
do_collect_set(<<$\n, Rest/bitstring>>, State, <<>>) ->
    {end_of_game, State, Rest};
do_collect_set(<<_, Rest/bitstring>>, State, Acc) ->
    do_collect_set(Rest, State, Acc);
do_collect_set(<<>>, State, <<>>) ->
    {end_of_game, State, <<>>}.

update_red(N, State) ->
    case N > State#state.min_red of
        true ->
            State#state{red = N, min_red = N};
        false ->
            State#state{red = N}
    end.

update_green(N, State) ->
    case N > State#state.min_green of
        true ->
            State#state{green = N, min_green = N};
        false ->
            State#state{green = N}
    end.

update_blue(N, State) ->
    case N > State#state.min_blue of
        true ->
            State#state{blue = N, min_blue = N};
        false ->
            State#state{blue = N}
    end.

update_power(State) ->
    SetPower = State#state.min_red * State#state.min_green * State#state.min_blue,
    State#state{power = State#state.power + SetPower}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

collect_id_test() ->
    ?assertEqual({1000, <<" foo">>}, collect_id(<<"1000: foo">>)).

example_test() ->
    ?assertEqual(?EXAMPLE_RESULT, file("part2_example")).

input_test() ->
    ?assertEqual(?INPUT_RESULT, file("input")).

-endif.
