-module(day2_part1).

-export([ file/4, reduce/4 ]).

-record(state, {
    available_red,
    available_green,
    available_blue,
    red,
    green,
    blue,
    current_id,
    valid_ids
}).

-define(EXAMPLE_RESULT, 8).
-define(INPUT_RESULT, 2551).

file(Filename, Red, Green, Blue) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day2), Filename])),
    reduce(Bin, Red, Green, Blue).

reduce(Bin, Red, Green, Blue) ->
    reduce(Bin, reset_game(undefined, #state{
        available_red = Red,
        available_green = Green,
        available_blue = Blue,
        valid_ids = []
    })).

reduce(<<"Game ", Rest0/bitstring>>, State0) ->
    {Id, Rest1} = collect_id(Rest0),
    case collect_set(Rest1, reset_game(Id, State0)) of
        {ok, State, Rest} ->
            reduce(Rest, State#state{
                valid_ids = [Id | State#state.valid_ids]
            });
        {error, State, Rest} ->
            reduce(Rest, State)
    end;
reduce(<<>>, State) ->
    lists:sum(State#state.valid_ids).

reset_game(Id, State) ->
    State#state{
        current_id = Id
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
            case is_set_valid(State) of
                true ->
                    collect_set(Rest, State);
                false ->
                    {error, State, reach_end_of_game(Rest)}
            end;
        {end_of_game, State, Rest} ->
            case is_set_valid(State) of
                true ->
                    {ok, State, Rest};
                false ->
                    {error, State, Rest}
            end
    end.

do_collect_set(<<H, Rest/bitstring>>, State, Acc) when H >= $0, H =< $9 ->
    do_collect_set(Rest, State, <<Acc/binary, H>>);
do_collect_set(<<"red", Rest/bitstring>>, State, Acc) ->
    N = State#state.red + binary_to_integer(Acc),
    do_collect_set(Rest, State#state{red = N}, <<>>);
do_collect_set(<<"green", Rest/bitstring>>, State, Acc) ->
    N = State#state.green + binary_to_integer(Acc),
    do_collect_set(Rest, State#state{green = N}, <<>>);
do_collect_set(<<"blue", Rest/bitstring>>, State, Acc) ->
    N = State#state.blue + binary_to_integer(Acc),
    do_collect_set(Rest, State#state{blue = N}, <<>>);
do_collect_set(<<$;, Rest/bitstring>>, State, <<>>) ->
    {next, State, Rest};
do_collect_set(<<$\n, Rest/bitstring>>, State, <<>>) ->
    {end_of_game, State, Rest};
do_collect_set(<<_, Rest/bitstring>>, State, Acc) ->
    do_collect_set(Rest, State, Acc);
do_collect_set(<<>>, State, <<>>) ->
    {end_of_game, State, <<>>}.

reach_end_of_game(<<$\n, Rest/bitstring>>) ->
    Rest;
reach_end_of_game(<<_, Rest/bitstring>>) ->
    reach_end_of_game(Rest);
reach_end_of_game(<<>>) ->
    <<>>.

is_set_valid(State) ->
    State#state.red =< State#state.available_red andalso
    State#state.green =< State#state.available_green andalso
    State#state.blue =< State#state.available_blue.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

collect_id_test() ->
    ?assertEqual({1000, <<" foo">>}, collect_id(<<"1000: foo">>)).

collect_set_test() ->
    Input = <<" 7 blue, 9 red, 1 green; foo">>,
    Expect = {next, #state{
        red = 9,
        green = 1,
        blue = 7
    }, <<" foo">>},
    Result = do_collect_set(Input, reset_set(#state{}), <<>>),
    ?assertEqual(Expect, Result).

example_test() ->
    Red = 12, Green = 13, Blue = 14,
    ?assertEqual(?EXAMPLE_RESULT, file("part1_example", Red, Green, Blue)).

input_test() ->
    Red = 12, Green = 13, Blue = 14,
    ?assertEqual(?INPUT_RESULT, file("input", Red, Green, Blue)).

-endif.
