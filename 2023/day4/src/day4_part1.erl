-module(day4_part1).

-export([ file/1, reduce/1 ]).

-record(state, {winning_numbers, hand_numbers, points}).

-define(EXAMPLE_RESULT, 13).
-define(INPUT_RESULT, 27454).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day4), Filename])),
    reduce(Bin).

reduce(Input) ->
    reduce(Input, #state{points = 0}).

reduce(<<"Card ", Rest/bitstring>>, State) ->
    collect_card_number(Rest, reset(State)).

collect_card_number(<<$:, Rest/bitstring>>, State) ->
    collect_winning_numbers(Rest, <<>>, State);
collect_card_number(<<_, Rest/bitstring>>, State) ->
    collect_card_number(Rest, State).

collect_winning_numbers(<<$|, Rest/bitstring>>, <<>>, State) ->
    collect_hand_numbers(Rest, <<>>, State);
collect_winning_numbers(<<H, Rest/bitstring>>, Acc, State) when H >= $0, H =< $9 ->
    collect_winning_numbers(Rest, <<Acc/binary, H>>, State);
collect_winning_numbers(<<_, Rest/bitstring>>, <<>>, State) ->
    collect_winning_numbers(Rest, <<>>, State);
collect_winning_numbers(Rest, Acc, State) ->
    collect_winning_numbers(Rest, <<>>, push_winning_number(binary_to_integer(Acc), State)).

push_winning_number(Number, State) ->
    State#state{winning_numbers = [Number | State#state.winning_numbers]}.

collect_hand_numbers(<<$\n, Rest/bitstring>>, <<>>, State) ->
    reduce(Rest, sum_hand_points(State));
collect_hand_numbers(<<H, Rest/bitstring>>, Acc, State) when H >= $0, H =< $9 ->
    collect_hand_numbers(Rest, <<Acc/binary, H>>, State);
collect_hand_numbers(<<_, Rest/bitstring>>, <<>>, State) ->
    collect_hand_numbers(Rest, <<>>, State);
collect_hand_numbers(<<>>, <<>>, State) ->
    eval(sum_hand_points(State));
collect_hand_numbers(Rest, Acc, State) ->
    collect_hand_numbers(Rest, <<>>, push_hand_number(binary_to_integer(Acc), State)).

push_hand_number(Number, State) ->
    State#state{hand_numbers = [Number | State#state.hand_numbers]}.

eval(State) ->
    State#state.points.

sum_hand_points(State) ->
    State#state{points = State#state.points + hand_points(State)}.

hand_points(State) ->
    case winning_hand_numbers(State) of
        [] ->
            0;
        [_|Rest] ->
            lists:foldl(fun(_,N) -> N*2 end, 1, Rest)
    end.

winning_hand_numbers(State) ->
    WinningNumbers = State#state.winning_numbers,
    HandNumbers = State#state.hand_numbers,
    [X || X <- HandNumbers, lists:member(X, WinningNumbers)].

reset(State) ->
    State#state{
        winning_numbers = [],
        hand_numbers = []
    }.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(?EXAMPLE_RESULT, file("part1_example")).

input_test() ->
    ?assertEqual(?INPUT_RESULT, file("input")).

-endif.
