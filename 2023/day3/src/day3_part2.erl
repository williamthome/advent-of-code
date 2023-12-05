-module(day3_part2).

-export([ file/1, reduce/1 ]).

-record(line, {pos, numbers, gears}).
-record(state, {line_count = 0, numbers = [], gears = [], lines = #{}}).

-define(EXAMPLE_RESULT, 467835).
-define(INPUT_RESULT, 83279367).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day3), Filename])),
    reduce(Bin).

reduce(Bin) ->
    reduce(Bin, Bin, 0, 0, #state{}).

reduce(<<$\n, Rest/bitstring>>, Input, GPos, LPos, State) ->
    reduce(Rest, Input, GPos+LPos+1, 0, new_line(GPos, State));
reduce(<<H, Rest0/bitstring>>, Input, GPos, LPos, State) when H >= $0, H =< $9 ->
    collect_number(Rest0, Input, GPos, LPos, 1, State);
reduce(<<$*, Rest/bitstring>>, Input, GPos, LPos, State) ->
    reduce(Rest, Input, GPos, LPos+1, push_gear(LPos, State));
reduce(<<_, Rest/bitstring>>, Input, GPos, LPos, State) ->
    reduce(Rest, Input, GPos, LPos+1, State);
reduce(<<>>, Input, GPos, _LPos, State) ->
    State1 = new_line(GPos, State),
    Lines = State1#state.lines,
    maps:fold(fun(N, Line, Acc0) ->
        lists:foldl(fun(Gear, Acc) ->
            case get_adjacent_numbers(N, Gear, Lines) of
                [A, B] ->
                    Acc + ratio(A, B, Input);
                _ ->
                    Acc
            end
        end, Acc0, Line#line.gears)
    end, 0, Lines).

new_line(GPos, State) ->
    Line = #line{
        pos = GPos,
        numbers = State#state.numbers,
        gears = State#state.gears
    },
    State#state{
        line_count = State#state.line_count+1,
        numbers = [],
        gears = [],
        lines = maps:put(State#state.line_count, Line, State#state.lines)
    }.

collect_number(<<H, Rest/bitstring>>, Input, GPos, LPos, Len, State) when H >= $0, H =< $9 ->
    collect_number(Rest, Input, GPos, LPos, Len+1, State);
collect_number(<<Rest/bitstring>>, Input, GPos, LPos, Len, State) ->
    reduce(Rest, Input, GPos, LPos+Len, push_number(GPos, LPos, Len, State)).

push_number(LPos, Pos, Len, State) ->
    State#state{numbers = [{LPos, {Pos, Len}} | State#state.numbers]}.

push_gear(Pos, State) ->
    State#state{gears = [Pos | State#state.gears]}.

get_adjacent_numbers(0, Gear, Lines) ->
    Current = maps:get(0, Lines),
    Below = maps:get(1, Lines),
    NCurrent = get_current_numbers(Gear, Current#line.numbers),
    NBelow = get_numbers_above_or_below(Gear, Below#line.numbers),
    lists:merge(NCurrent, NBelow);
get_adjacent_numbers(N, Gear, Lines) when N =:= map_size(Lines)-1 ->
    Above = maps:get(N-1, Lines),
    Current = maps:get(N, Lines),
    NAbove = get_numbers_above_or_below(Gear, Above#line.numbers),
    NCurrent = get_current_numbers(Gear, Current#line.numbers),
    lists:merge(NAbove, NCurrent);
get_adjacent_numbers(N, Gear, Lines) ->
    Above = maps:get(N-1, Lines),
    Current = maps:get(N, Lines),
    Below = maps:get(N+1, Lines),
    NAbove = get_numbers_above_or_below(Gear, Above#line.numbers),
    NCurrent = get_current_numbers(Gear, Current#line.numbers),
    NBelow = get_numbers_above_or_below(Gear, Below#line.numbers),
    lists:merge3(NAbove, NCurrent, NBelow).

get_current_numbers(Gear, Numbers) ->
    lists:filter(fun(Number) -> intersect_current(Number, Gear) end, Numbers).

get_numbers_above_or_below(Gear, Numbers) ->
    lists:filter(fun(Number) -> intersect_above_or_below(Number, Gear) end, Numbers).

intersect_current({_LPos, {NPos, NLen}}, SPos) ->
    SPos =:= NPos-1 orelse SPos =:= NPos+NLen.

intersect_above_or_below({_LPos, {NPos, NLen}}, SPos) ->
    SPos >= NPos-1 andalso SPos =< NPos+NLen.

ratio({ALPos, A}, {BLPos, B}, Input) ->
    to_integer(A, ALPos, Input) * to_integer(B, BLPos, Input).

to_integer({NPos, NLen}, LPos, Input) ->
    binary_to_integer(binary:part(Input, NPos + LPos, NLen)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(?EXAMPLE_RESULT, file("part2_example")).

input_test() ->
    ?assertEqual(?INPUT_RESULT, file("input")).

-endif.
