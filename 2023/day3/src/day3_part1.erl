-module(day3_part1).

-export([ file/1, reduce/1 ]).

-record(line, {pos, numbers, symbols}).
-record(state, {line_count = 0, numbers = [], symbols = [], lines = #{}}).

-define(EXAMPLE_RESULT, 4361).
-define(INPUT_RESULT, 531561).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day3), Filename])),
    reduce(Bin).

reduce(Bin) ->
    reduce(Bin, Bin, 0, 0, #state{}).

reduce(<<$\n, Rest/bitstring>>, Input, GPos, LPos, State) ->
    reduce(Rest, Input, GPos+LPos+1, 0, new_line(GPos, State));
reduce(<<$., Rest/bitstring>>, Input, GPos, LPos, State) ->
    reduce(Rest, Input, GPos, LPos+1, State);
reduce(<<H, Rest0/bitstring>>, Input, GPos, LPos, State) when H >= $0, H =< $9 ->
    collect_number(Rest0, Input, GPos, LPos, 1, State);
reduce(<<_, Rest/bitstring>>, Input, GPos, LPos, State) ->
    reduce(Rest, Input, GPos, LPos+1, push_symbol(LPos, State));
reduce(<<>>, Input, GPos, _LPos, State) ->
    State1 = new_line(GPos, State),
    Lines = State1#state.lines,
    maps:fold(fun(LineNumber, Line, Acc) ->
        LSymbols = Line#line.symbols,
        ASymbols = get_symbols(LineNumber, Lines),
        LPos = Line#line.pos,
        lists:foldl(fun(Number, Acc1) ->
            case any_intersect_line(Number, LSymbols) orelse
                 any_intersect_adjacent(Number, ASymbols)
            of
                true ->
                    Acc1 + to_integer(Number, LPos, Input);
                false ->
                    Acc1
            end
        end, Acc, Line#line.numbers)
    end, 0, Lines).

new_line(GPos, State) ->
    Line = #line{
        pos = GPos,
        numbers = State#state.numbers,
        symbols = State#state.symbols
    },
    State#state{
        line_count = State#state.line_count+1,
        numbers = [],
        symbols = [],
        lines = maps:put(State#state.line_count, Line, State#state.lines)
    }.

collect_number(<<H, Rest/bitstring>>, Input, GPos, LPos, Len, State) when H >= $0, H =< $9 ->
    collect_number(Rest, Input, GPos, LPos, Len+1, State);
collect_number(<<Rest/bitstring>>, Input, GPos, LPos, Len, State) ->
    reduce(Rest, Input, GPos, LPos+Len, push_number(LPos, Len, State)).

push_number(Pos, Len, State) ->
    State#state{numbers = [{Pos, Len} | State#state.numbers]}.

push_symbol(Pos, State) ->
    State#state{symbols = [Pos | State#state.symbols]}.

get_symbols(0, Lines) ->
    Below = maps:get(1, Lines),
    Below#line.symbols;
get_symbols(N, Lines) when N =:= map_size(Lines)-1 ->
    Above = maps:get(N-1, Lines),
    Above#line.symbols;
get_symbols(N, Lines) ->
    Above = maps:get(N-1, Lines),
    Below = maps:get(N+1, Lines),
    lists:usort(lists:merge(Above#line.symbols, Below#line.symbols)).

any_intersect_line(Number, Symbols) ->
    lists:any(fun(Symbol) -> intersect_line(Number, Symbol) end, Symbols).

any_intersect_adjacent(Number, Symbols) ->
    lists:any(fun(Symbol) -> intersect_adjacent(Number, Symbol) end, Symbols).

intersect_line({NPos, NLen}, SPos) ->
    SPos =:= NPos-1 orelse SPos =:= NPos+NLen.

intersect_adjacent({NPos, NLen}, SPos) ->
    SPos >= NPos-1 andalso SPos =< NPos+NLen.

to_integer({NPos, NLen}, LPos, Input) ->
    binary_to_integer(binary:part(Input, NPos + LPos, NLen)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

any_intersect_line_test() ->
    ?assert(any_intersect_line({1,1}, [0])),
    ?assert(any_intersect_line({1,1}, [2])).

any_intersect_adjacent_test() ->
    ?assert(any_intersect_adjacent({1,1}, [0])),
    ?assert(any_intersect_adjacent({1,1}, [1])),
    ?assert(any_intersect_adjacent({1,1}, [2])).

example_test() ->
    ?assertEqual(?EXAMPLE_RESULT, file("part1_example")).

input_test() ->
    ?assertEqual(?INPUT_RESULT, file("input")).

-endif.
