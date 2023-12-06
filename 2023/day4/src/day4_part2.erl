-module(day4_part2).

-export([ file/1, reduce/1 ]).

-record(card, {number, winning_numbers, hand_numbers, winning_hand_numbers}).
-record(state, {card_number, winning_numbers, hand_numbers, cards, winning_cards}).

-define(EXAMPLE_RESULT, 30).
-define(INPUT_RESULT, 6857330).

file(Filename) ->
    {ok, Bin} = file:read_file(filename:join([code:priv_dir(day4), Filename])),
    reduce(Bin).

reduce(Input) ->
    reduce(Input, #state{cards = #{}, winning_cards = #{}}).

reduce(<<"Card ", Rest/bitstring>>, State) ->
    collect_card_number(Rest, <<>>, reset(State)).

collect_card_number(<<$:, Rest/bitstring>>, Acc, State) ->
    collect_winning_numbers(Rest, <<>>, set_card_number(binary_to_integer(Acc), State));
collect_card_number(<<H, Rest/bitstring>>, Acc, State) when H >= $0, H =< $9 ->
    collect_card_number(Rest, <<Acc/binary, H>>, State);
collect_card_number(<<_, Rest/bitstring>>, Acc, State) ->
    collect_card_number(Rest, Acc, State).

set_card_number(CardNumber, State) ->
    State#state{card_number = CardNumber}.

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
    reduce(Rest, push_card(State));
collect_hand_numbers(<<H, Rest/bitstring>>, Acc, State) when H >= $0, H =< $9 ->
    collect_hand_numbers(Rest, <<Acc/binary, H>>, State);
collect_hand_numbers(<<_, Rest/bitstring>>, <<>>, State) ->
    collect_hand_numbers(Rest, <<>>, State);
collect_hand_numbers(<<>>, <<>>, State) ->
    eval(push_card(State));
collect_hand_numbers(Rest, Acc, State) ->
    collect_hand_numbers(Rest, <<>>, push_hand_number(binary_to_integer(Acc), State)).

push_hand_number(Number, State) ->
    State#state{hand_numbers = [Number | State#state.hand_numbers]}.

push_card(State) ->
    Card = #card{
        number = State#state.card_number,
        winning_numbers = State#state.winning_numbers,
        hand_numbers = State#state.hand_numbers,
        winning_hand_numbers = winning_hand_numbers(State)
    },
    push_card(Card, State).

push_card(#card{number = CardNumber, winning_hand_numbers = []} = Card, State) ->
    Cards = State#state.cards,
    State#state{cards = Cards#{CardNumber => Card}};
push_card(#card{number = CardNumber} = Card, State) ->
    Cards = State#state.cards,
    WinningCards = State#state.winning_cards,
    State#state{
        cards = Cards#{CardNumber => Card},
        winning_cards = WinningCards#{CardNumber => Card}
    }.

eval(State) ->
    length(copy(State#state.cards)).

copy(Originals) ->
    Cards = maps:values(Originals),
    lists:flatten(do_copy(Cards, Originals, Cards)).

do_copy(Cards, Originals, Acc0) ->
    lists:foldl(fun
        (#card{winning_hand_numbers = []}, Acc) ->
            Acc;
        (#card{number = CardNumber} = Card, Acc) ->
            NextCardNumber = CardNumber+1,
            WinningCardsCount = length(Card#card.winning_hand_numbers),
            LastCardNumber = CardNumber+WinningCardsCount+1,
            case do_copy_1(NextCardNumber, LastCardNumber, Originals, []) of
                [] ->
                    Acc;
                Copies ->
                    do_copy(Copies, Originals, [Copies | Acc])
            end
    end, Acc0, Cards).

do_copy_1(LastCardNumber, LastCardNumber, _Cards, Copies) ->
    Copies;
do_copy_1(CardNumber, LastCardNumber, Cards, Copies) ->
    case Cards of
        #{CardNumber := Card} ->
            do_copy_1(CardNumber+1, LastCardNumber, Cards, [Card | Copies]);
        #{} ->
            Copies
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
