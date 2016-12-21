-module(adventofcode10).
-export([responsible_for/3]).
-export([dispatch/5]).
-export([receiver_listener/5]).


responsible_for(Filename, First, Second) ->
  Dispatcher = spawn(?MODULE, dispatch, [self(), [],[], {First, Second}, 1]),
  {ok, Device} = file:open(Filename, [read]),
  parse_file(Device, Dispatcher).

parse_hit(Hit) ->
  ResultList = {element(1, Hit), element(2, element(2, Hit))},
  Result = case ResultList of
             {handling, Spec} ->
               {handling, {list_to_integer(lists:nth(1, Spec)),
                           list_to_atom(lists:nth(2, Spec)),
                           list_to_integer(lists:nth(3, Spec)),
                           list_to_atom(lists:nth(4, Spec)),
                           list_to_integer(lists:nth(5,Spec))}};
             {giving, Spec} ->
               {giving, {bot,
                         list_to_integer(lists:nth(1, Spec)),
                         list_to_integer(lists:nth(2, Spec))}}
           end,
  Result.

parse_next_line(Line) ->
  Commands = [{"value (?<value>\\d+) goes to bot (?<to_num>\\d+)",
               [value, to_num], giving},
              {"bot (?<from_num>\\d+) gives low to (?<where1>\\w+) "
               "(?<to_num1>\\d+) and high to (?<where2>\\w+) (?<to_num2>\\d+)",
               [from_num, where1, to_num1, where2, to_num2], handling}],
  Results = lists:map(fun(Format) ->
                          {element(3, Format),
                           re:run(Line, element(1, Format),
                                  [{capture, element(2, Format), list}])}
                      end, Commands),
  Hit = lists:nth(1, lists:filter(fun(Match) -> case Match of
                                                  {_, nomatch} -> false;
                                                  _ -> true
                                                end
                                  end, Results)),
  parse_hit(Hit).

parse_file(Device, Dispatcher) ->
  case io:get_line(Device, "") of
    eof ->
      receive
        finished -> ok
      end;
    Line ->
      Dispatcher ! parse_next_line(Line),
      parse_file(Device, Dispatcher)
  end.

send_chips_if_possible(Dispatcher, Directives, Chips) ->
  case tuple_size(Directives) of
    0 ->
      Chips;
    _ ->
      case length(Chips) of
        2 ->
          Where1 = element(2, element(1, Directives)),
          Num1 = element(3, element(1, Directives)),
          Where2 = element(2, element(2, Directives)),
          Num2 = element(3, element(2, Directives)),
          Dispatcher ! {giving, {Where1, lists:min(Chips), Num1}},
          Dispatcher ! {giving, {Where2, lists:max(Chips), Num2}},
          [];
        _ ->
          Chips
      end
  end.

receiver_listener(Dispatcher, Type, Directives, Chips, FindComparing) ->
  {NewDirectives, NewChips} = receive
                                {directives, Where1, Num1, Where2, Num2} ->
                                  Dir = {{low, Where1, Num1}, {high, Where2, Num2}},
                                  ChipsNotSent = send_chips_if_possible(Dispatcher, Dir, Chips),
                                  {Dir, ChipsNotSent};
                                {chip, Chip} ->
                                  case Type of
                                    {output, Num} when Num >= 0, Num =< 2 ->
                                      Dispatcher ! {output, Chip};
                                    _ -> ok
                                  end,
                                  AllChips = Chips ++ [Chip],
                                  case lists:sort(AllChips) == FindComparing of
                                    true -> io:format("Comparing ~p done by ~p~n",
                                                      [AllChips, Type]);
                                    false -> ok
                                  end,
                                  ChipsNotSent = send_chips_if_possible(Dispatcher, Directives, AllChips),
                                  {Directives, ChipsNotSent}
                              end,
  receiver_listener(Dispatcher, Type, NewDirectives, NewChips, FindComparing).

find_or_add_receiver(Dispatcher, Type, Num, Receivers, {First, Second}) ->
  case lists:keyfind(Num, 1, Receivers) of
    {_Num, Tid} ->
      {Tid, Receivers};
    false ->
      Tid = spawn(?MODULE, receiver_listener, [Dispatcher, {Type, Num}, {}, [], lists:sort([First, Second])]),
      {Tid, [{Num, Tid}] ++ Receivers}
  end.

dispatch(Main, Bots, Outputs, FindComparing, OutputsMultiplied) ->
  receive
    {giving, Spec} ->
      case Spec of
        {bot, Chip, Where} ->
          {Tid, AllBots} = find_or_add_receiver(self(), bot, Where, Bots, FindComparing),
          Tid ! {chip, Chip},
          dispatch(Main, AllBots, Outputs, FindComparing, OutputsMultiplied);
        {output, Chip, Where} ->
          {Tid, AllOutputs} = find_or_add_receiver(self(), output,
                                                   Where, Outputs, FindComparing),
          Tid ! {chip, Chip},
          dispatch(Main, Bots, AllOutputs, FindComparing, OutputsMultiplied)
      end;
    {handling, Spec} ->
      {Tid, AllBots} = find_or_add_receiver(self(), bot, element(1, Spec),
                                            Bots, FindComparing),
      Tid ! erlang:insert_element(1, erlang:delete_element(1, Spec), directives),
      dispatch(Main, AllBots, Outputs, FindComparing, OutputsMultiplied);
    {output, Value} ->
      dispatch(Main, Bots, Outputs, FindComparing, OutputsMultiplied*Value)
  after 30 ->
          io:format("Outputs 0, 1, 2 multiplied: ~p~n", [OutputsMultiplied]),
          Main ! finished

  end.
