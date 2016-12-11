-module(adv5).
-export([bruteforce/1]).
-export([bruteforce_password/4]).
-export([gather_results/2]).


bruteforce(Id) ->
  ResultProc = spawn(?MODULE, gather_results, [[], []]),
  Num = 100000,
  start_bruteforcing(Id, 1, ResultProc, Num, 0).

start_bruteforcing(Id, Index, ResultProc, Num, Started) when Started < Num ->
  spawn(?MODULE, bruteforce_password, [Id, Index, ResultProc, Num]),
  start_bruteforcing(Id, Index + 1, ResultProc, Num, Started + 1);
start_bruteforcing(_Id, _Index, _ResultProc, _Num, _Started) ->
  ok.

check_hash(<<Zeroes:24, _/binary>>) ->
  Zeroes < 2#10000.

bruteforce_password(Id, Index, ResultProc, Num) when Index rem Num == 0 ->
  ResultProc ! {more_processes, Id, Index + 1, Num};
bruteforce_password(Id, Index, ResultProc, _Num) ->
  Input = string:concat(Id, integer_to_list(Index)),
  Hash = erlang:md5(Input),
  case check_hash(Hash) of
    true -> ResultProc ! {match, Id, Index, Hash};
    false -> ok
  end.

add_new_hash_if_position_not_taken([First|Rest], Pos, Hash) ->
  case lists:nth(6, First) of
    TakenPos when TakenPos == Pos ->
      [First] ++ Rest;
    _TakenPos ->
      [First] ++ add_new_hash_if_position_not_taken(Rest, Pos, Hash)
  end;
add_new_hash_if_position_not_taken([], _Pos, Hash) ->
  [Hash].

check_part2_result(Result, Hash) ->
  case lists:nth(6, Hash) of
    Pos when Pos > 47, Pos < 56 ->
      add_new_hash_if_position_not_taken(Result, Pos, Hash);
    _Pos ->
      Result
  end.

gather_results(ResultsPart1, ResultsPart2) when length(ResultsPart2) < 8 ->
  {NewResults1, NewResults2} = receive
              {more_processes, Id, Index, Num} ->
                start_bruteforcing(Id, Index, self(), Num, 0),
                {ResultsPart1, ResultsPart2};
              {match, Id, Index, Hash} ->
                HashStr = lists:flatten([io_lib:format("~2.16.0b", [B]) ||
                                         <<B>> <= Hash]),
                          [Id ++ integer_to_list(Index), HashStr]),
                New1 = case ResultsPart1 of
                  Res when length(ResultsPart1) < 8 ->
                           Res ++ [HashStr];
                  _ -> ResultsPart1
                end,
                New2 = check_part2_result(ResultsPart2, HashStr),
                {New1, New2}
            end,
  gather_results(NewResults1, NewResults2);
gather_results(ResultsPart1, ResultsPart2) ->
  io:format("Password (Part1): "),
  print_results_part1(ResultsPart1),
  io:format("Password (Part2): "),
  print_results_part2(lists:sort(ResultsPart2)).

print_results_part1([First|Rest]) ->
  io:format("~c", [lists:nth(6, First)]),
  print_results_part1(Rest);
print_results_part1([]) ->
  io:format("~n").

print_results_part2([First|Rest]) ->
  io:format("~c", [lists:nth(7, First)]),
  print_results_part2(Rest);
print_results_part2([]) ->
  io:format("~n").
