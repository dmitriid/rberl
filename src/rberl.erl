-module(rberl).

-export([get/0, start/1]).

get() ->
	get("./../examples/lang.properties").

get(FileName) ->
	{ok, Binary} = file:read_file(FileName),
    Lines = string:tokens(erlang:binary_to_list(Binary), "\n"),
	process_lines(Lines).

process_lines(Lines) ->
	process_lines(Lines, [], "").

process_lines([], Acc, _) ->
	Acc;
process_lines([H|T], Acc, CurrentKey) ->
	%% find
	%%     a=b
	%% look out for
	%%     a=a\=b
	%% and
	%%     a=abcdef\
	%%       a\=b
	{ok, Re} = re:compile("(?<!\\\\)="),
	Match = re:run(H, Re),
	case Match of
		nomatch ->  %% we are in the middle of a value
			Acc1 = append_value(Acc, CurrentKey, H),
			process_lines(T, Acc1, CurrentKey);
		{match, [{Position, _}|_T]} -> %% we've come across a new value
			Key = string:left(H, Position),
			Value = string:substr(H, Position+2),
			Acc1 = append_value(Acc, Key, Value),
			process_lines(T, Acc1, Key)
	end.

append_value(Acc, K, Value) ->
	ProcessedValue = process_string(Value),
	Key = process_string(K),
	Val = proplists:get_value(Key, Acc),
	case Val of
		undefined ->
			[{Key, ProcessedValue}] ++ Acc;
		V ->
			[{Key, V ++ ProcessedValue}] ++ proplists:delete(Key, Acc)
	end.

process_string(Value) ->
	S = list_to_binary(string:strip(string:strip(Value, both, $\t))),
	S1 = convert(S, []),
	L = lists:last(S1),
	%% remove
	case L of
		$\\ ->
			string:left(S1, string:len(S1)-1);
		_ ->
			S1
	end.

convert(<<>>, Acc) ->
	binary_to_list(unicode:characters_to_binary(lists:reverse(Acc), unicode));
convert(<<$\t, Rest/binary>>, Acc) ->
	convert(Rest, "\t" ++ Acc);
convert(<<$\n, Rest/binary>>, Acc) ->
	convert(Rest, "\n" ++ Acc);
convert(<<$\\, $u, Unicode:4/binary, Rest/binary>>, Acc) ->
	{ok, Character, _} = io_lib:fread("~#", "16#" ++ binary_to_list(Unicode)),
	convert(Rest, Character ++ Acc);
convert(<<Char:1/binary, Rest/binary>>, Acc) ->
	Character = binary_to_list(Char),
	convert(Rest, Character ++ Acc).
